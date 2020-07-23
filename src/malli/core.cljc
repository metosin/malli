(ns malli.core
  (:refer-clojure :exclude [eval type -deref -lookup])
  (:require [malli.sci :as ms]
            [malli.registry :as mr])
  #?(:clj (:import (java.util.regex Pattern)
                   (clojure.lang IDeref))))

;;
;; protocols and records
;;

(defprotocol IntoSchema
  (-into-schema [this properties children options] "creates a new schema instance"))

(defprotocol Schema
  (-type [this] "returns type of the schema")
  (-validator [this] "returns a predicate function that checks if the schema is valid")
  (-explainer [this path] "returns a function of `x in acc -> maybe errors` to explain the errors for invalid values")
  (-transformer [this transformer method options] "returns an interceptor map with :enter and :leave functions to transform the value for the given schema and method")
  (-walk [this walker in options] "walks the schema and it's children")
  (-properties [this] "returns original schema properties")
  (-options [this] "returns original options")
  (-children [this] "returns schema children")
  (-form [this] "returns original form of the schema"))

(defprotocol MapSchema
  (-map-entries [this] "returns map entries"))

(defprotocol LensSchema
  (-get [this key default] "returns schema at key")
  (-set [this key value] "returns a copy with key having new value"))

(defprotocol RefSchema
  (-ref [this] "returns the reference name")
  (-deref [this] "returns the referenced schema"))

(defprotocol Walker
  (-accept [this schema in options])
  (-inner [this schema in options])
  (-outer [this schema children in options]))

(defprotocol Transformer
  (-transformer-chain [this] "returns transformer chain as a vector of maps with :name, :encoders, :decoders and :options")
  (-value-transformer [this schema method options] "returns an value transforming interceptor for the given schema and method"))

(defrecord SchemaError [path in schema value type message])

#?(:clj (defmethod print-method SchemaError [v ^java.io.Writer w]
          (.write w (str "#Error" (->> v (filter val) (into {}))))))

#?(:clj (defmethod print-method ::into-schema [v ^java.io.Writer w]
          (.write w (str "#IntoSchema{:class " v "}"))))

#?(:clj (defmethod print-method ::schema [v ^java.io.Writer w]
          (.write w (pr-str (-form v)))))

;;
;; impl
;;

(declare schema schema? into-schema into-schema? eval registry default-registry)

(defn keyword->string [x]
  (if (keyword? x)
    (if-let [nn (namespace x)]
      (str nn "/" (name x))
      (name x))
    x))

(defn error
  ([path in schema value]
   (->SchemaError path in schema value nil nil))
  ([path in schema value type]
   (->SchemaError path in schema value type nil)))

(defn error? [x]
  (instance? SchemaError x))

(defn fail!
  ([type]
   (fail! type nil))
  ([type data]
   (throw (ex-info (str type " " data) {:type type, :data data}))))

(defn -check-children! [type properties children {:keys [min max] :as opts}]
  (if (or (and min (< (count children) min)) (and max (> (count children) max)))
    (fail! ::child-error (merge {:type type, :properties properties, :children children} opts))))

(defn create-form [type properties children]
  (cond
    (and (seq properties) (seq children)) (into [type properties] children)
    (seq properties) [type properties]
    (seq children) (into [type] children)
    :else type))

(defn -distance [properties] (if (seq properties) 2 1))

(defn -guard [pred tf] (if tf (fn [x] (if (pred x) (tf x) x))))

(defn -chain [phase chain]
  (when-let [fns (->> (case phase, :enter (rseq chain), :leave chain) (keep identity) (seq))]
    (apply comp fns)))

(defn -parent-children-transformer [parent children transformer method options]
  (let [parent-transformer (-value-transformer transformer parent method options)
        child-transformers (map #(-transformer % transformer method options) children)
        build (fn [phase] (-chain phase (apply vector (phase parent-transformer) (map phase child-transformers))))]
    {:enter (build :enter)
     :leave (build :leave)}))

(defn -leaf-schema [type ->validator-and-children]
  ^{:type ::into-schema}
  (reify IntoSchema
    (-into-schema [_ properties children options]
      (let [[validator children] (->validator-and-children properties children options)
            form (create-form type properties children)]
        ^{:type ::schema}
        (reify
          Schema
          (-type [_] type)
          (-validator [_] validator)
          (-explainer [this path]
            (fn [value in acc]
              (if-not (validator value) (conj acc (error path in this value)) acc)))
          (-transformer [this transformer method options]
            (-value-transformer transformer this method options))
          (-walk [this walker in options]
            (if (-accept walker this in options)
              (-outer walker this children in options)))
          (-properties [_] properties)
          (-options [_] options)
          (-children [_] children)
          (-form [_] form))))))

(defn fn-schema [type f]
  (-leaf-schema
    type
    (fn [properties children _]
      (when (seq children)
        (fail! ::child-error {:type type, :properties properties, :children children, :min 0, :max 0}))
      [f children])))

(defn -partial-fn-schema [type f]
  (-leaf-schema
    type
    (fn [properties [child :as children] _]
      (-check-children! type properties children {:min 1, :max 1})
      [#(try (f % child) (catch #?(:clj Exception, :cljs js/Error) _ false)) children])))

(defn -and-schema []
  ^{:type ::into-schema}
  (reify IntoSchema
    (-into-schema [_ properties children options]
      (-check-children! :and properties children {:min 1})
      (let [children (mapv #(schema % options) children)
            form (create-form :and properties (map -form children))]
        ^{:type ::schema}
        (reify Schema
          (-type [_] :and)
          (-validator [_]
            (let [validators (distinct (map -validator children))
                  f (if (seq (rest validators)) (partial apply every-pred) first)]
              (f validators)))
          (-explainer [_ path]
            (let [distance (-distance properties)
                  explainers (mapv (fn [[i c]] (-explainer c (into path [(+ i distance)]))) (map-indexed vector children))]
              (fn explain [x in acc]
                (reduce
                  (fn [acc' explainer]
                    (let [acc'' (explainer x in acc')]
                      (cond
                        (nil? acc'') acc'
                        :else acc'')))
                  acc explainers))))
          (-transformer [this transformer method options]
            (-parent-children-transformer this children transformer method options))
          (-walk [this walker in options]
            (if (-accept walker this in options)
              (-outer walker this (mapv #(-inner walker % in options) children) in options)))
          (-properties [_] properties)
          (-options [_] options)
          (-children [_] children)
          (-form [_] form)
          LensSchema
          (-get [_ key default] (get children key default))
          (-set [_ key value] (into-schema :and properties (assoc children key value))))))))

(defn -or-schema []
  ^{:type ::into-schema}
  (reify IntoSchema
    (-into-schema [_ properties children options]
      (-check-children! :or properties children {:min 1})
      (let [children (mapv #(schema % options) children)
            form (create-form :or properties (map -form children))]
        ^{:type ::schema}
        (reify Schema
          (-type [_] :or)
          (-validator [_]
            (let [validators (distinct (map -validator children))
                  f (if (seq (rest validators)) (partial apply some-fn) first)]
              (f validators)))
          (-explainer [_ path]
            (let [distance (-distance properties)
                  explainers (mapv (fn [[i c]] (-explainer c (into path [(+ i distance)]))) (map-indexed vector children))]
              (fn explain [x in acc]
                (reduce
                  (fn [acc' explainer]
                    (let [acc'' (explainer x in acc')]
                      (cond
                        (identical? acc' acc'') (reduced acc)
                        (nil? acc'') acc'
                        :else acc'')))
                  acc explainers))))
          (-transformer [this transformer method options]
            (let [this-transformer (-value-transformer transformer this method options)
                  child-transformers (map #(-transformer % transformer method options) children)
                  decode? (= :decode method)
                  build (fn [phase]
                          (let [->this (phase this-transformer)
                                ?->this (or ->this identity)
                                ->children (mapv #(or (phase %) identity) child-transformers)
                                validators (mapv -validator children)]
                            (cond
                              (not (seq ->children)) ->this

                              ;; decode, on the way in, we transforma all values into vector + the original
                              (and decode? (= :enter phase)) (let [->children (conj ->children identity)]
                                                               (fn [x] (let [x (?->this x)] (mapv #(% x) ->children))))

                              ;; decode, on the way out, we take the first transformed value that is valid
                              decode? (fn [xs]
                                        (?->this
                                          (reduce-kv
                                            (fn [acc i x]
                                              (let [x' ((nth ->children i) x)]
                                                (if ((nth validators i) x') (reduced x') acc)))
                                            (peek xs) (pop xs))))

                              ;; encode, on the way in, we take the first valid valud and it's index
                              (= :enter phase) (fn [x]
                                                 (let [x (?->this x)]
                                                   (reduce-kv
                                                     (fn [acc i v]
                                                       (if (v x)
                                                         (reduced [((nth ->children i) x) i]) acc))
                                                     [x] validators)))

                              ;; encode, on the way out, we transform the value using the index
                              :else (fn [[x i]]
                                      (?->this (if i ((nth ->children i) x) x))))))]

              {:enter (build :enter)
               :leave (build :leave)}))
          (-walk [this walker in options]
            (if (-accept walker this in options)
              (-outer walker this (mapv #(-inner walker % in options) children) in options)))
          (-properties [_] properties)
          (-options [_] options)
          (-children [_] children)
          (-form [_] form)
          LensSchema
          (-get [_ key default] (get children key default))
          (-set [_ key value] (into-schema :or properties (assoc children key value))))))))

(defn- -properties-and-children [[x :as xs]]
  (if ((some-fn map? nil?) x)
    [x (rest xs)]
    [nil xs]))

(defn -parse-entry-syntax [ast options]
  (let [-parse (fn [[k ?p ?v :as e] f expand]
                 (let [[p ?s] (if (or (nil? ?p) (map? ?p)) [?p ?v] [nil ?p]), s (f k p ?s)]
                   (if expand [k p s] (->> (assoc (vec e) (dec (count e)) s) (keep identity) (vec)))))
        children (->> ast (keep identity) (mapv #(-parse % (fn [_ _ s] (schema s options)) false)))
        entries (->> children (mapv #(-parse % (fn [_ _ s] s) true)))
        forms (->> children (mapv #(-parse % (fn [_ _ s] (-form s)) false)))
        keys (->> children (map first))]
    (when-not (= keys (distinct keys))
      (fail! ::non-distinct-entry-keys {:keys keys}))
    {:children children
     :entries entries
     :forms forms}))

(defn ^:no-doc required-map-entry? [[_ ?p]]
  (not (and (map? ?p) (true? (:optional ?p)))))

(defn -map-schema []
  ^{:type ::into-schema}
  (reify IntoSchema
    (-into-schema [_ {:keys [closed] :as properties} children options]
      (let [{:keys [children entries forms]} (-parse-entry-syntax children options)
            form (create-form :map properties forms)
            keyset (->> entries (map first) (set))]
        ^{:type ::schema}
        (reify Schema
          (-type [_] :map)
          (-validator [_]
            (let [validators (cond-> (mapv
                                       (fn [[key {:keys [optional]} value]]
                                         (let [valid? (-validator value)
                                               default (boolean optional)]
                                           (fn [m] (if-let [map-entry (find m key)] (valid? (val map-entry)) default))))
                                       entries)
                                     closed (into [(fn [m]
                                                     (reduce
                                                       (fn [acc k] (if (contains? keyset k) acc (reduced false)))
                                                       true (keys m)))]))
                  validate (fn [m]
                             (boolean
                               #?(:clj  (let [it (.iterator ^Iterable validators)]
                                          (boolean
                                            (loop []
                                              (if (.hasNext it)
                                                (and ((.next it) m) (recur))
                                                true))))
                                  :cljs (reduce #(or (%2 m) (reduced false)) true validators))))]
              (fn [m] (and (map? m) (validate m)))))
          (-explainer [this path]
            (let [distance (-distance properties)
                  explainers (cond-> (mapv
                                       (fn [[i [key {:keys [optional] :as key-properties} schema]]]
                                         (let [key-distance (-distance key-properties)
                                               explainer (-explainer schema (into path [(+ i distance) key-distance]))
                                               key-path (into path [(+ i distance) 0])]
                                           (fn [x in acc]
                                             (if-let [e (find x key)]
                                               (explainer (val e) (conj in key) acc)
                                               (if-not optional
                                                 (conj acc (error key-path (conj in key) this nil ::missing-key))
                                                 acc)))))
                                       (map-indexed vector entries))
                                     closed (into [(fn [x in acc]
                                                     (reduce
                                                       (fn [acc k]
                                                         (if (contains? keyset k)
                                                           acc
                                                           (conj acc (error path (conj in k) this nil ::extra-key))))
                                                       acc (keys x)))]))]
              (fn [x in acc]
                (if-not (map? x)
                  (conj acc (error path in this x ::invalid-type))
                  (reduce
                    (fn [acc explainer]
                      (explainer x in acc))
                    acc explainers)))))
          (-transformer [this transformer method options]
            (let [this-transformer (-value-transformer transformer this method options)
                  transformers (some->>
                                 entries
                                 (keep (fn [[k _ s]] (if-let [t (-transformer s transformer method options)] [k t])))
                                 (into {}))
                  build (fn [phase]
                          (let [->this (phase this-transformer)
                                ->children (->> transformers
                                                (keep (fn extract-value-transformer-phase [[k t]]
                                                        (if-let [phase-t (phase t)]
                                                          [k phase-t])))
                                                (into {}))
                                apply->children (if (seq ->children)
                                                  #(reduce-kv
                                                     (fn reduce-child-transformers [m k t]
                                                       (if-let [entry (find m k)]
                                                         (assoc m k (t (val entry)))
                                                         m))
                                                     % ->children))]
                            (-chain phase [->this (-guard map? apply->children)])))]
              {:enter (build :enter)
               :leave (build :leave)}))
          (-walk [this walker in options]
            (if (-accept walker this in options)
              (-outer walker this (mapv (fn [[k p s]] [k p (-inner walker s (conj in k) options)]) entries) in options)))
          (-properties [_] properties)
          (-options [_] options)
          (-children [_] children)
          (-form [_] form)
          MapSchema
          (-map-entries [_] entries)
          LensSchema
          (-get [_ key default] (or (some (fn [[k _ s]] (if (= k key) s)) entries) default))
          (-set [_ key value]
            (let [found (atom nil)
                  [key kprop] (if (vector? key) key [key])
                  entries (cond-> (mapv (fn [[k p s]] (if (= key k) (do (reset! found true) [k kprop value]) [k p s])) entries)
                                  (not @found) (conj [key kprop value])
                                  :always (->> (filter (fn [e] (-> e last some?)))))]
              (into-schema :map properties entries))))))))

(defn -map-of-schema []
  ^{:type ::into-schema}
  (reify IntoSchema
    (-into-schema [_ properties children options]
      (-check-children! :map-of properties children {:min 2 :max 2})
      (let [[key-schema value-schema :as children] (mapv #(schema % options) children)
            form (create-form :map-of properties (mapv -form children))]
        ^{:type ::schema}
        (reify Schema
          (-type [_] :map-of)
          (-validator [_]
            (let [key-valid? (-validator key-schema)
                  value-valid? (-validator value-schema)]
              (fn [m]
                (and (map? m)
                     (reduce-kv
                       (fn [_ key value]
                         (or (and (key-valid? key) (value-valid? value)) (reduced false)))
                       true m)))))
          (-explainer [this path]
            (let [distance (-distance properties)
                  key-explainer (-explainer key-schema (conj path distance))
                  value-explainer (-explainer value-schema (conj path (inc distance)))]
              (fn explain [m in acc]
                (if-not (map? m)
                  (conj acc (error path in this m ::invalid-type))
                  (reduce-kv
                    (fn [acc key value]
                      (let [in (conj in key)]
                        (->> acc
                             (key-explainer key in)
                             (value-explainer value in))))
                    acc m)))))
          (-transformer [this transformer method options]
            (let [this-transformer (-value-transformer transformer this method options)
                  key-transformer (-transformer key-schema transformer method options)
                  child-transformer (-transformer value-schema transformer method options)
                  build (fn [phase]
                          (let [->this (phase this-transformer)
                                ->key (if-let [t (phase key-transformer)]
                                        (fn [x] (t x)))
                                ->child (phase child-transformer)
                                ->key-child (cond
                                              (and ->key ->child) #(assoc %1 (->key %2) (->child %3))
                                              ->key #(assoc %1 (->key %2) %3)
                                              ->child #(assoc %1 %2 (->child %3)))
                                apply->key-child (if ->key-child #(reduce-kv ->key-child (empty %) %))]
                            (-chain phase [->this (-guard map? apply->key-child)])))]
              {:enter (build :enter)
               :leave (build :leave)}))
          (-walk [this walker in options]
            (if (-accept walker this in options)
              (-outer walker this (mapv #(-inner walker % in options) children) in options)))
          (-properties [_] properties)
          (-options [_] options)
          (-children [_] children)
          (-form [_] form))))))

(defn -collection-schema [type fpred fempty]
  ^{:type ::into-schema}
  (reify IntoSchema
    (-into-schema [_ {:keys [min max] :as properties} children options]
      (-check-children! type properties children {:min 1 :max 1})
      (let [schema (schema (first children) options)
            form (create-form type properties [(-form schema)])
            validate-limits (cond
                              (not (or min max)) (constantly true)
                              (and min max) (fn [x] (let [size (count x)] (<= min size max)))
                              min (fn [x] (let [size (count x)] (<= min size)))
                              max (fn [x] (let [size (count x)] (<= size max))))]
        ^{:type ::schema}
        (reify Schema
          (-type [_] type)
          (-validator [_]
            (let [validator (-validator schema)]
              (fn [x] (and (fpred x)
                           (validate-limits x)
                           (reduce (fn [acc v] (if (validator v) acc (reduced false))) true x)))))
          (-explainer [this path]
            (let [distance (-distance properties)
                  explainer (-explainer schema (conj path distance))]
              (fn [x in acc]
                (cond
                  (not (fpred x)) (conj acc (error path in this x ::invalid-type))
                  (not (validate-limits x)) (conj acc (error path in this x ::limits))
                  :else (let [size (count x)]
                          (loop [acc acc, i 0, [x & xs] x]
                            (if (< i size)
                              (cond-> (or (explainer x (conj in i) acc) acc) xs (recur (inc i) xs))
                              acc)))))))
          (-transformer [this transformer method options]
            (let [collection? #(or (sequential? %) (set? %))
                  this-transformer (-value-transformer transformer this method options)
                  child-transformer (-transformer schema transformer method options)
                  build (fn [phase]
                          (let [->this (phase this-transformer)
                                ->child (if-let [ct (phase child-transformer)]
                                          (if fempty
                                            #(into (if % fempty) (map ct) %)
                                            #(map ct %)))]
                            (-chain phase [->this (-guard collection? ->child)])))]
              {:enter (build :enter)
               :leave (build :leave)}))
          (-walk [this walker in options]
            (if (-accept walker this in options)
              (-outer walker this [(-inner walker schema (conj in ::in) options)] in options)))
          (-properties [_] properties)
          (-options [_] options)
          (-children [_] [schema])
          (-form [_] form)
          LensSchema
          (-get [_ key default] (if (= 0 key) schema default))
          (-set [_ key value] (if (= 0 key) (into-schema type properties [value]) schema)))))))

(defn -tuple-schema []
  ^{:type ::into-schema}
  (reify IntoSchema
    (-into-schema [_ properties children options]
      (let [children (mapv #(schema % options) children)
            size (count children)
            form (create-form :tuple properties (map -form children))]
        (-check-children! :tuple properties children {:min 1})
        ^{:type ::schema}
        (reify Schema
          (-type [_] :tuple)
          (-validator [_]
            (let [validators (into (array-map) (map-indexed vector (mapv -validator children)))]
              (fn [x] (and (vector? x)
                           (= (count x) size)
                           (reduce-kv
                             (fn [acc i validator]
                               (if (validator (nth x i)) acc (reduced false))) true validators)))))
          (-explainer [this path]
            (let [distance (-distance properties)
                  explainers (mapv (fn [[i s]]
                                     (-explainer s (conj path (+ i distance))))
                                   (map-indexed vector children))]
              (fn [x in acc]
                (cond
                  (not (vector? x)) (conj acc (error path in this x ::invalid-type))
                  (not= (count x) size) (conj acc (error path in this x ::tuple-size))
                  :else (loop [acc acc, i 0, [x & xs] x, [e & es] explainers]
                          (cond-> (e x (conj in i) acc) xs (recur (inc i) xs es)))))))
          (-transformer [this transformer method options]
            (let [this-transformer (-value-transformer transformer this method options)
                  child-transformers (->> children
                                          (mapv #(-transformer % transformer method options))
                                          (map-indexed vector)
                                          (into {}))
                  build (fn [phase]
                          (let [->this (phase this-transformer)
                                ->children (->> child-transformers
                                                (keep (fn [[k t]] (if-let [t (phase t)] [k t])))
                                                (into {}))
                                apply->children #(reduce-kv update % ->children)]
                            (-chain phase [->this (-guard vector? apply->children)])))]
              {:enter (build :enter)
               :leave (build :leave)}))
          (-walk [this walker in options]
            (if (-accept walker this in options)
              (-outer walker this (mapv (fn [[i s]] (-inner walker s (conj in i) options)) (map-indexed vector children)) in options)))
          (-properties [_] properties)
          (-options [_] options)
          (-children [_] children)
          (-form [_] form)
          LensSchema
          (-get [_ key default] (get children key default))
          (-set [_ key value] (into-schema :tuple properties (assoc children key value))))))))

(defn -enum-schema []
  ^{:type ::into-schema}
  (reify IntoSchema
    (-into-schema [_ properties children options]
      (-check-children! :enum properties children {:min 1})
      (let [schema (set children)
            form (create-form :enum properties children)]
        ^{:type ::schema}
        (reify Schema
          (-type [_] :enum)
          (-validator [_]
            (fn [x] (contains? schema x)))
          (-explainer [this path]
            (fn explain [x in acc]
              (if-not (contains? schema x) (conj acc (error (conj path 0) in this x)) acc)))
          ;; TODO: should we try to derive the type from values? e.g. [:enum 1 2] ~> int?
          (-transformer [this transformer method options]
            (-value-transformer transformer this method options))
          (-walk [this walker in options]
            (if (-accept walker this in options)
              (-outer walker this (vec children) in options)))
          (-properties [_] properties)
          (-options [_] options)
          (-children [_] children)
          (-form [_] form))))))

(defn -re-schema [class?]
  ^{:type ::into-schema}
  (reify IntoSchema
    (-into-schema [_ properties [child :as children] options]
      (-check-children! :re properties children {:min 1, :max 1})
      (let [re (re-pattern child)
            form (if class? re (create-form :re properties children))]
        ^{:type ::schema}
        (reify Schema
          (-type [_] :re)
          (-validator [_]
            (fn [x] (try (boolean (re-find re x)) (catch #?(:clj Exception, :cljs js/Error) _ false))))
          (-explainer [this path]
            (fn explain [x in acc]
              (try
                (if-not (re-find re x)
                  (conj acc (error path in this x))
                  acc)
                (catch #?(:clj Exception, :cljs js/Error) e
                  (conj acc (error path in this x (:type (ex-data e))))))))
          (-transformer [this transformer method options]
            (-value-transformer transformer this method options))
          (-walk [this walker in options]
            (if (-accept walker this in options)
              (-outer walker this (vec children) in options)))
          (-properties [_] properties)
          (-options [_] options)
          (-children [_] children)
          (-form [_] form))))))

(defn -fn-schema []
  ^{:type ::into-schema}
  (reify IntoSchema
    (-into-schema [_ properties children options]
      (-check-children! :fn properties children {:min 1, :max 1})
      (let [f (eval (first children))
            form (create-form :fn properties children)]
        ^{:type ::schema}
        (reify Schema
          (-type [_] :fn)
          (-validator [_]
            (fn [x] (try (f x) (catch #?(:clj Exception, :cljs js/Error) _ false))))
          (-explainer [this path]
            (fn explain [x in acc]
              (try
                (if-not (f x)
                  (conj acc (error path in this x))
                  acc)
                (catch #?(:clj Exception, :cljs js/Error) e
                  (conj acc (error path in this x (:type (ex-data e))))))))
          (-transformer [this transformer method options]
            (-value-transformer transformer this method options))
          (-walk [this walker in options]
            (if (-accept walker this in options)
              (-outer walker this (vec children) in options)))
          (-properties [_] properties)
          (-options [_] options)
          (-children [_] children)
          (-form [_] form))))))

(defn -maybe-schema []
  ^{:type ::into-schema}
  (reify IntoSchema
    (-into-schema [_ properties children options]
      (-check-children! :maybe properties children {:min 1, :max 1})
      (let [[schema :as children] (map #(schema % options) children)
            form (create-form :maybe properties (map -form children))]
        ^{:type ::schema}
        (reify Schema
          (-type [_] :maybe)
          (-validator [_]
            (let [validator' (-validator schema)]
              (fn [x] (or (nil? x) (validator' x)))))
          (-explainer [_ path]
            (let [explainer' (-explainer schema (conj path (-distance properties)))]
              (fn explain [x in acc]
                (if (nil? x) acc (explainer' x in acc)))))
          (-transformer [this transformer method options]
            (-parent-children-transformer this children transformer method options))
          (-walk [this walker in options]
            (if (-accept walker this in options)
              (-outer walker this [(-inner walker schema in options)] in options)))
          (-properties [_] properties)
          (-options [_] options)
          (-children [_] children)
          (-form [_] form)
          LensSchema
          (-get [_ key default] (if (= 0 key) schema default))
          (-set [_ key value] (if (= 0 key) (into-schema :maybe properties [value]) schema)))))))

(defn- -multi-schema []
  ^{:type ::into-schema}
  (reify IntoSchema
    (-into-schema [_ properties children options]
      (let [{:keys [children entries forms]} (-parse-entry-syntax children options)
            form (create-form :multi properties forms)
            dispatch (eval (:dispatch properties))
            dispatch-map (->> (for [[d _ s] entries] [d s]) (into {}))]
        (when-not dispatch
          (fail! ::missing-property {:key :dispatch}))
        ^{:type ::schema}
        (reify Schema
          (-type [_] :multi)
          (-validator [_]
            (let [validators (reduce-kv (fn [acc k s] (assoc acc k (-validator s))) {} dispatch-map)]
              (fn [x]
                (if-let [validator (validators (dispatch x))]
                  (validator x)
                  false))))
          ;; is path ok?
          (-explainer [this path]
            (let [explainers (reduce
                               (fn [acc [i [key key-properties schema]]]
                                 (let [key-distance (-distance key-properties)
                                       explainer (-explainer schema (into path [(+ i (-distance properties)) key-distance]))]
                                   (assoc acc key (fn [x in acc] (explainer x in acc)))))
                               {} (map-indexed vector entries))]
              (fn [x in acc]
                (if-let [explainer (explainers (dispatch x))]
                  (explainer x in acc)
                  (conj acc (error path in this x ::invalid-dispatch-value))))))
          (-transformer [this transformer method options]
            (let [this-transformer (-value-transformer transformer this method options)
                  child-transformers (reduce-kv
                                       #(assoc %1 %2 (-transformer %3 transformer method options))
                                       {} dispatch-map)
                  build (fn [phase]
                          (let [->this (phase this-transformer)
                                ->children (->> child-transformers
                                                (keep (fn [[k v]] (if-let [t (phase v)] [k t])))
                                                (into {}))
                                ->child (if (seq ->children) (fn [x] (if-let [t (->children (dispatch x))] (t x) x)))]
                            (-chain phase [->this ->child])))]
              {:enter (build :enter)
               :leave (build :leave)}))
          (-walk [this walker in options]
            (if (-accept walker this in options)
              (-outer walker this (mapv (fn [[k p s]] [k p (-inner walker s in options)]) entries) in options)))
          (-properties [_] properties)
          (-options [_] options)
          (-children [_] children)
          (-form [_] form)
          MapSchema
          (-map-entries [_] entries))))))

(defn -string-schema []
  ^{:type ::into-schema}
  (reify IntoSchema
    (-into-schema [_ {:keys [min max] :as properties} children options]
      (-check-children! :string properties children {:min 0, :max 0})
      (let [count-validator (cond
                              (not (or min max)) nil
                              (and min max) (fn [x] (let [size (count x)] (<= min size max)))
                              min (fn [x] (<= min (count x)))
                              max (fn [x] (<= (count x) max)))
            validator (if count-validator (fn [x] (and (string? x) (count-validator x))) string?)
            form (create-form :string properties children)]
        ^{:type ::schema}
        (reify Schema
          (-type [_] :string)
          (-validator [_] validator)
          (-explainer [this path]
            (fn explain [x in acc]
              (if (or (not (string? x)) (and count-validator (not (count-validator x))))
                (conj acc (error path in this x)) acc)))
          (-transformer [this transformer method options]
            (-value-transformer transformer this method options))
          (-walk [this walker in options]
            (if (-accept walker this in options)
              (-outer walker this (vec children) in options)))
          (-properties [_] properties)
          (-options [_] options)
          (-children [_] children)
          (-form [_] form))))))

(defn- -ref-schema []
  ^{:type ::into-schema}
  (reify IntoSchema
    (-into-schema [_ properties [ref :as children] {::keys [allow-invalid-refs] :as options}]
      (-check-children! :ref properties children {:min 1, :max 1})
      (let [-memoize (fn [f] (let [value (atom nil)] (fn [] (or @value) (reset! value (f)))))
            -ref (or (if-let [s (mr/-schema (registry options) ref)] (-memoize (fn [] (schema s options))))
                     (when-not allow-invalid-refs
                       (fail! ::invalid-ref {:type :ref, :ref ref})))
            form (create-form :ref properties children)]
        ^{:type ::schema}
        (reify Schema
          (-type [_] :ref)
          (-validator [_]
            (let [validator (-memoize (fn [] (-validator (-ref))))]
              (fn [x] ((validator) x))))
          (-explainer [_ path]
            (let [explainer (-memoize (fn [] (-explainer (-ref) (conj path 0))))]
              (fn [x in acc] ((explainer) x in acc))))
          (-transformer [this transformer method options]
            (let [this-transformer (-value-transformer transformer this method options)
                  enter (-memoize (fn [] (:enter (-transformer (-ref) transformer method options))))
                  leave (-memoize (fn [] (:leave (-transformer (-ref) transformer method options))))]
              {:enter (-chain :enter [(:enter this-transformer) (fn [x] ((enter) x))])
               :leave (-chain :leave [(:leave this-transformer) (fn [x] ((leave) x))])}))
          (-walk [this walker in options]
            (let [accept (fn [] (-inner walker (-ref) in (update options ::ref-walked (fnil conj #{}) ref)))
                  accept-ref ^{:type ::ref} (reify IDeref (#?(:cljs cljs.core/-deref, :clj deref) [_] (accept)))
                  options (assoc options ::ref-walk accept-ref)]
              (if (-accept walker this in options)
                (-outer walker this (vec children) in options))))
          (-properties [_] properties)
          (-options [_] options)
          (-children [_] children)
          (-form [_] form)
          LensSchema
          (-get [_ key default] (if (= key 0) (-ref) default))
          (-set [_ key value] (if (= key 0) (into-schema :ref properties [value])))
          RefSchema
          (-ref [_] ref)
          (-deref [_] (-ref)))))))

(defn -schema-schema [{:keys [id raw]}]
  ^{:type ::into-schema}
  (let [type (if (or id raw) ::schema :schema)]
    (reify IntoSchema
      (-into-schema [_ properties children options]
        (-check-children! type properties children {:min 1, :max 1})
        (let [[child :as children] (map #(schema % options) children)
              form (or (and (empty? properties) (or id (and raw (-form child))))
                       (create-form type properties [(-form child)]))]
          ^{:type ::schema}
          (reify Schema
            (-type [_] type)
            (-validator [_] (-validator child))
            (-explainer [_ path] (-explainer child path))
            (-transformer [this transformer method options]
              (-parent-children-transformer this children transformer method options))
            (-walk [this walker in options]
              (if (-accept walker this in options)
                (if id
                  (-outer walker this [id] in options)
                  (-outer walker this [(-inner walker child in options)] in options))))
            (-properties [_] properties)
            (-options [_] options)
            (-children [_] children)
            (-form [_] form)
            ;; TODO: are this correct
            LensSchema
            (-get [_ key default] (if id (-get child key default) (if (= key 0) (-get child key default))))
            (-set [_ key value] (if (= key 0) (-set child key value)))
            RefSchema
            (-ref [_] id)
            (-deref [_] child)))))))

(defn- -register-var [registry v]
  (let [name (-> v meta :name)
        schema (fn-schema name @v)]
    (-> registry
        (assoc name schema)
        (assoc @v schema))))

(defn registry
  ([] default-registry)
  ([{:keys [registry]}] (or (mr/registry registry) default-registry)))

(defn- -lookup [?schema options]
  (let [registry (registry options)]
    (or (mr/-schema registry ?schema)
        (some-> registry (mr/-schema (clojure.core/type ?schema)) (-into-schema nil [?schema] options)))))

(defn- -schema [?schema options]
  (or (and (or (schema? ?schema) (into-schema? ?schema)) ?schema)
      (-lookup ?schema options)
      (fail! ::invalid-schema {:schema ?schema})))

(defn ^:no-doc into-transformer [x]
  (cond
    (satisfies? Transformer x) x
    (fn? x) (into-transformer (x))
    :else (fail! ::invalid-transformer {:value x})))

(defn- -property-registry [m options f]
  (let [options (assoc options ::allow-invalid-refs true)]
    (reduce-kv (fn [acc k v] (assoc acc k (f (schema v options)))) {} m)))

(defn -properties-and-options [properties options f]
  (if-let [r (some-> properties :registry)]
    (let [options (update options :registry #(mr/composite-registry r (or % (registry options))))]
      [(assoc properties :registry (-property-registry r options f)) options])
    [properties options]))

(defn -ref-key? [?schema] (or (string? ?schema) (qualified-keyword? ?schema)))

;;
;; public api
;;


(defn schema? [x]
  (satisfies? Schema x))

(defn into-schema? [x]
  (satisfies? IntoSchema x))

(defn into-schema
  ([type properties children]
   (into-schema type properties children nil))
  ([type properties children options]
   (let [[properties options] (-properties-and-options properties options -form)]
     (-into-schema (-schema type options) (if (seq properties) properties) children options))))

(defn schema
  ([?schema]
   (schema ?schema nil))
  ([?schema options]
   (cond
     (schema? ?schema) ?schema
     (into-schema? ?schema) (-into-schema ?schema nil nil options)
     (vector? ?schema) (let [[p c] (-properties-and-children (rest ?schema))]
                         (into-schema (-schema (first ?schema) options) p c options))
     :else (if-let [?schema' (and (-ref-key? ?schema) (-lookup ?schema options))]
             (-into-schema (-schema-schema {:id ?schema}) nil [(schema ?schema' options)] options)
             (-> ?schema (-schema options) (schema options))))))

(defn form
  ([?schema]
   (form ?schema nil))
  ([?schema options]
   (-form (schema ?schema options))))

(defn properties
  ([?schema]
   (properties ?schema nil))
  ([?schema options]
   (-properties (schema ?schema options))))

(defn options
  ([?schema]
   (options ?schema nil))
  ([?schema options]
   (-options (schema ?schema options))))

(defn children
  ([?schema]
   (children ?schema nil))
  ([?schema options]
   (let [schema (schema ?schema options)]
     (-children schema))))

(defn type
  ([?schema]
   (type ?schema nil))
  ([?schema options]
   (-type (schema ?schema options))))

(defn walk
  ([?schema f]
   (walk ?schema f nil))
  ([?schema f options]
   (-walk
     (schema ?schema options)
     (reify Walker
       (-accept [_ s _ _] s)
       (-inner [this s in options] (-walk s this in options))
       (-outer [_ s c in options] (f s c in options)))
     [] options)))

(defn validator
  ([?schema]
   (validator ?schema nil))
  ([?schema options]
   (-validator (schema ?schema options))))

(defn validate
  ([?schema value]
   (validate ?schema value nil))
  ([?schema value options]
   ((validator ?schema options) value)))

(defn explainer
  ([?schema]
   (explainer ?schema nil))
  ([?schema options]
   (let [schema' (schema ?schema options)
         explainer' (-explainer schema' [])]
     (fn explainer
       ([value]
        (explainer value [] []))
       ([value in acc]
        (if-let [errors (seq (explainer' value in acc))]
          {:schema schema'
           :value value
           :errors errors}))))))

(defn explain
  ([?schema value]
   (explain ?schema value nil))
  ([?schema value options]
   ((explainer ?schema options) value [] [])))

(defn decoder
  "Creates a value decoding transformer given a transformer and a schema."
  ([?schema t]
   (decoder ?schema nil t))
  ([?schema options t]
   (let [{:keys [enter leave]} (-transformer (schema ?schema options) (into-transformer t) :decode options)]
     (cond
       (and enter leave) (comp leave enter)
       (or enter leave) (or enter leave)
       :else identity))))

(defn decode
  "Transforms a value with a given decoding transformer against a schema."
  ([?schema value t]
   (decode ?schema value nil t))
  ([?schema value options t]
   (if-let [transform (decoder ?schema options t)]
     (transform value)
     value)))

(defn encoder
  "Creates a value encoding transformer given a transformer and a schema."
  ([?schema t]
   (encoder ?schema nil t))
  ([?schema options t]
   (let [{:keys [enter leave]} (-transformer (schema ?schema options) (into-transformer t) :encode options)]
     (cond
       (and enter leave) (comp leave enter)
       (or enter leave) (or enter leave)
       :else identity))))

(defn encode
  "Transforms a value with a given encoding transformer against a schema."
  ([?schema value t]
   (encode ?schema value nil t))
  ([?schema value options t]
   (if-let [transform (encoder ?schema options t)]
     (transform value)
     value)))

(defn map-entries
  "Returns a sequence of 3-element map-entry tuples of type `key ?properties schema`"
  ([?schema]
   (map-entries ?schema nil))
  ([?schema options]
   (if-let [schema (schema ?schema options)]
     (if (satisfies? MapSchema schema)
       (-map-entries schema)))))

;;
;; eval
;;

(let [-eval (or (ms/evaluator {:preset :termination-safe
                               :bindings {'m/properties properties
                                          'm/type type
                                          'm/children children
                                          'm/map-entries map-entries}})
                #(fail! :sci-not-available {:code %}))]
  (defn eval [?code] (if (fn? ?code) ?code (-eval (str ?code)))))

;;
;; Walkers
;;

(defn schema-walker [f]
  (fn [schema children _ _]
    (f (into-schema (-type schema) (-properties schema) children (-options schema)))))

(defn ^:no-doc map-syntax-walker [schema children _ _]
  (let [properties (properties schema)]
    (cond-> {:type (type schema)}
            (seq properties) (assoc :properties properties)
            (seq children) (assoc :children children))))

;;
;; map-syntax
;;

(defn ^:no-doc to-map-syntax
  ([?schema] (to-map-syntax ?schema nil))
  ([?schema options] (walk ?schema map-syntax-walker options)))

(defn ^:no-doc from-map-syntax
  ([m] (from-map-syntax m nil))
  ([{:keys [type properties children] :as m} options]
   (if (map? m)
     (let [<-child (if (-> children first vector?) (fn [f] #(update % 2 f)) identity)
           [properties options] (-properties-and-options properties options -form)]
       (into-schema type properties (mapv (<-child #(from-map-syntax % options)) children) options))
     m)))

;;
;; registry
;;

(defn predicate-schemas []
  (->> [#'any? #'some? #'number? #'integer? #'int? #'pos-int? #'neg-int? #'nat-int? #'float? #'double?
        #'boolean? #'string? #'ident? #'simple-ident? #'qualified-ident? #'keyword? #'simple-keyword?
        #'qualified-keyword? #'symbol? #'simple-symbol? #'qualified-symbol? #'uuid? #'uri? #?(:clj #'decimal?)
        #'inst? #'seqable? #'indexed? #'map? #'vector? #'list? #'seq? #'char? #'set? #'nil? #'false? #'true?
        #'zero? #?(:clj #'rational?) #'coll? #'empty? #'associative? #'sequential? #?(:clj #'ratio?) #?(:clj #'bytes?)]
       (reduce -register-var {})))

(defn class-schemas []
  {#?(:clj Pattern, :cljs js/RegExp) (-re-schema true)})

(defn comparator-schemas []
  (->> {:> >, :>= >=, :< <, :<= <=, := =, :not= not=}
       (map (fn [[k v]] [k (-partial-fn-schema k v)]))
       (into {}) (reduce-kv assoc nil)))

(defn base-schemas []
  {:and (-and-schema)
   :or (-or-schema)
   :map (-map-schema)
   :map-of (-map-of-schema)
   :vector (-collection-schema :vector vector? [])
   :list (-collection-schema :list list? nil)
   :sequential (-collection-schema :sequential sequential? nil)
   :set (-collection-schema :set set? #{})
   :enum (-enum-schema)
   :maybe (-maybe-schema)
   :tuple (-tuple-schema)
   :multi (-multi-schema)
   :re (-re-schema false)
   :fn (-fn-schema)
   :string (-string-schema)
   :ref (-ref-schema)
   :schema (-schema-schema nil)
   ::schema (-schema-schema {:raw true})})

(defn default-schemas []
  (merge (predicate-schemas) (class-schemas) (comparator-schemas) (base-schemas)))

(def default-registry
  (mr/registry (cond (identical? mr/type "default") (default-schemas)
                     (identical? mr/type "custom") (mr/custom-default-registry)
                     :else (fail! ::invalid-registry.type {:type mr/type}))))
