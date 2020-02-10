(ns malli.core
  (:refer-clojure :exclude [-name eval name])
  (:require [sci.core :as sci])
  #?(:clj (:import (java.util.regex Pattern))))

;;
;; protocols and records
;;

(defprotocol IntoSchema
  (-into-schema [this properties children options] "creates a new schema instance"))

(defprotocol Schema
  (-name [this] "returns name of the schema")
  (-validator [this] "returns a predicate function that checks if the schema is valid")
  (-explainer [this path] "returns a function of `x in acc -> maybe errors` to explain the errors for invalid values")
  (-transformer [this transformer method options] "returns an interceptor map with :enter and :leave functions to transform the value for the given schema and method")
  (-accept [this visitor options] "accepts the visitor to visit schema and it's children")
  (-properties [this] "returns original schema properties")
  (-options [this] "returns original options")
  (-form [this] "returns original form of the schema"))

(defprotocol MapSchema
  (-map-entries [this] "returns map entries"))

(defprotocol LookupSchema
  (-get [this key default] "returns schema at key"))

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

(declare schema)
(declare eval)
(declare default-registry)

(defn keyword->string [x]
  (if (keyword? x)
    (if-let [nn (namespace x)]
      (str nn "/" (clojure.core/name x))
      (clojure.core/name x))
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
   (throw (ex-info (str type) {:type type, :data data}))))

(defn create-form [name properties children]
  (cond
    (and (seq properties) (seq children)) (into [name properties] children)
    (seq properties) [name properties]
    (seq children) (into [name] children)
    :else name))

(defn- -guard [pred tf] (if tf (fn [x] (if (pred x) (tf x) x))))

(defn- -chain [phase chain]
  (let [f (case phase, :enter identity, :leave reverse)]
    (some->> chain (keep identity) (seq) (f) (reverse) (apply comp))))

(defn- -leaf-schema [name ->validator-and-children]
  ^{:type ::into-schema}
  (reify IntoSchema
    (-into-schema [_ properties children options]
      (let [[validator children] (->validator-and-children properties children options)
            form (create-form name properties children)]
        ^{:type ::schema}
        (reify
          Schema
          (-name [_] name)
          (-validator [_] validator)
          (-explainer [this path]
            (fn [value in acc]
              (if-not (validator value) (conj acc (error path in this value)) acc)))
          (-transformer [this transformer method options]
            (-value-transformer transformer this method options))
          (-accept [this visitor options] (visitor this (vec children) options))
          (-properties [_] properties)
          (-options [_] options)
          (-form [_] form))))))

(defn fn-schema [name f]
  (-leaf-schema
    name
    (fn [properties children _]
      (when (seq children)
        (fail! ::child-error {:name name, :properties properties, :children children, :min 0, :max 0}))
      [f children])))

(defn- -partial-fn-schema [name f]
  (-leaf-schema
    name
    (fn [properties [child :as children] _]
      (when-not (= 1 (count children))
        (fail! ::child-error {:name name, :properties properties, :children children, :min 1, :max 1}))
      [#(try (f % child) (catch #?(:clj Exception, :cljs js/Error) _ false)) children])))

(defn- -composite-schema [name f short-circuit]
  ^{:type ::into-schema}
  (reify IntoSchema
    (-into-schema [_ properties children options]
      (when-not (seq children)
        (fail! ::no-children {:name name, :properties properties}))
      (let [child-schemas (mapv #(schema % options) children)
            validators (distinct (map -validator child-schemas))
            validator (apply f validators)]
        ^{:type ::schema}
        (reify Schema
          (-name [_] name)
          (-validator [_] validator)
          (-explainer [_ path]
            (let [distance (if (seq properties) 2 1)
                  explainers (mapv (fn [[i c]] (-explainer c (into path [(+ i distance)]))) (map-indexed vector child-schemas))]
              (fn explain [x in acc]
                (reduce
                  (fn [acc' explainer]
                    (let [acc'' (explainer x in acc')]
                      (cond
                        (and short-circuit (identical? acc' acc'')) (reduced acc)
                        (nil? acc'') acc'
                        :else acc'')))
                  acc explainers))))
          (-transformer [this transformer method options]
            (let [this-transformer (-value-transformer transformer this method options)
                  child-transformers (map #(-transformer % transformer method options) child-schemas)
                  build (fn [phase]
                          (let [->this (phase this-transformer)
                                ?->this (or ->this identity)
                                ->children (into [] (keep phase) child-transformers)]
                            (cond
                              (not (seq ->children)) ->this
                              short-circuit (fn [x]
                                              (let [x (?->this x)]
                                                (reduce-kv
                                                  (fn [_ _ t]
                                                    (let [x' (t x)]
                                                      (if-not (identical? x' x)
                                                        (reduced x')
                                                        x)))
                                                  x ->children)))
                              :else (fn [x]
                                      (reduce-kv
                                        (fn [x' _ t] (t x'))
                                        (?->this x) ->children)))))]
              {:enter (build :enter)
               :leave (build :leave)}))
          (-accept [this visitor options]
            (visitor this (mapv #(-accept % visitor options) child-schemas) options))
          (-properties [_] properties)
          (-options [_] options)
          (-form [_] (create-form name properties (map -form child-schemas))))))))

(defn- -properties-and-children [xs]
  (if ((some-fn map? nil?) (first xs))
    [(first xs) (rest xs)]
    [nil xs]))

(defn- -expand-key [[k ?p ?v] options f]
  (let [[p v] (if (or (nil? ?p) (map? ?p)) [?p ?v] [nil ?p])]
    [k p (f (schema v options))]))

(defn- -parse-map-entries [children options]
  (->> children (mapv #(-expand-key % options identity))))

(defn ^:no-doc map-entry-forms [entries]
  (mapv (fn [[k p v]] (let [v' (-form v)] (if p [k p v'] [k v']))) entries))

(defn ^:no-doc required-map-entry? [[_ ?p]]
  (not (and (map? ?p) (true? (:optional ?p)))))

(defn- -map-schema []
  ^{:type ::into-schema}
  (reify IntoSchema
    (-into-schema [_ {:keys [closed] :as properties} children options]
      (let [entries (-parse-map-entries children options)
            keyset (->> entries (map first) (set))
            forms (map-entry-forms entries)
            form (create-form :map properties forms)]
        ^{:type ::schema}
        (reify Schema
          (-name [_] :map)
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
            (let [distance (if (seq properties) 2 1)
                  explainers (cond-> (mapv
                                       (fn [[i [key {:keys [optional] :as key-properties} schema]]]
                                         (let [key-distance (if (seq key-properties) 2 1)
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
                  child-transformers (some->>
                                       entries
                                       (keep (fn [[k _ s]] (if-let [t (-transformer s transformer method options)] [k t])))
                                       (into {}))
                  build (fn [phase]
                          (let [->this (phase this-transformer)
                                ->children (->> child-transformers
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
          (-accept [this visitor options]
            (visitor this (mapv (fn [[k p s]] [k p (-accept s visitor options)]) entries) options))
          (-properties [_] properties)
          (-options [_] options)
          (-form [_] form)
          MapSchema
          (-map-entries [_] entries)
          LookupSchema
          (-get [_ key default] (or (some (fn [[k _ s]] (if (= k key) s)) entries) default)))))))

(defn- -map-of-schema []
  ^{:type ::into-schema}
  (reify IntoSchema
    (-into-schema [_ properties children options]
      (when-not (and (seq children) (= 2 (count children)))
        (fail! ::child-error {:name :vector, :properties properties, :children children, :min 2, :max 2}))
      (let [[key-schema value-schema :as schemas] (mapv #(schema % options) children)
            key-valid? (-validator key-schema)
            value-valid? (-validator value-schema)
            validate (fn [m]
                       (reduce-kv
                         (fn [_ key value]
                           (or (and (key-valid? key) (value-valid? value)) (reduced false)))
                         true m))]
        ^{:type ::schema}
        (reify Schema
          (-name [_] :map-of)
          (-validator [_] (fn [m] (and (map? m) (validate m))))
          (-explainer [this path]
            (let [distance (if (seq properties) 2 1)
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
          (-accept [this visitor options]
            (visitor this (mapv #(-accept % visitor options) schemas) options))
          (-properties [_] properties)
          (-options [_] options)
          (-form [_] (create-form :map-of properties (mapv -form schemas))))))))

(defn- -collection-schema [name fpred fwrap fempty]
  ^{:type ::into-schema}
  (reify IntoSchema
    (-into-schema [_ {:keys [min max] :as properties} children options]
      (when-not (= 1 (count children))
        (fail! ::child-error {:name name, :properties properties, :children children, :min 1, :max 1}))
      (let [schema (schema (first children) options)
            form (create-form name properties [(-form schema)])
            fwrap (fn [x]
                    (if (coll? x)
                      (fwrap x)
                      x))
            validate-limits (cond
                              (not (or min max)) (constantly true)
                              (and min max) (fn [x] (let [size (count x)] (<= min size max)))
                              min (fn [x] (let [size (count x)] (<= min size)))
                              max (fn [x] (let [size (count x)] (<= size max))))]
        ^{:type ::schema}
        (reify Schema
          (-name [_] name)
          (-validator [_]
            (let [validator (-validator schema)]
              (fn [x] (and (fpred x)
                           (validate-limits x)
                           (reduce (fn [acc v] (if (validator v) acc (reduced false))) true x)))))
          (-explainer [this path]
            (let [distance (if (seq properties) 2 1)
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
            (let [this-transformer (-value-transformer transformer this method options)
                  child-transformer (-transformer schema transformer method options)
                  build (fn [phase]
                          (let [->this (or (phase this-transformer) fwrap)
                                ->child (if-let [ct (phase child-transformer)]
                                          (if fempty
                                            #(into (if % fempty) (map ct) %)
                                            #(map ct %)))]
                            (-chain phase [->this (-guard coll? ->child)])))]
              {:enter (build :enter)
               :leave (build :leave)}))
          (-accept [this visitor options] (visitor this [(-accept schema visitor options)] options))
          (-properties [_] properties)
          (-options [_] options)
          (-form [_] form)
          LookupSchema
          (-get [_ key default] (if (= 0 key) schema default)))))))

(defn- -tuple-schema []
  ^{:type ::into-schema}
  (reify IntoSchema
    (-into-schema [_ properties children options]
      (let [schemas (mapv #(schema % options) children)
            size (count schemas)
            form (create-form :tuple properties (map -form schemas))
            validators (into (array-map) (map-indexed vector (mapv -validator schemas)))]
        (when-not (seq children)
          (fail! ::child-error {:name :tuple, :properties properties, :children children, :min 1}))
        ^{:type ::schema}
        (reify Schema
          (-name [_] :tuple)
          (-validator [_]
            (fn [x] (and (vector? x)
                         (= (count x) size)
                         (reduce-kv
                           (fn [acc i validator]
                             (if (validator (nth x i)) acc (reduced false))) true validators))))
          (-explainer [this path]
            (let [distance (if (seq properties) 2 1)
                  explainers (mapv (fn [[i s]]
                                     (-explainer s (conj path (+ i distance))))
                                   (map-indexed vector schemas))]
              (fn [x in acc]
                (cond
                  (not (vector? x)) (conj acc (error path in this x ::invalid-type))
                  (not= (count x) size) (conj acc (error path in this x ::tuple-size))
                  :else (loop [acc acc, i 0, [x & xs] x, [e & es] explainers]
                          (cond-> (e x (conj in i) acc) xs (recur (inc i) xs es)))))))
          (-transformer [this transformer method options]
            (let [this-transformer (-value-transformer transformer this method options)
                  child-transformers (->> schemas
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
          (-accept [this visitor options] (visitor this (mapv #(-accept % visitor options) schemas) options))
          (-properties [_] properties)
          (-options [_] options)
          (-form [_] form)
          LookupSchema
          (-get [_ key default] (get schemas key default)))))))

(defn- -enum-schema []
  ^{:type ::into-schema}
  (reify IntoSchema
    (-into-schema [_ properties children options]
      (when-not (seq children)
        (fail! ::no-children {:name :enum, :properties properties}))
      (let [schema (set children)
            validator (fn [x] (contains? schema x))]
        ^{:type ::schema}
        (reify Schema
          (-name [_] :enum)
          (-validator [_] validator)
          (-explainer [this path]
            (fn explain [x in acc]
              (if-not (validator x) (conj acc (error path in this x)) acc)))
          ;; TODO: should we try to derive the type from values? e.g. [:enum 1 2] ~> int?
          (-transformer [this transformer method options]
            (-value-transformer transformer this method options))
          (-accept [this visitor options] (visitor this (vec children) options))
          (-properties [_] properties)
          (-options [_] options)
          (-form [_] (create-form :enum properties children)))))))

(defn- -re-schema [class?]
  ^{:type ::into-schema}
  (reify IntoSchema
    (-into-schema [_ properties [child :as children] options]
      (when-not (= 1 (count children))
        (fail! ::child-error {:name :re, :properties properties, :children children, :min 1, :max 1}))
      (let [re (re-pattern child)
            validator (fn [x] (try (boolean (re-find re x)) (catch #?(:clj Exception, :cljs js/Error) _ false)))
            form (if class? re (create-form :re properties children))]
        ^{:type ::schema}
        (reify Schema
          (-name [_] :re)
          (-validator [_] validator)
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
          (-accept [this visitor options] (visitor this (vec children) options))
          (-properties [_] properties)
          (-options [_] options)
          (-form [_] form))))))

(defn- -fn-schema []
  ^{:type ::into-schema}
  (reify IntoSchema
    (-into-schema [_ properties children options]
      (when-not (= 1 (count children))
        (fail! ::child-error {:name :fn, :properties properties, :children children, :min 1, :max 1}))
      (let [f (eval (first children))
            validator (fn [x] (try (f x) (catch #?(:clj Exception, :cljs js/Error) _ false)))]
        ^{:type ::schema}
        (reify Schema
          (-name [_] :fn)
          (-validator [_] validator)
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
          (-accept [this visitor options] (visitor this (vec children) options))
          (-properties [_] properties)
          (-options [_] options)
          (-form [_] (create-form :fn properties children)))))))

(defn- -maybe-schema []
  ^{:type ::into-schema}
  (reify IntoSchema
    (-into-schema [_ properties children options]
      (when-not (= 1 (count children))
        (fail! ::child-error {:name :vector, :properties properties, :children children, :min 1, :max 1}))
      (let [schema' (-> children first (schema options))
            validator' (-validator schema')
            form (create-form :maybe properties [(-form schema')])]
        ^{:type ::schema}
        (reify Schema
          (-name [_] :maybe)
          (-validator [_] (fn [x] (or (nil? x) (validator' x))))
          (-explainer [this path]
            (fn explain [x in acc]
              (if-not (or (nil? x) (validator' x)) (conj acc (error path in this x)) acc)))
          (-transformer [this transformer method options]
            (let [this-transformer (-value-transformer transformer this method options)
                  child-transformer (-transformer schema' transformer method options)
                  build (fn [phase]
                          (let [->this (phase this-transformer)
                                ->child (phase child-transformer)]
                            (if (and ->this ->child)
                              (comp ->child ->this)
                              (or ->this ->child))))]
              {:enter (build :enter)
               :leave (build :leave)}))
          (-accept [this visitor options] (visitor this [(-accept schema' visitor options)] options))
          (-properties [_] properties)
          (-options [_] options)
          (-form [_] form)
          LookupSchema
          (-get [_ key default] (if (= 0 key) schema' default)))))))

(defn- -multi-schema []
  ^{:type ::into-schema}
  (reify IntoSchema
    (-into-schema [_ properties children options]
      (let [entries (-parse-map-entries children options)
            forms (map-entry-forms entries)
            dispatch (eval (:dispatch properties))
            dispatch-map (->> (for [[d _ s] entries] [d s]) (into {}))
            form (create-form :multi properties forms)]
        (when-not dispatch
          (fail! ::missing-property {:key :dispatch}))
        ^{:type ::schema}
        (reify Schema
          (-name [_] :multi)
          (-validator [_]
            (let [validators (reduce-kv (fn [acc k s] (assoc acc k (-validator s))) {} dispatch-map)]
              (fn [x]
                (if-let [validator (validators (dispatch x))]
                  (validator x)
                  false))))
          (-explainer [this path]
            (let [explainers (reduce-kv (fn [acc k s] (assoc acc k (-explainer s path))) {} dispatch-map)]
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
          (-accept [this visitor options]
            (visitor this (mapv (fn [[k p s]] [k p (-accept s visitor options)]) entries) options))
          (-properties [_] properties)
          (-options [_] options)
          (-form [_] form))))))

(defn- -register [registry k schema]
  (if (contains? registry k)
    (fail! ::schema-already-registered {:key k, :registry registry}))
  (assoc registry k schema))

(defn- -register-var [registry v]
  (let [name (-> v meta :name)
        schema (fn-schema name @v)]
    (-> registry
        (-register name schema)
        (-register @v schema))))

(defn- -schema [?schema {:keys [registry] :as options :or {registry default-registry}}]
  (or (if (satisfies? IntoSchema ?schema) ?schema)
      (get registry ?schema)
      (some-> registry (get (type ?schema)) (-into-schema nil [?schema] options))))

(defn ^:no-doc into-transformer [x]
  (cond
    (satisfies? Transformer x) x
    (fn? x) (into-transformer (x))
    :else (fail! ::invalid-transformer {:value x})))

;;
;; public api
;;

(defn into-schema
  ([name properties children]
   (into-schema name properties children nil))
  ([name properties children options]
   (-into-schema (-schema name options) (if (seq properties) properties) children options)))

(defn schema? [x]
  (satisfies? Schema x))

(defn schema
  ([?schema]
   (schema ?schema nil))
  ([?schema options]
   (cond
     (schema? ?schema) ?schema
     (satisfies? IntoSchema ?schema) (-into-schema ?schema nil nil options)
     (vector? ?schema) (apply into-schema (concat [(-schema (first ?schema) options)]
                                                  (-properties-and-children (rest ?schema)) [options]))
     :else (or (some-> ?schema (-schema options) (schema options)) (fail! ::invalid-schema {:schema ?schema})))))

(defn form
  ([?schema]
   (form ?schema nil))
  ([?schema options]
   (-form (schema ?schema options))))

(defn accept
  ([?schema visitor]
   (accept ?schema visitor nil))
  ([?schema visitor options]
   (-accept (schema ?schema options) visitor options)))

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
   (let [schema (schema ?schema options)
         form (-form schema)]
     (if (vector? form)
       (->> form (drop (if (seq (-properties schema)) 2 1)))))))

(defn name
  ([?schema]
   (name ?schema nil))
  ([?schema options]
   (-name (schema ?schema options))))

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
  "Transforms a value with a given decoding transformer agains a schema."
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
  "Transforms a value with a given encoding transformer agains a schema."
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

(defn ^:no-doc eval [?code]
  (if (fn? ?code) ?code (sci/eval-string (str ?code) {:preset :termination-safe
                                                      :bindings {'m/properties properties
                                                                 'm/name name
                                                                 'm/children children
                                                                 'm/map-entries map-entries}})))
;;
;; Visitors
;;

(defn schema-visitor [f]
  (fn [schema children options]
    (f (into-schema (name schema) (properties schema) children options))))

(defn ^:no-doc map-syntax-visitor [schema children _]
  (let [properties (properties schema)]
    (cond-> {:name (name schema)}
            (seq properties) (assoc :properties properties)
            (seq children) (assoc :children children))))

;;
;; registries
;;

(def predicate-registry
  (->> [#'any? #'some? #'number? #'integer? #'int? #'pos-int? #'neg-int? #'nat-int? #'float? #'double?
        #'boolean? #'string? #'ident? #'simple-ident? #'qualified-ident? #'keyword? #'simple-keyword?
        #'qualified-keyword? #'symbol? #'simple-symbol? #'qualified-symbol? #'uuid? #'uri? #?(:clj #'decimal?)
        #'inst? #'seqable? #'indexed? #'map? #'vector? #'list? #'seq? #'char? #'set? #'nil? #'false? #'true?
        #'zero? #?(:clj #'rational?) #'coll? #'empty? #'associative? #'sequential? #?(:clj #'ratio?) #?(:clj #'bytes?)]
       (reduce -register-var {})))

(def class-registry
  {#?(:clj Pattern, :cljs js/RegExp) (-re-schema true)})

(def comparator-registry
  (->> {:> >, :>= >=, :< <, :<= <=, := =, :not= not=}
       (map (fn [[k v]] [k (-partial-fn-schema k v)]))
       (into {})
       (reduce-kv -register nil)))

(def base-registry
  {:and (-composite-schema :and every-pred false)
   :or (-composite-schema :or some-fn true)
   :map (-map-schema)
   :map-of (-map-of-schema)
   :vector (-collection-schema :vector vector? vec [])
   :list (-collection-schema :list list? seq nil)
   :sequential (-collection-schema :sequential sequential? seq nil)
   :set (-collection-schema :set set? set #{})
   :enum (-enum-schema)
   :maybe (-maybe-schema)
   :tuple (-tuple-schema)
   :multi (-multi-schema)
   :re (-re-schema false)
   :fn (-fn-schema)})

(def default-registry
  (clojure.core/merge predicate-registry class-registry comparator-registry base-registry))
