(ns malli.core
  (:refer-clojure :exclude [eval type -deref deref -lookup -key])
  (:require [malli.sci :as ms]
            [malli.registry :as mr])
  #?(:clj (:import (java.util.regex Pattern)
                   (clojure.lang IDeref MapEntry))))

;;
;; protocols and records
;;

(defprotocol Schemas
  (-schema? [this])
  (-into-schema? [this]))

(defprotocol IntoSchema
  (-into-schema [this properties children options] "creates a new schema instance"))

(defprotocol Schema
  (-type [this] "returns type of the schema")
  (-type-properties [this] "returns schema type properties")
  (-validator [this] "returns a predicate function that checks if the schema is valid")
  (-explainer [this path] "returns a function of `x in acc -> maybe errors` to explain the errors for invalid values")
  (-transformer [this transformer method options] "returns an interceptor map with :enter and :leave functions to transform the value for the given schema and method")
  (-walk [this walker path options] "walks the schema and it's children, ::m/walk-entry-vals, ::m/walk-refs, ::m/walk-schema-refs options effect how walking is done.")
  (-properties [this] "returns original schema properties")
  (-options [this] "returns original options")
  (-children [this] "returns schema children")
  (-form [this] "returns original form of the schema"))

(defprotocol MapSchema
  (-entries [this] "returns sequence of `key -val-schema` MapEntries"))

(defprotocol LensSchema
  (-keep [this] "returns truthy if schema contributes to value path")
  (-get [this key default] "returns schema at key")
  (-set [this key value] "returns a copy with key having new value"))

(defprotocol RefSchema
  (-ref [this] "returns the reference name")
  (-deref [this] "returns the referenced schema"))

(defprotocol Walker
  (-accept [this schema path options])
  (-inner [this schema path options])
  (-outer [this schema path children options]))

(defprotocol Transformer
  (-transformer-chain [this] "returns transformer chain as a vector of maps with :name, :encoders, :decoders and :options")
  (-value-transformer [this schema method options] "returns an value transforming interceptor for the given schema and method"))

(defrecord SchemaError [path in schema value type message])

#?(:clj (defmethod print-method SchemaError [v ^java.io.Writer w] (.write w (str "#Error" (->> v (filter val) (into {}))))))
#?(:clj (defmethod print-method ::into-schema [v ^java.io.Writer w] (.write w (str "#IntoSchema{:class " v "}"))))
#?(:clj (defmethod print-method ::schema [v ^java.io.Writer w] (.write w (pr-str (-form v)))))

;;
;; impl
;;

(declare schema schema? into-schema into-schema? eval default-registry -simple-schema -val-schema -ref-schema -schema-schema -registry)

(defn -safe-pred [f] #(try (f %) (catch #?(:clj Exception, :cljs js/Error) _ false)))

(defn -keyword->string [x]
  (if (keyword? x)
    (if-let [nn (namespace x)]
      (str nn "/" (name x))
      (name x))
    x))

(defn -error
  ([path in schema value]
   (->SchemaError path in schema value nil nil))
  ([path in schema value type]
   (->SchemaError path in schema value type nil)))

(defn -fail!
  ([type]
   (-fail! type nil))
  ([type data]
   (throw (ex-info (str type " " (pr-str data)) {:type type, :data data}))))

(defn -check-children! [type properties children {:keys [min max] :as opts}]
  (if (or (and min (< (count children) min)) (and max (> (count children) max)))
    (-fail! ::child-error (merge {:type type, :properties properties, :children children} opts))))

(defn -create-form [type properties children]
  (cond
    (and (seq properties) (seq children)) (into [type properties] children)
    (seq properties) [type properties]
    (seq children) (into [type] children)
    :else type))

(defn -pointer [id schema options] (-into-schema (-schema-schema {:id id}) nil [schema] options))

(defn -reference? [?schema] (or (string? ?schema) (qualified-keyword? ?schema)))

(defn -lazy [ref options] (-into-schema (-ref-schema {:lazy true}) nil [ref] options))

(defn -boolean-fn [x] (cond (boolean? x) (constantly x) (ifn? x) x :else (constantly false)))

(defn -comp
  ([] identity)
  ([f] f)
  ([f g] (fn [x] (f (g x))))
  ([f g h] (fn [x] (f (g (h x)))))
  ([f1 f2 f3 & fs]
   (let [fs (reverse (list* f1 f2 f3 fs))]
     #(loop [ret ((first fs) %), fs (next fs)] (if fs (recur ((first fs) ret) (next fs)) ret)))))

(defn -update [m k f] (assoc m k (f (get m k))))

(defn -inner-indexed [walker path children options]
  (mapv (fn [[i c]] (-inner walker c (conj path i) options)) (map-indexed vector children)))

(defn -inner-entries [walker path entries options]
  (mapv (fn [[k s]] [k (-properties s) (-inner walker s (conj path k) options)]) entries))

(defn -get-entries [schema key default]
  (or (if (and (vector? key) (= ::val (nth key 0)))
        (some (fn [[k s]] (if (= k (nth key 1)) s)) (-entries schema))
        (some (fn [[k _ s]] (if (= k key) s)) (-children schema)))
      default))

(defn -set-entries [schema key value]
  (let [found (atom nil)
        [key :as new-child] (if (vector? key) (conj key value) [key value])
        children (cond-> (mapv (fn [[k :as child]] (if (= key k) (do (reset! found true) new-child) child)) (-children schema))
                         (not @found) (conj new-child)
                         :always (->> (filter (fn [e] (-> e last some?)))))]
    (into-schema (-type schema) (-properties schema) children)))

(defn -parse-entries [children {:keys [naked-keys lazy-refs]} options]
  (let [-entry (fn [k v] #?(:clj (MapEntry. k v), :cljs (MapEntry. k v nil)))
        -parse (fn [e] (let [[[k ?p ?v] f] (cond
                                             (-reference? e) (if naked-keys [[e nil e] e])
                                             (and (= 2 (count e)) (-reference? (first e)) (map? (last e))) (if naked-keys [(conj e (first e)) e])
                                             :else [e (->> (-update (vec e) (dec (count e)) (-comp -form #(schema % options))) (keep identity) (vec))])
                             _ (when (nil? k) (-fail! ::naked-keys-not-supported))
                             [p ?s] (if (or (nil? ?p) (map? ?p)) [?p ?v] [nil ?p])
                             s (cond-> (or ?s (if (-reference? k) f)) lazy-refs (-lazy options))
                             c [k p (schema s options)]]
                         {:children [c]
                          :entries [(-entry k (-val-schema (last c) p))]
                          :forms [f]}))
        es (reduce #(merge-with into %1 %2) {} (mapv -parse children))
        keys (->> es :entries (map first))]
    (when-not (= keys (distinct keys))
      (-fail! ::non-distinct-entry-keys {:keys keys}))
    es))

(defn -guard [pred tf]
  (if tf (fn [x] (if (pred x) (tf x) x))))

(defn -coder [{:keys [enter leave]}]
  (if (and enter leave) #(leave (enter %)) (or enter leave identity)))

(defn -chain [phase chain]
  (some->> (case phase, :enter (rseq chain), :leave chain) (keep identity) (seq) (apply -comp)))

(defn -parent-children-transformer [parent children transformer method options]
  (let [parent-transformer (-value-transformer transformer parent method options)
        child-transformers (map #(-transformer % transformer method options) children)
        build (fn [phase] (-chain phase (apply vector (phase parent-transformer) (map phase child-transformers))))]
    {:enter (build :enter)
     :leave (build :leave)}))

(defn- -properties-and-children [[x :as xs]]
  (if (or (nil? x) (map? x))
    [x (rest xs)]
    [nil xs]))

(defn- -register-var [registry v]
  (let [name (-> v meta :name)
        schema (-simple-schema {:type name, :pred @v})]
    (-> registry
        (assoc name schema)
        (assoc @v schema))))

(defn -registry
  ([] default-registry)
  ([{:keys [registry]}] (or (mr/registry registry) default-registry)))

(defn- -lookup [?schema options]
  (let [registry (-registry options)]
    (or (mr/-schema registry ?schema)
        (some-> registry (mr/-schema (clojure.core/type ?schema)) (-into-schema nil [?schema] options)))))

(defn- -schema [?schema options]
  (or (and (or (schema? ?schema) (into-schema? ?schema)) ?schema)
      (-lookup ?schema options)
      (-fail! ::invalid-schema {:schema ?schema})))

(defn -into-transformer [x]
  (cond
    (satisfies? Transformer x) x
    (fn? x) (-into-transformer (x))
    :else (-fail! ::invalid-transformer {:value x})))

(defn- -property-registry [m options f]
  (let [options (assoc options ::allow-invalid-refs true)]
    (reduce-kv (fn [acc k v] (assoc acc k (f (schema v options)))) {} m)))

(defn -properties-and-options [properties options f]
  (if-let [r (some-> properties :registry)]
    (let [options (-update options :registry #(mr/composite-registry r (or % (-registry options))))]
      [(assoc properties :registry (-property-registry r options f)) options])
    [properties options]))

(defn -min-max-pred [f]
  (fn [{:keys [min max]}]
    (cond
      (not (or min max)) nil
      (and (and min max) f) (fn [x] (let [size (f x)] (<= min size max)))
      (and min max) (fn [x] (<= min x max))
      (and min f) (fn [x] (<= min (f x)))
      min (fn [x] (<= min x))
      (and max f) (fn [x] (<= (f x) max))
      max (fn [x] (<= x max)))))

;;
;; Protocol Cache
;;

(let [extend (fn [protocol this]
               ;; cljs: class clojure.lang.PersistentList cannot be cast to class clojure.lang.Named
               #?(:clj (let [s? (satisfies? Schema this)
                             is? (satisfies? IntoSchema this)]
                         (extend-protocol Schemas (class this)
                           (-schema? [_] s?)
                           (-into-schema? [_] is?))))
               (satisfies? protocol this))]
  (extend-protocol Schemas
    nil
    (-schema? [_] false)
    (-into-schema? [_] false)
    #?(:clj Object, :cljs default)
    (-schema? [this] #?(:clj (extend Schema this)) (satisfies? Schema this))
    (-into-schema? [this] #?(:clj (extend IntoSchema this)) (satisfies? IntoSchema this))))

;;
;; Schemas
;;

(defn -simple-schema [?props]
  ^{:type ::into-schema}
  (reify IntoSchema
    (-into-schema [_ properties children options]
      (let [{:keys [type pred property-pred type-properties min max]
             :or {min 0, max 0}} (if (fn? ?props) (?props properties children) ?props)]
        (-check-children! type properties children {:min min, :max max})
        (let [pvalidator (if property-pred (property-pred properties))
              validator (if pvalidator (fn [x] (and (pred x) (pvalidator x))) pred)
              form (-create-form type properties children)]
          ^{:type ::schema}
          (reify
            Schema
            (-type [_] type)
            (-type-properties [_] type-properties)
            (-validator [_] validator)
            (-explainer [this path]
              (fn explain [x in acc]
                (if-not (validator x) (conj acc (-error path in this x)) acc)))
            (-transformer [this transformer method options]
              (-value-transformer transformer this method options))
            (-walk [this walker path options]
              (if (-accept walker this path options)
                (-outer walker this path (vec children) options)))
            (-properties [_] properties)
            (-options [_] options)
            (-children [_] children)
            (-form [_] form)
            LensSchema
            (-keep [_])
            (-get [_ _ default] default)
            (-set [this key _] (-fail! ::non-associative-schema {:schema this, :key key}))))))))

(defn -string-schema [] (-simple-schema {:type :string, :pred string?, :property-pred (-min-max-pred count)}))
(defn -int-schema [] (-simple-schema {:type :int, :pred int?, :property-pred (-min-max-pred nil)}))
(defn -double-schema [] (-simple-schema {:type :double, :pred double?, :property-pred (-min-max-pred nil)}))
(defn -boolean-schema [] (-simple-schema {:type :boolean, :pred boolean?}))
(defn -keyword-schema [] (-simple-schema {:type :keyword, :pred keyword?}))
(defn -symbol-schema [] (-simple-schema {:type :symbol, :pred symbol?}))
(defn -qualified-keyword-schema [] (-simple-schema {:type :qualified-keyword, :pred qualified-keyword?}))
(defn -qualified-symbol-schema [] (-simple-schema {:type :qualified-symbol, :pred qualified-symbol?}))
(defn -uuid-schema [] (-simple-schema {:type :uuid, :pred uuid?}))

(defn -and-schema []
  ^{:type ::into-schema}
  (reify IntoSchema
    (-into-schema [_ properties children options]
      (-check-children! :and properties children {:min 1})
      (let [children (mapv #(schema % options) children)
            form (-create-form :and properties (map -form children))]
        ^{:type ::schema}
        (reify
          Schema
          (-type [_] :and)
          (-type-properties [_])
          (-validator [_]
            (let [validators (distinct (map -validator children))]
              (if (second validators) (apply every-pred validators) (first validators))))
          (-explainer [_ path]
            (let [explainers (mapv (fn [[i c]] (-explainer c (conj path i))) (map-indexed vector children))]
              (fn explain [x in acc] (reduce (fn [acc' explainer] (explainer x in acc')) acc explainers))))
          (-transformer [this transformer method options]
            (-parent-children-transformer this children transformer method options))
          (-walk [this walker path options]
            (if (-accept walker this path options)
              (-outer walker this path (-inner-indexed walker path children options) options)))
          (-properties [_] properties)
          (-options [_] options)
          (-children [_] children)
          (-form [_] form)
          LensSchema
          (-keep [_])
          (-get [_ key default] (get children key default))
          (-set [_ key value] (into-schema :and properties (assoc children key value))))))))

(defn -or-schema []
  ^{:type ::into-schema}
  (reify IntoSchema
    (-into-schema [_ properties children options]
      (-check-children! :or properties children {:min 1})
      (let [children (mapv #(schema % options) children)
            form (-create-form :or properties (map -form children))]
        ^{:type ::schema}
        (reify
          Schema
          (-type [_] :or)
          (-type-properties [_])
          (-validator [_]
            (let [validators (distinct (map -validator children))]
              (if (second validators) (fn [x] (boolean (some #(% x) validators))) (first validators))))
          (-explainer [_ path]
            (let [explainers (mapv (fn [[i c]] (-explainer c (conj path i))) (map-indexed vector children))]
              (fn explain [x in acc]
                (reduce
                  (fn [acc' explainer]
                    (let [acc'' (explainer x in acc')]
                      (if (identical? acc' acc'') (reduced acc) acc'')))
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
          (-walk [this walker path options]
            (if (-accept walker this path options)
              (-outer walker this path (-inner-indexed walker path children options) options)))
          (-properties [_] properties)
          (-options [_] options)
          (-children [_] children)
          (-form [_] form)
          LensSchema
          (-keep [_])
          (-get [_ key default] (get children key default))
          (-set [_ key value] (into-schema :or properties (assoc children key value))))))))

(defn -val-schema
  ([schema properties]
   (-into-schema (-val-schema) properties [schema] (-options schema)))
  ([]
   ^{:type ::into-schema}
   (reify IntoSchema
     (-into-schema [_ properties children options]
       (-check-children! ::val properties children {:min 1, :max 1})
       (let [[schema :as children] (map #(schema % options) children)
             form (-create-form ::val properties (map -form children))]
         ^{:type ::schema}
         (reify Schema
           (-type [_] ::val)
           (-type-properties [_])
           (-validator [_] (-validator schema))
           (-explainer [_ path] (-explainer schema path))
           (-transformer [this transformer method options]
             (-parent-children-transformer this children transformer method options))
           (-walk [this walker path options]
             (if (::walk-entry-vals options)
               (if (-accept walker this path options)
                 (-outer walker this path [(-inner walker schema path options)] options))
               (-walk schema walker path options)))
           (-properties [_] properties)
           (-options [_] (-options schema))
           (-children [_] children)
           (-form [_] form)
           LensSchema
           (-keep [_])
           (-get [_ key default] (if (= 0 key) schema default))
           (-set [_ key value] (if (= 0 key) (-val-schema value properties)))
           RefSchema
           (-ref [_])
           (-deref [_] schema)))))))

(defn -map-schema
  ([]
   (-map-schema {:naked-keys true}))
  ([opts] ;; :naked-keys
   ^{:type ::into-schema}
   (reify IntoSchema
     (-into-schema [_ {:keys [closed] :as properties} children options]
       (let [{:keys [children entries forms]} (-parse-entries children opts options)
             form (-create-form :map properties forms)
             keyset (->> entries (map first) (set))]
         ^{:type ::schema}
         (reify
           Schema
           (-type [_] :map)
           (-type-properties [_])
           (-validator [_]
             (let [validators (cond-> (mapv
                                        (fn [[key {:keys [optional]} value]]
                                          (let [valid? (-validator value)
                                                default (boolean optional)]
                                            (fn [m] (if-let [map-entry (find m key)] (valid? (val map-entry)) default))))
                                        children)
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
             (let [explainers (cond-> (mapv
                                        (fn [[key {:keys [optional]} schema]]
                                          (let [explainer (-explainer schema (conj path key))]
                                            (fn [x in acc]
                                              (if-let [e (find x key)]
                                                (explainer (val e) (conj in key) acc)
                                                (if-not optional
                                                  (conj acc (-error (conj path key) (conj in key) this nil ::missing-key))
                                                  acc)))))
                                        children)
                                      closed (into [(fn [x in acc]
                                                      (reduce
                                                        (fn [acc k]
                                                          (if (contains? keyset k)
                                                            acc
                                                            (conj acc (-error (conj path k) (conj in k) this nil ::extra-key))))
                                                        acc (keys x)))]))]
               (fn [x in acc]
                 (if-not (map? x)
                   (conj acc (-error path in this x ::invalid-type))
                   (reduce
                     (fn [acc explainer]
                       (explainer x in acc))
                     acc explainers)))))
           (-transformer [this transformer method options]
             (let [this-transformer (-value-transformer transformer this method options)
                   transformers (some->> entries
                                         (keep (fn [[k s]] (if-let [t (-transformer s transformer method options)] [k t])))
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
           (-walk [this walker path options]
             (if (-accept walker this path options)
               (-outer walker this path (-inner-entries walker path entries options) options)))
           (-properties [_] properties)
           (-options [_] options)
           (-children [_] children)
           (-form [_] form)
           MapSchema
           (-entries [_] entries)
           LensSchema
           (-keep [_] true)
           (-get [this key default] (-get-entries this key default))
           (-set [this key value] (-set-entries this key value))))))))

(defn -map-of-schema []
  ^{:type ::into-schema}
  (reify IntoSchema
    (-into-schema [_ properties children options]
      (-check-children! :map-of properties children {:min 2 :max 2})
      (let [[key-schema value-schema :as children] (mapv #(schema % options) children)
            form (-create-form :map-of properties (mapv -form children))]
        ^{:type ::schema}
        (reify
          Schema
          (-type [_] :map-of)
          (-type-properties [_])
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
            (let [key-explainer (-explainer key-schema (conj path 0))
                  value-explainer (-explainer value-schema (conj path 1))]
              (fn explain [m in acc]
                (if-not (map? m)
                  (conj acc (-error path in this m ::invalid-type))
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
          (-walk [this walker path options]
            (if (-accept walker this path options)
              (-outer walker this path (-inner-indexed walker path children options) options)))
          (-properties [_] properties)
          (-options [_] options)
          (-children [_] children)
          (-form [_] form)
          LensSchema
          (-keep [_])
          (-get [_ key default] (get children key default))
          (-set [_ key value] (into-schema :map-of properties (assoc children key value))))))))

(defn -collection-schema [{type :type fpred :pred, fempty :empty, fin :in :or {fin (fn [i _] i)}}]
  ^{:type ::into-schema}
  (reify IntoSchema
    (-into-schema [_ {:keys [min max] :as properties} children options]
      (-check-children! type properties children {:min 1 :max 1})
      (let [[schema :as children] (mapv #(schema % options) children)
            form (-create-form type properties (map -form children))
            validate-limits (cond
                              (not (or min max)) (constantly true)
                              (and min max) (fn [x] (let [size (count x)] (<= min size max)))
                              min (fn [x] (let [size (count x)] (<= min size)))
                              max (fn [x] (let [size (count x)] (<= size max))))]
        ^{:type ::schema}
        (reify
          Schema
          (-type [_] type)
          (-type-properties [_])
          (-validator [_]
            (let [validator (-validator schema)]
              (fn [x] (and (fpred x)
                           (validate-limits x)
                           (reduce (fn [acc v] (if (validator v) acc (reduced false))) true x)))))
          (-explainer [this path]
            (let [explainer (-explainer schema (conj path 0))]
              (fn [x in acc]
                (cond
                  (not (fpred x)) (conj acc (-error path in this x ::invalid-type))
                  (not (validate-limits x)) (conj acc (-error path in this x ::limits))
                  :else (let [size (count x)]
                          (loop [acc acc, i 0, [x & xs] x]
                            (if (< i size)
                              (cond-> (or (explainer x (conj in (fin i x)) acc) acc) xs (recur (inc i) xs))
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
          (-walk [this walker path options]
            (if (-accept walker this path options)
              (-outer walker this path [(-inner walker schema (conj path ::in) options)] options)))
          (-properties [_] properties)
          (-options [_] options)
          (-children [_] children)
          (-form [_] form)
          LensSchema
          (-keep [_] true)
          (-get [_ _ _] schema)
          (-set [_ _ value] (into-schema type properties [value])))))))

(defn -tuple-schema []
  ^{:type ::into-schema}
  (reify IntoSchema
    (-into-schema [_ properties children options]
      (let [children (mapv #(schema % options) children)
            size (count children)
            form (-create-form :tuple properties (map -form children))]
        (-check-children! :tuple properties children {:min 1})
        ^{:type ::schema}
        (reify
          Schema
          (-type [_] :tuple)
          (-type-properties [_])
          (-validator [_]
            (let [validators (into (array-map) (map-indexed vector (mapv -validator children)))]
              (fn [x] (and (vector? x)
                           (= (count x) size)
                           (reduce-kv
                             (fn [acc i validator]
                               (if (validator (nth x i)) acc (reduced false))) true validators)))))
          (-explainer [this path]
            (let [explainers (mapv (fn [[i s]] (-explainer s (conj path i))) (map-indexed vector children))]
              (fn [x in acc]
                (cond
                  (not (vector? x)) (conj acc (-error path in this x ::invalid-type))
                  (not= (count x) size) (conj acc (-error path in this x ::tuple-size))
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
                                apply->children #(reduce-kv -update % ->children)]
                            (-chain phase [->this (-guard vector? apply->children)])))]
              {:enter (build :enter)
               :leave (build :leave)}))
          (-walk [this walker path options]
            (if (-accept walker this path options)
              (-outer walker this path (-inner-indexed walker path children options) options)))
          (-properties [_] properties)
          (-options [_] options)
          (-children [_] children)
          (-form [_] form)
          LensSchema
          (-keep [_] true)
          (-get [_ key default] (get children key default))
          (-set [_ key value] (into-schema :tuple properties (assoc children key value))))))))

(defn -enum-schema []
  ^{:type ::into-schema}
  (reify IntoSchema
    (-into-schema [_ properties children options]
      (-check-children! :enum properties children {:min 1})
      (let [children (vec children)
            schema (set children)
            form (-create-form :enum properties children)]
        ^{:type ::schema}
        (reify
          Schema
          (-type [_] :enum)
          (-type-properties [_])
          (-validator [_]
            (fn [x] (contains? schema x)))
          (-explainer [this path]
            (fn explain [x in acc]
              (if-not (contains? schema x) (conj acc (-error (conj path 0) in this x)) acc)))
          ;; TODO: should we try to derive the type from values? e.g. [:enum 1 2] ~> int?
          (-transformer [this transformer method options]
            (-value-transformer transformer this method options))
          (-walk [this walker path options]
            (if (-accept walker this path options)
              (-outer walker this path children options)))
          (-properties [_] properties)
          (-options [_] options)
          (-children [_] children)
          (-form [_] form)
          LensSchema
          (-keep [_])
          (-get [_ key default] (get children key default))
          (-set [_ key value] (into-schema :enum properties (assoc children key value))))))))

(defn -re-schema [class?]
  ^{:type ::into-schema}
  (reify IntoSchema
    (-into-schema [_ properties [child :as children] options]
      (-check-children! :re properties children {:min 1, :max 1})
      (let [children (vec children)
            re (re-pattern child)
            form (if class? re (-create-form :re properties children))]
        ^{:type ::schema}
        (reify
          Schema
          (-type [_] :re)
          (-type-properties [_])
          (-validator [_]
            (-safe-pred #(boolean (re-find re %))))
          (-explainer [this path]
            (fn explain [x in acc]
              (try
                (if-not (re-find re x)
                  (conj acc (-error path in this x))
                  acc)
                (catch #?(:clj Exception, :cljs js/Error) e
                  (conj acc (-error path in this x (:type (ex-data e))))))))
          (-transformer [this transformer method options]
            (-value-transformer transformer this method options))
          (-walk [this walker path options]
            (if (-accept walker this path options)
              (-outer walker this path children options)))
          (-properties [_] properties)
          (-options [_] options)
          (-children [_] children)
          (-form [_] form)
          LensSchema
          (-keep [_])
          (-get [_ key default] (get children key default))
          (-set [_ key value] (into-schema :re properties (assoc children key value))))))))

(defn -fn-schema []
  ^{:type ::into-schema}
  (reify IntoSchema
    (-into-schema [_ properties children options]
      (-check-children! :fn properties children {:min 1, :max 1})
      (let [children (vec children)
            f (eval (first children) options)
            form (-create-form :fn properties children)]
        ^{:type ::schema}
        (reify
          Schema
          (-type [_] :fn)
          (-type-properties [_])
          (-validator [_]
            (fn [x] (try (f x) (catch #?(:clj Exception, :cljs js/Error) _ false))))
          (-explainer [this path]
            (fn explain [x in acc]
              (try
                (if-not (f x)
                  (conj acc (-error path in this x))
                  acc)
                (catch #?(:clj Exception, :cljs js/Error) e
                  (conj acc (-error path in this x (:type (ex-data e))))))))
          (-transformer [this transformer method options]
            (-value-transformer transformer this method options))
          (-walk [this walker path options]
            (if (-accept walker this path options)
              (-outer walker this path children options)))
          (-properties [_] properties)
          (-options [_] options)
          (-children [_] children)
          (-form [_] form)
          LensSchema
          (-keep [_])
          (-get [_ key default] (get children key default))
          (-set [_ key value] (into-schema :fn properties (assoc children key value))))))))

(defn -maybe-schema []
  ^{:type ::into-schema}
  (reify IntoSchema
    (-into-schema [_ properties children options]
      (-check-children! :maybe properties children {:min 1, :max 1})
      (let [[schema :as children] (map #(schema % options) children)
            form (-create-form :maybe properties (map -form children))]
        ^{:type ::schema}
        (reify
          Schema
          (-type [_] :maybe)
          (-type-properties [_])
          (-validator [_]
            (let [validator' (-validator schema)]
              (fn [x] (or (nil? x) (validator' x)))))
          (-explainer [_ path]
            (let [explainer' (-explainer schema (conj path 0))]
              (fn explain [x in acc]
                (if (nil? x) acc (explainer' x in acc)))))
          (-transformer [this transformer method options]
            (-parent-children-transformer this children transformer method options))
          (-walk [this walker path options]
            (if (-accept walker this path options)
              (-outer walker this path (-inner-indexed walker path children options) options)))
          (-properties [_] properties)
          (-options [_] options)
          (-children [_] children)
          (-form [_] form)
          LensSchema
          (-keep [_])
          (-get [_ key default] (if (= 0 key) schema default))
          (-set [this key value] (if (= 0 key)
                                   (into-schema :maybe properties [value])
                                   (-fail! ::index-out-of-bounds {:schema this, :key key}))))))))

(defn -multi-schema
  ([]
   (-multi-schema {:naked-keys true}))
  ([opts]
   ^{:type ::into-schema}
   (reify IntoSchema
     (-into-schema [_ properties children options]
       (let [type (or (:type opts) :multi)
             opts (merge opts (select-keys properties [:lazy-refs]))
             {:keys [children entries forms]} (-parse-entries children opts options)
             form (-create-form type properties forms)
             dispatch (eval (:dispatch properties) options)
             dispatch-map (->> (for [[k s] entries] [k s]) (into {}))]
         (when-not dispatch
           (-fail! ::missing-property {:key :dispatch}))
         ^{:type ::schema}
         (reify
           Schema
           (-type [_] type)
           (-type-properties [_] (:type-properties opts))
           (-validator [_]
             (let [validators (reduce-kv (fn [acc k s] (assoc acc k (-validator s))) {} dispatch-map)]
               (fn [x]
                 (if-let [validator (validators (dispatch x))]
                   (validator x)
                   false))))
           (-explainer [this path]
             (let [explainers (reduce (fn [acc [k s]] (assoc acc k (-explainer s (conj path k)))) {} entries)
                   ->path (if (keyword? dispatch) #(conj % dispatch) identity)]
               (fn [x in acc]
                 (if-let [explainer (explainers (dispatch x))]
                   (explainer x in acc)
                   (conj acc (-error (->path path) (->path in) this x ::invalid-dispatch-value))))))
           (-transformer [this transformer method options]
             (let [this-transformer (-value-transformer transformer this method options)
                   child-transformers (reduce-kv
                                        (fn [acc k s] (assoc acc k (-transformer s transformer method options)))
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
           (-walk [this walker path options]
             (if (-accept walker this path options)
               (-outer walker this path (-inner-entries walker path entries options) options)))
           (-properties [_] properties)
           (-options [_] options)
           (-children [_] children)
           (-form [_] form)
           MapSchema
           (-entries [_] entries)
           LensSchema
           (-keep [_])
           (-get [this key default] (-get-entries this key default))
           (-set [this key value] (-set-entries this key value))))))))

(defn -ref-schema
  ([]
   (-ref-schema nil))
  ([{:keys [lazy type-properties]}]
   ^{:type ::into-schema}
   (reify IntoSchema
     (-into-schema [_ properties [ref :as children] {::keys [allow-invalid-refs] :as options}]
       (-check-children! :ref properties children {:min 1, :max 1})
       (when-not (-reference? ref)
         (-fail! ::invalid-ref {:ref ref}))
       (let [-memoize (fn [f] (let [value (atom nil)] (fn [] (or @value) (reset! value (f)))))
             -ref (or (and lazy (-memoize (fn [] (schema (mr/-schema (-registry options) ref) options))))
                      (if-let [s (mr/-schema (-registry options) ref)] (-memoize (fn [] (schema s options))))
                      (when-not allow-invalid-refs
                        (-fail! ::invalid-ref {:type :ref, :ref ref})))
             children (vec children)
             form (-create-form :ref properties children)]
         ^{:type ::schema}
         (reify
           Schema
           (-type [_] :ref)
           (-type-properties [_] type-properties)
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
           (-walk [this walker path options]
             (let [accept (fn [] (-inner walker (-ref) (into path [0 0]) (-update options ::walked-refs #(conj (or % #{}) ref))))]
               (if (-accept walker this path options)
                 (if (or (not ((-boolean-fn (::walk-refs options false)) ref)) (contains? (::walked-refs options) ref))
                   (-outer walker this path [ref] options)
                   (-outer walker this path [(accept)] options)))))
           (-properties [_] properties)
           (-options [_] options)
           (-children [_] children)
           (-form [_] form)
           LensSchema
           (-get [_ key default] (if (= key 0) (-pointer ref (-ref) options) default))
           (-keep [_])
           (-set [this key value] (if (= key 0) (into-schema :ref properties [value])
                                                (-fail! ::index-out-of-bounds {:schema this, :key key})))
           RefSchema
           (-ref [_] ref)
           (-deref [_] (-ref))))))))

(defn -schema-schema [{:keys [id raw]}]
  ^{:type ::into-schema}
  (let [type (if (or id raw) ::schema :schema)]
    (reify IntoSchema
      (-into-schema [_ properties children options]
        (-check-children! type properties children {:min 1, :max 1})
        (let [[child :as children] (map #(schema % options) children)
              form (or (and (empty? properties) (or id (and raw (-form child))))
                       (-create-form type properties [(-form child)]))]
          ^{:type ::schema}
          (reify
            Schema
            (-type [_] type)
            (-type-properties [_])
            (-validator [_] (-validator child))
            (-explainer [_ path] (-explainer child path))
            (-transformer [this transformer method options]
              (-parent-children-transformer this children transformer method options))
            (-walk [this walker path options]
              (if (-accept walker this path options)
                (if (or (not id) ((-boolean-fn (::walk-schema-refs options false)) id))
                  (-outer walker this path (-inner-indexed walker path children options) options)
                  (-outer walker this path [id] options))))
            (-properties [_] properties)
            (-options [_] options)
            (-children [_] children)
            (-form [_] form)
            LensSchema
            (-keep [_])
            (-get [_ key default] (if (= key 0) child default))
            (-set [this key value] (if (= key 0) (into-schema type properties [value])
                                                 (-fail! ::index-out-of-bounds {:schema this, :key key})))
            RefSchema
            (-ref [_] id)
            (-deref [_] child)))))))

(defn -memoized-schema [s]
  (let [validator-fn (memoize (partial -validator s))
        explainer-fn (memoize (partial -explainer s))
        transformer-fn (memoize (partial -transformer s))]
    (reify Schema

      ;; Passthrough
      (-type [this]
        (-type s))
      (-type-properties [this]
        (-type-properties s))
      (-walk [this walker path options]
        (-walk s walker path options))
      (-properties [this]
        (-properties s))
      (-options [this]
        (-options s))
      (-children [this]
        (-children s))
      (-form [this]
        (-form s))

      ;; Actual memoization/caching
      (-validator [this]
        (validator-fn))
      (-explainer [this path]
        (explainer-fn path))
      (-transformer [this transformer method options]
        (transformer-fn transformer method options)))))

;;
;; public api
;;

(defn schema?
  "Checks if x is a Schema instance"
  [x] (-schema? x))

(defn into-schema?
  "Checks if x is a IntoSchema instance"
  [x] (-into-schema? x))

(defn into-schema
  "Creates a Schema instance out of type, optional properties map and children"
  ([type properties children]
   (into-schema type properties children nil))
  ([type properties children options]
   (let [[properties options] (-properties-and-options properties options -form)]
     (-into-schema (-schema type options) (if (seq properties) properties) children options))))

(defn schema
  "Creates a Schema object from any of the following:

   - Schema instance (just returns it)
   - IntoSchema instance
   - Schema vector syntax, e.g. [:string {:min 1}]
   - Qualified Keyword or String, using a registry lookup"
  ([?schema]
   (schema ?schema nil))
  ([?schema options]
   (cond
     (schema? ?schema) ?schema
     (into-schema? ?schema) (-into-schema ?schema nil nil options)
     (vector? ?schema) (let [[p c] (-properties-and-children (rest ?schema))]
                         (into-schema (-schema (first ?schema) options) p c options))
     :else (if-let [?schema' (and (-reference? ?schema) (-lookup ?schema options))]
             (-pointer ?schema (schema ?schema' options) options)
             (-> ?schema (-schema options) (schema options))))))

(defn memoized-schema [s]
  (-memoized-schema (-schema s)))

(defn form
  "Returns the Schema form"
  ([?schema]
   (form ?schema nil))
  ([?schema options]
   (-form (schema ?schema options))))

(defn properties
  "Returns the Schema properties"
  ([?schema]
   (properties ?schema nil))
  ([?schema options]
   (-properties (schema ?schema options))))

(defn type-properties
  "Returns the Schema type properties"
  ([?schema]
   (type-properties ?schema nil))
  ([?schema options]
   (-type-properties (schema ?schema options))))

(defn options
  "Returns options used in creating the Schema"
  ([?schema]
   (options ?schema nil))
  ([?schema options]
   (-options (schema ?schema options))))

(defn children
  "Returns the Schema children with all Child Schemas resolved. For
  `MapEntry` Schemas, returns a always tuple3 of `key ?properties child`"
  ([?schema]
   (children ?schema nil))
  ([?schema options]
   (let [schema (schema ?schema options)]
     (-children schema))))

(defn type
  "Returns the Schema type."
  ([?schema]
   (type ?schema nil))
  ([?schema options]
   (-type (schema ?schema options))))

(defn walk
  "Postwalks recursively over the Schema and it's children.
   The walker callback is a arity4 function with the following
   arguments: schema, properties, (walked) children and optins."
  ([?schema f]
   (walk ?schema f nil))
  ([?schema f options]
   (-walk
     (schema ?schema options)
     (reify Walker
       (-accept [_ s _ _] s)
       (-inner [this s p options] (-walk s this p options))
       (-outer [_ s p c options] (f s p c options)))
     [] options)))

(defn validator
  "Returns an pure validation function of type `x -> boolean` for a given Schema"
  ([?schema]
   (validator ?schema nil))
  ([?schema options]
   (-validator (schema ?schema options))))

(defn validate
  "Validates a value againsta a given schema. Creates the `validator` for every call.
   When performance matters, (re-)use `validator` instead."
  ([?schema value]
   (validate ?schema value nil))
  ([?schema value options]
   ((validator ?schema options) value)))

(defn explainer
  "Returns an pure explainer function of type `x -> explanation` for a given Schema"
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
  "Explains a value againsta a given schema. Creates the `explainer` for every call.
   When performance matters, (re-)use `explainer` instead."
  ([?schema value]
   (explain ?schema value nil))
  ([?schema value options]
   ((explainer ?schema options) value [] [])))

(defn decoder
  "Creates a value decoding function given a transformer and a schema."
  ([?schema t]
   (decoder ?schema nil t))
  ([?schema options t]
   (-coder (-transformer (schema ?schema options) (-into-transformer t) :decode options))))

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
   (-coder (-transformer (schema ?schema options) (-into-transformer t) :encode options))))

(defn encode
  "Transforms a value with a given encoding transformer against a schema."
  ([?schema value t]
   (encode ?schema value nil t))
  ([?schema value options t]
   (if-let [transform (encoder ?schema options t)]
     (transform value)
     value)))

(defn entries
  "Returns `MapSchema` children as a sequence of `clojure.lang/MapEntry`s
   where the values child schemas wrapped in `:malli.core/val` Schemas,
   with the entry properties as properties.

   Using `entries` enable usage of entry properties in walking and value
   transformation.

      (def schema
        [:map
         [:x int?]
         [:y {:optional true} int?]])

      (m/children schema)
      ; [[:x nil int?]
      ;  [:y {:optional true} int?]]

      (m/entries schema)
      ; [[:x [:malli.core/val int?]]
      ;  [:y [:malli.core/val {:optional true} int?]]]

      (map key (m/entries schema))
      ; (:x :y)"
  ([?schema]
   (entries ?schema nil))
  ([?schema options]
   (if-let [schema (schema ?schema options)]
     (if (satisfies? MapSchema schema)
       (-entries schema)))))

(defn deref
  "Derefs top-level `RefSchema`s or returns original Schema."
  ([?schema]
   (deref ?schema nil))
  ([?schema options]
   (let [schema (schema ?schema options)]
     (cond-> schema (satisfies? RefSchema schema) (-deref)))))

(defn deref-all
  "Derefs top-level `RefSchema`s recursively or returns original Schema."
  ([?schema]
   (deref-all ?schema nil))
  ([?schema options]
   (let [schema (deref ?schema options)]
     (cond-> schema (satisfies? RefSchema schema) (recur options)))))

;;
;; eval
;;

(defn -default-sci-options []
  {:preset :termination-safe
   :bindings {'m/properties properties
              'm/type type
              'm/children children
              'm/entries entries}})

(let [-fail! #(-fail! ::sci-not-available {:code %})
      -eval? #(or (symbol? %) (string? %) (sequential? %))
      -evaluator (memoize ms/evaluator)]
  (defn eval
    ([?code] (eval ?code nil))
    ([?code options]
     (cond (vector? ?code) ?code
           (-eval? ?code) (if (::disable-sci options)
                            (-fail! ?code)
                            (((-evaluator (or (::sci-options options) (-default-sci-options)) -fail!)) ?code))
           :else ?code))))

;;
;; schema walker
;;

(defn schema-walker [f]
  (fn [schema _ children _]
    (f (into-schema (-type schema) (-properties schema) children (-options schema)))))

;;
;; registry
;;

(defn predicate-schemas []
  (->> [#'any? #'some? #'number? #'integer? #'int? #'pos-int? #'neg-int? #'nat-int? #'pos? #'neg? #'float? #'double?
        #'boolean? #'string? #'ident? #'simple-ident? #'qualified-ident? #'keyword? #'simple-keyword?
        #'qualified-keyword? #'symbol? #'simple-symbol? #'qualified-symbol? #'uuid? #'uri? #?(:clj #'decimal?)
        #'inst? #'seqable? #'indexed? #'map? #'vector? #'list? #'seq? #'char? #'set? #'nil? #'false? #'true?
        #'zero? #?(:clj #'rational?) #'coll? #'empty? #'associative? #'sequential? #?(:clj #'ratio?) #?(:clj #'bytes?)]
       (reduce -register-var {})))

(defn class-schemas []
  {#?(:clj Pattern, :cljs js/RegExp) (-re-schema true)})

(defn comparator-schemas []
  (->> {:> >, :>= >=, :< <, :<= <=, := =, :not= not=}
       (map (fn [[k v]] [k (-simple-schema (fn [_ [child]] {:type k, :pred (-safe-pred #(v % child)), :min 1, :max 1}))]))
       (into {}) (reduce-kv assoc nil)))

(defn type-schemas []
  {:string (-string-schema)
   :int (-int-schema)
   :double (-double-schema)
   :boolean (-boolean-schema)
   :keyword (-keyword-schema)
   :symbol (-symbol-schema)
   :qualified-keyword (-qualified-keyword-schema)
   :qualified-symbol (-qualified-symbol-schema)
   :uuid (-uuid-schema)})

(defn base-schemas []
  {:and (-and-schema)
   :or (-or-schema)
   :map (-map-schema)
   :map-of (-map-of-schema)
   :vector (-collection-schema {:type :vector, :pred vector?, :empty []})
   :sequential (-collection-schema {:type :sequential, :pred sequential?})
   :set (-collection-schema {:type :set, :pred set?, :empty #{}, :in (fn [_ x] x)})
   :enum (-enum-schema)
   :maybe (-maybe-schema)
   :tuple (-tuple-schema)
   :multi (-multi-schema)
   :re (-re-schema false)
   :fn (-fn-schema)
   :ref (-ref-schema)
   :schema (-schema-schema nil)
   ::schema (-schema-schema {:raw true})})

(defn default-schemas []
  (merge (predicate-schemas) (class-schemas) (comparator-schemas) (type-schemas) (base-schemas)))

(def default-registry
  (mr/registry (cond (identical? mr/type "default") (default-schemas)
                     (identical? mr/type "custom") (mr/custom-default-registry)
                     :else (-fail! ::invalid-registry.type {:type mr/type}))))
