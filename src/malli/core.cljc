(ns malli.core
  (:refer-clojure :exclude [-name eval name])
  (:require [sci.core :as sci]))

;;
;; protocols
;;

(defprotocol IntoSchema
  (-name [this] "returns name of the schema")
  (-into-schema [this properties childs opts] "creates a new schema instance"))

(defprotocol Schema
  (-validator [this] "returns a predicate function that checks if the schema is valid")
  (-explainer [this path] "returns a function of `x in acc -> maybe errors` to explain the errors for invalid values")
  (-transformer [this transformer] "returns a function of `x -> y` to transform values with the given transformer")
  (-accept [this visitor opts] "accepts the visitor to visit schema and it's childs")
  (-properties [this] "returns original schema properties")
  (-form [this] "returns original form of the schema"))

#?(:clj (defmethod print-method ::into-schema [v ^java.io.Writer w]
          (.write w (str "#IntoSchema{:name " (-name v) "}"))))

#?(:clj (defmethod print-method ::schema [v ^java.io.Writer w]
          (.write w (str (-form v)))))

;;
;; impl
;;

(declare schema)
(declare default-registry)

(defn keyword->string [x]
  (if (keyword? x)
    (if-let [nn (namespace x)]
      (str nn "/" (clojure.core/name x))
      (clojure.core/name x))
    x))

(defn eval [code]
  (sci/eval-string (str code) {:realize-max 100}))

(defn fail!
  ([type]
   (fail! type nil))
  ([type data]
   (throw (ex-info (str type) {:type type, :data data}))))

(defn create-form [name properties childs]
  (cond
    (and (seq properties) (seq childs)) (into [name properties] childs)
    (seq properties) [name properties]
    (seq childs) (into [name] childs)
    :else name))

(defn- -leaf-schema [name ->validator-and-childs]
  ^{:type ::into-schema}
  (reify IntoSchema
    (-name [_] name)
    (-into-schema [_ properties childs opts]
      (let [[validator childs] (->validator-and-childs properties childs opts)
            form (create-form name properties childs)]
        ^{:type ::schema}
        (reify
          Schema
          (-validator [_] validator)
          (-explainer [this path]
            (fn [value in acc]
              (if-not (validator value)
                (conj acc {:path path
                           :in in
                           :schema this
                           :value value})
                acc)))
          (-transformer [this transformer]
            (transformer this))
          (-accept [this visitor opts] (visitor this (vec childs) opts))
          (-properties [_] properties)
          (-form [_] form))))))

(defn fn-schema [name f]
  (-leaf-schema
    name
    (fn [properties childs _]
      (when (seq childs)
        (fail! ::child-error {:name name, :properties properties, :childs childs, :min 0, :max 0}))
      [f childs])))

(defn- -partial-fn-schema [name f]
  (-leaf-schema
    name
    (fn [properties [child :as childs] _]
      (when-not (= 1 (count childs))
        (fail! ::child-error {:name name, :properties properties, :childs childs, :min 1, :max 1}))
      [#(try (f % child) (catch #?(:clj Exception, :cljs js/Error) _ false)) childs])))

(defn- -composite-schema [name f short-circuit]
  ^{:type ::into-schema}
  (reify IntoSchema
    (-name [_] name)
    (-into-schema [_ properties childs opts]
      (when-not (seq childs)
        (fail! ::no-childs {:name name, :properties properties}))
      (let [child-schemas (mapv #(schema % opts) childs)
            validators (distinct (map -validator child-schemas))
            validator (apply f validators)]
        ^{:type ::schema}
        (reify Schema
          (-validator [_] validator)
          (-explainer [_ path]
            (let [distance (if (seq properties) 2 1)
                  explainers (mapv (fn [[i c]] (-explainer c (into path [(+ i distance)]))) (map-indexed vector child-schemas))]
              (fn explain [x in acc]
                (reduce
                  (fn [acc' explainer]
                    (let [acc'' (explainer x in acc')]
                      (if (and short-circuit (identical? acc' acc''))
                        (reduced acc)
                        acc'')))
                  acc explainers))))
          (-transformer [_ transformer]
            (let [tvs (into [] (keep #(-transformer % transformer) child-schemas))]
              (if (seq tvs)
                (if short-circuit
                  (fn [x]
                    (reduce-kv
                      (fn [_ _ t] (let [x' (t x)] (if-not (identical? x' x) (reduced x') x)))
                      x tvs))
                  (fn [x]
                    (reduce-kv
                      (fn [x' _ t] (t x'))
                      x tvs))))))
          (-accept [this visitor opts]
            (visitor this (mapv #(-accept % visitor opts) child-schemas) opts))
          (-properties [_] properties)
          (-form [_] (create-form name properties (map -form child-schemas))))))))

(defn- -properties-and-childs [xs]
  (if (map? (first xs))
    [(first xs) (rest xs)]
    [nil xs]))

(defn- -valid-schema? [schema?]
  (try
    (schema schema?)
    true
    (catch Exception _ false)))

(defn- -is-pred-key?
  [entry]
  (-> entry meta :pred-key?))

(defn- -expand-key [[k ?p ?v] opts f]
  (let [pred-key? (-valid-schema? k)
        [p v] (if (map? ?p) [?p ?v] [nil ?p])
        v' (f (schema v opts))
        [k' p'] (if (and (vector? k) (not pred-key?))
                  (let [[r k'] k]
                    [k' (assoc p :optional (case r :opt true, :req false))])
                  [k p])]
    (if pred-key?
      (with-meta [(f (schema k' opts)) p' v'] {:pred-key? true})
      [k' p' v'])))

(defn -parse-keys [childs opts]
  (let [entries (mapv #(-expand-key % opts identity) childs)]
    {:required (->> entries (filter (comp not :optional second)) (mapv first))
     :optional (->> entries (filter (comp :optional second)) (mapv first))
     :keys (->> entries (filter (comp not :pred-key? meta)) (mapv first))
     :entries entries
     :forms (mapv (fn [[k p v :as entry]]
                    (let [k' (if (-is-pred-key? entry)
                               (-form k)
                               k)
                          v' (-form v)]
                      (if p [k' p v'] [k' v']))) entries)}))

(defn- -map-schema []
  ^{:type ::into-schema}
  (reify IntoSchema
    (-name [_] :map)
    (-into-schema [_ properties childs opts]
      (let [{:keys [entries forms keys]} (-parse-keys childs opts)
            form (create-form :map properties forms)
            dissoc-schema-keys (fn [m] (apply dissoc m keys))]
        ^{:type ::schema}
        (reify Schema
          (-validator [_]
            (let [validators (mapv
                               (fn [[key {:keys [optional]} value :as entry]]
                                 (let [key-valid? (if (-is-pred-key? entry) (-validator key))
                                       value-valid? (-validator value)
                                       default (boolean optional)]
                                   (fn [m]
                                     (if (-is-pred-key? entry)
                                       (reduce-kv
                                         (fn [_ k v]
                                           (or (and (key-valid? k) (value-valid? v)) (reduced false)))
                                         true (dissoc-schema-keys m))
                                       (if-let [map-entry (find m key)]
                                         (value-valid? (val map-entry))
                                         default)))))
                               entries)
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
                  explainers (mapv
                               (fn [[i [key {:keys [optional] :as key-properties} schema :as entry]]]
                                 (let [key-distance 0
                                       schema-distance (if (and (seq key-properties)) 2 1)
                                       default-path (conj path (+ i distance))
                                       key-explainer (if (-is-pred-key? entry) (-explainer key (conj default-path key-distance)))
                                       schema-explainer (-explainer schema (conj default-path schema-distance))]
                                   (fn [x in acc]
                                     (if (-is-pred-key? entry)
                                       (reduce-kv
                                         (fn [acc k v]
                                           (let [in (conj in k)]
                                             (->> acc
                                                  (key-explainer k in)
                                                  (schema-explainer v in))))
                                         acc (dissoc-schema-keys x))
                                       (if-let [v (key x)]
                                         (schema-explainer v (conj in key) acc)
                                         (if-not optional
                                           (conj acc {:path path
                                                      :in in
                                                      :schema this
                                                      :type ::missing-key
                                                      ::key key})
                                           acc))))))
                               (map-indexed vector entries))]
              (fn [x in acc]
                (if-not (map? x)
                  (conj acc {:path path
                             :in in
                             :schema this
                             :value x
                             :type ::invalid-type})
                  (reduce
                    (fn [acc explainer]
                      (explainer x in acc))
                    acc explainers)))))
          (-transformer [_ transformer]
            (let [transformers (->> entries
                                    (mapcat
                                      (fn [[k _ s :as entry]]
                                        (if (-is-pred-key? entry)
                                          [::pred-key [(-transformer k transformer)
                                                       (-transformer s transformer)]]
                                          (if-let [t (-transformer s transformer)]
                                            [k t]))))
                                    (apply array-map))]
              (if (seq transformers)
                (fn [x]
                  (if (map? x)
                    (reduce-kv
                      (fn [acc k v]
                        (let [t (get transformers k)]
                          (if t
                            (assoc acc k (t v))
                            (if-let [[key-transformer value-transformer] (get transformers ::pred-key)]
                              (cond
                                (and key-transformer value-transformer)
                                (let [k' (key-transformer k)]
                                  (-> acc
                                      (assoc k' (value-transformer v))
                                      (cond-> (not (identical? k' k)) (dissoc k))))

                                key-transformer
                                (let [k' (key-transformer k)]
                                  (-> acc
                                      (assoc k' v)
                                      (cond-> (not (identical? k' k)) (dissoc k))))

                                value-transformer
                                (assoc acc k (value-transformer v))

                                :else acc)

                              acc)))) x x)
                    x)))))
          (-accept [this visitor opts]
            (visitor this
                     (->> entries
                          (mapv (fn [[k _ v :as entry]]
                                  (if (-is-pred-key? entry)
                                    (mapv #(-accept % visitor opts) [k v])
                                    (-accept v visitor opts)))))
                     opts))
          (-properties [_] properties)
          (-form [_] form))))))

(defn- -collection-schema [name fpred fwrap fempty]
  ^{:type ::into-schema}
  (reify IntoSchema
    (-name [_] name)
    (-into-schema [_ {:keys [min max] :as properties} childs opts]
      (when-not (= 1 (count childs))
        (fail! ::child-error {:name :vector, :properties properties, :childs childs, :min 1, :max 1}))
      (let [schema (schema (first childs) opts)
            form (create-form name properties [(-form schema)])
            validate-limits (cond
                              (not (or min max)) (constantly true)
                              (and min max) (fn [x] (let [size (count x)] (<= min size max)))
                              min (fn [x] (let [size (count x)] (<= min size)))
                              max (fn [x] (let [size (count x)] (<= size max))))]
        ^{:type ::schema}
        (reify Schema
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
                  (not (fpred x))
                  (conj acc {:path path
                             :in in
                             :type ::invalid-type
                             :schema this
                             :value x})
                  (not (validate-limits x))
                  (conj acc {:path path
                             :in in
                             :type ::limits
                             :schema this
                             :value x})
                  :else
                  (loop [acc acc, i 0, [x & xs] x]
                    (cond-> (explainer x (conj in i) acc) xs (recur (inc i) xs)))))))
          (-transformer [_ transformer]
            (if-let [t (-transformer schema transformer)]
              (if fempty
                (fn [x]
                  (try
                    (persistent! (reduce (fn [v o] (conj! v (t o))) (transient fempty) x))
                    (catch #?(:clj Exception, :cljs js/Error) _ x)))
                (fn [x]
                  (try
                    (map t x)
                    (catch #?(:clj Exception, :cljs js/Error) _ x))))
              ;; should wrapping be optional?
              fwrap))
          (-accept [this visitor opts] (visitor this [(-accept schema visitor opts)] opts))
          (-properties [_] properties)
          (-form [_] form))))))

(defn- -tuple-schema []
  ^{:type ::into-schema}
  (reify IntoSchema
    (-name [_] :tuple)
    (-into-schema [_ properties childs opts]
      (let [schemas (mapv #(schema % opts) childs)
            size (count schemas)
            form (create-form :tuple properties (map -form schemas))
            validators (into (array-map) (map-indexed vector (mapv -validator schemas)))]
        ^{:type ::schema}
        (reify Schema
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
                  (not (vector? x))
                  (conj acc {:path path
                             :in in
                             :type ::invalid-type
                             :schema this
                             :value x})
                  (not= (count x) size)
                  (conj acc {:path path
                             :in in
                             :type ::tuple-size
                             :schema this
                             :value x})
                  :else
                  (loop [acc acc, i 0, [x & xs] x, [e & es] explainers]
                    (cond-> (e x (conj in i) acc) xs (recur (inc i) xs es)))))))
          (-transformer [_ transformer]
            (let [ts (->> schemas
                          (mapv transformer)
                          (map-indexed vector)
                          (filter second)
                          (mapcat identity)
                          (apply array-map))]
              (fn [x] (if (vector? x) (reduce-kv (fn [acc i t] (update acc i t)) (vec x) ts) x))))
          (-accept [this visitor opts] (visitor this (mapv #(-accept % visitor opts) schemas) opts))
          (-properties [_] properties)
          (-form [_] form))))))

(defn- -enum-schema []
  ^{:type ::into-schema}
  (reify IntoSchema
    (-name [_] :enum)
    (-into-schema [_ properties childs _]
      (when-not (seq childs)
        (fail! ::no-childs {:name :enum, :properties properties}))
      (let [schema (set childs)
            validator (fn [x] (contains? schema x))]
        ^{:type ::schema}
        (reify Schema
          (-validator [_] validator)
          (-explainer [this path]
            (fn explain [x in acc]
              (if-not (validator x)
                (conj acc {:path path
                           :in in
                           :schema this
                           :value x}))))
          ;; TODO: should we try to derive the type from values? e.g. [:enum 1 2] ~> int?
          (-transformer [_ _])
          (-accept [this visitor opts] (visitor this (vec childs) opts))
          (-properties [_] properties)
          (-form [_] (create-form :enum properties childs)))))))

(defn- -fn-schema []
  ^{:type ::into-schema}
  (reify IntoSchema
    (-name [_] :fn)
    (-into-schema [_ properties childs _]
      (when-not (= 1 (count childs))
        (fail! ::child-error {:name :vector, :properties properties, :childs childs, :min 1, :max 1}))
      (let [f (eval (first childs))
            validator (fn [x] (try (f x) (catch #?(:clj Exception, :cljs js/Error) _ false)))]
        ^{:type ::schema}
        (reify Schema
          (-validator [_] validator)
          (-explainer [this path]
            (fn explain [x in acc]
              (try
                (if-not (f x)
                  (conj acc {:path path
                             :in in
                             :schema this
                             :value x}))
                (catch #?(:clj Exception, :cljs js/Error) e
                  (let [type (:type (ex-data e))]
                    (conj acc (cond-> {:path path
                                       :in in
                                       :schema this
                                       :value x}
                                      type (assoc :type type))))))))
          (-transformer [_ _])
          (-accept [this visitor opts] (visitor this [] opts))
          (-properties [_] properties)
          (-form [_] (create-form :fn properties childs)))))))

(defn- -maybe-schema []
  ^{:type ::into-schema}
  (reify IntoSchema
    (-name [_] :maybe)
    (-into-schema [_ properties childs opts]
      (when-not (= 1 (count childs))
        (fail! ::child-error {:name :vector, :properties properties, :childs childs, :min 1, :max 1}))
      (let [schema' (-> childs first (schema opts))
            validator' (-validator schema')
            form (create-form :maybe properties [(-form schema')])]
        ^{:type ::schema}
        (reify Schema
          (-validator [_] (fn [x] (or (nil? x) (validator' x))))
          (-explainer [this path]
            (fn explain [x in acc]
              (if-not (or (nil? x) (validator' x))
                (conj acc {:path path
                           :in in
                           :schema this
                           :value x}))))
          (-transformer [_ transformer] (-transformer schema' transformer))
          (-accept [this visitor opts] (visitor this [(-accept schema' visitor opts)] opts))
          (-properties [_] properties)
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

;;
;; public api
;;

(defn schema? [x]
  (satisfies? Schema x))

(defn schema
  ([?schema]
   (schema ?schema nil))
  ([?schema {:keys [registry] :as opts :or {registry default-registry}}]
   (cond
     (schema? ?schema) ?schema
     (satisfies? IntoSchema ?schema) (-into-schema ?schema nil nil opts)
     :else (if (vector? ?schema)
             (apply -into-schema (concat [(get registry (first ?schema))]
                                         (-properties-and-childs (rest ?schema)) [opts]))
             (if-let [schema' (get registry ?schema)]
               (if (schema? schema')
                 schema'
                 (-into-schema schema' nil nil opts))
               (fail! ::invalid-schema {:schema ?schema}))))))

(defn form
  ([?schema]
   (form ?schema nil))
  ([?schema opts]
   (-form (schema ?schema opts))))

(defn accept
  ([?schema visitor]
   (accept ?schema visitor nil))
  ([?schema visitor opts]
   (-accept (schema ?schema opts) visitor opts)))

(defn properties
  ([?schema]
   (properties ?schema nil))
  ([?schema opts]
   (-properties (schema ?schema opts))))

(defn name
  ([?schema]
   (name ?schema nil))
  ([?schema opts]
   (let [form (form ?schema opts)]
     (if (vector? form) (first form) form))))

(defn validator
  ([?schema]
   (validator ?schema nil))
  ([?schema opts]
   (-validator (schema ?schema opts))))

(defn validate
  ([?schema value]
   (validate ?schema value nil))
  ([?schema value opts]
   ((validator ?schema opts) value)))

(defn explainer
  ([?schema]
   (explainer ?schema nil))
  ([?schema opts]
   (let [schema' (schema ?schema opts)
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
  ([?schema value opts]
   ((explainer ?schema opts) value [] [])))

(defn transformer
  ([?schema t]
   (transformer ?schema nil t))
  ([?schema opts t]
   (or (-transformer (schema ?schema opts) t) identity)))

(defn transform
  ([?schema value t]
   (transform ?schema value nil t))
  ([?schema value opts t]
   (if-let [transform (transformer (schema ?schema) opts t)]
     (transform value)
     value)))

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

(def comparator-registry
  (->> {:> >, :>= >=, :< <, :<= <=, := =, :not= not=}
       (map (fn [[k v]] [k (-partial-fn-schema k v)]))
       (into {})
       (reduce-kv -register nil)))

(def base-registry
  {:and (-composite-schema :and every-pred false)
   :or (-composite-schema :or some-fn true)
   :map (-map-schema)
   :vector (-collection-schema :vector vector? vec [])
   :list (-collection-schema :list list? seq nil)
   :set (-collection-schema :set set? set #{})
   :enum (-enum-schema)
   :maybe (-maybe-schema)
   :tuple (-tuple-schema)
   :fn (-fn-schema)})

(def default-registry
  (merge predicate-registry comparator-registry base-registry))
