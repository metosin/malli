(ns malli.core
  (:refer-clojure :exclude [-name eval name merge])
  (:require [sci.core :as sci]
            [edamame.core :as edamame])
  #?(:clj (:import (java.util.regex Pattern))))

;;
;; protocols and records
;;

(defprotocol IntoSchema
  (-into-schema [this properties childs opts] "creates a new schema instance"))

(defprotocol Schema
  (-name [this] "returns name of the schema")
  (-validator [this] "returns a predicate function that checks if the schema is valid")
  (-explainer [this path] "returns a function of `x in acc -> maybe errors` to explain the errors for invalid values")
  (-transformer [this transformer] "returns a function of `x -> y` to transform values with the given transformer")
  (-accept [this visitor opts] "accepts the visitor to visit schema and it's childs")
  (-properties [this] "returns original schema properties")
  (-form [this] "returns original form of the schema"))

(defprotocol Transformer
  (-transformer-name [this] "name of the transformer")
  (-transformer-options [this] "returns transformer options")
  (-value-transformer [this schema] "returns a function to transform value for the given schema"))

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
(declare default-registry)

(defn keyword->string [x]
  (if (keyword? x)
    (if-let [nn (namespace x)]
      (str nn "/" (clojure.core/name x))
      (clojure.core/name x))
    x))

(defn eval [?code]
  (if (fn? ?code) ?code (sci/eval-string (str ?code) {:preset :termination-safe})))

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

(defn create-form [name properties childs]
  (cond
    (and (seq properties) (seq childs)) (into [name properties] childs)
    (seq properties) [name properties]
    (seq childs) (into [name] childs)
    :else name))

(defn- -leaf-schema [name ->validator-and-childs]
  ^{:type ::into-schema}
  (reify IntoSchema
    (-into-schema [_ properties childs opts]
      (let [[validator childs] (->validator-and-childs properties childs opts)
            form (create-form name properties childs)]
        ^{:type ::schema}
        (reify
          Schema
          (-name [_] name)
          (-validator [_] validator)
          (-explainer [this path]
            (fn [value in acc]
              (if-not (validator value) (conj acc (error path in this value)) acc)))
          (-transformer [this transformer]
            (-value-transformer transformer this))
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
    (-into-schema [_ properties childs opts]
      (when-not (seq childs)
        (fail! ::no-childs {:name name, :properties properties}))
      (let [child-schemas (mapv #(schema % opts) childs)
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

(defn- -optional-entry? [[_ ?p]]
  (boolean (and (map? ?p) (true? (:optional ?p)))))

(defn- -optional-entry [[k ?p s :as entry] optional]
  (if (map? ?p)
    (cond
      optional (update entry 1 assoc :optional true)
      (= [:optional] (keys ?p)) [k s]
      :else (update entry 1 dissoc :optional))
    (if optional [k {:optional true} ?p] [k ?p])))

(defn- -expand-key [[k ?p ?v] opts f]
  (let [[p v] (if (map? ?p) [?p ?v] [nil ?p])
        v' (f (schema v opts))
        [k' p'] (if (vector? k)
                  (let [[r k'] k]
                    [k' (assoc p :optional (case r :opt true, :req false))])
                  [k p])]
    [k' p' v']))

(defn -parse-keys [childs opts]
  (let [entries (mapv #(-expand-key % opts identity) childs)]
    {:required (->> entries (filter (comp not :optional second)) (mapv first))
     :optional (->> entries (filter (comp :optional second)) (mapv first))
     :keys (->> entries (mapv first))
     :entries entries
     :forms (mapv (fn [[k p v]]
                    (let [v' (-form v)]
                      (if p [k p v'] [k v']))) entries)}))

(defn- -map-schema []
  ^{:type ::into-schema}
  (reify IntoSchema
    (-into-schema [_ properties childs opts]
      (let [{:keys [entries forms]} (-parse-keys childs opts)
            form (create-form :map properties forms)]
        ^{:type ::schema}
        (reify Schema
          (-name [_] :map)
          (-validator [_]
            (let [validators (mapv
                               (fn [[key {:keys [optional]} value]]
                                 (let [valid? (-validator value)
                                       default (boolean optional)]
                                   (fn [m] (if-let [map-entry (find m key)] (valid? (val map-entry)) default))))
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
                               (fn [[i [key {:keys [optional] :as key-properties} schema]]]
                                 (let [key-distance (if (seq key-properties) 2 1)
                                       explainer (-explainer schema (into path [(+ i distance) key-distance]))
                                       key-path (into path [(+ i distance) 0])]
                                   (fn [x in acc]
                                     (if-let [v (key x)]
                                       (explainer v (conj in key) acc)
                                       (if-not optional
                                         (conj acc (error key-path (conj in key) this nil ::missing-key)))))))
                               (map-indexed vector entries))]
              (fn [x in acc]
                (if-not (map? x)
                  (conj acc (error path in this x ::invalid-type))
                  (reduce
                    (fn [acc explainer]
                      (explainer x in acc))
                    acc explainers)))))
          (-transformer [this transformer]
            (let [transformers (some->> entries
                                        (mapcat (fn [[k _ s]] (if-let [t (-transformer s transformer)] [k t])))
                                        (seq)
                                        (apply array-map))
                  map-transformer (-value-transformer transformer this)]
              (cond
                (and (not transformers) (not map-transformer))
                nil

                (and (not transformers) map-transformer)
                (fn [x]
                  (if (map? x)
                    (map-transformer x)
                    x))

                (and transformers (not map-transformer))
                (fn [x]
                  (if (map? x)
                    (reduce-kv (fn [acc k t]
                                 (if-let [entry (find x k)]
                                   (assoc acc k (t (val entry)))
                                   acc))
                               x transformers)
                    x))

                :else
                (fn [x]
                  (if (map? x)
                    (reduce-kv (fn [acc k t]
                                 (if-let [entry (find x k)]
                                   (assoc acc k (t (val entry)))
                                   acc))
                               (map-transformer x) transformers)
                    x)))))
          (-accept [this visitor opts]
            (visitor this (->> entries (map last) (mapv #(-accept % visitor opts))) opts))
          (-properties [_] properties)
          (-form [_] form))))))

(defn- -map-of-schema []
  ^{:type ::into-schema}
  (reify IntoSchema
    (-into-schema [_ properties childs opts]
      (when-not (and (seq childs) (= 2 (count childs)))
        (fail! ::child-error {:name :vector, :properties properties, :childs childs, :min 2, :max 2}))
      (let [[key-schema value-schema :as schemas] (mapv #(schema % opts) childs)
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
          (-transformer [_ transformer]
            (let [key-transformer (if-let [t (-transformer key-schema transformer)]
                                    (fn [x] (t (keyword->string x))))
                  value-transformer (-transformer value-schema transformer)]
              (cond
                (and key-transformer value-transformer)
                (fn [x]
                  (if (map? x)
                    (reduce-kv
                      (fn [acc k v]
                        (let [k' (key-transformer k)]
                          (-> acc
                              (assoc (key-transformer k) (value-transformer v))
                              (cond-> (not (identical? k' k)) (dissoc k))))) x x)
                    x))
                key-transformer
                (fn [x]
                  (if (map? x)
                    (reduce-kv
                      (fn [acc k v]
                        (let [k' (key-transformer k)]
                          (-> acc
                              (assoc (key-transformer k) v)
                              (cond-> (not (identical? k' k)) (dissoc k))))) x x)
                    x))
                value-transformer
                (fn [x]
                  (if (map? x)
                    (reduce-kv (fn [acc k v] (assoc acc k (value-transformer v))) x x)
                    x)))))
          (-accept [this visitor opts]
            (visitor this (mapv #(-accept % visitor opts) schemas) opts))
          (-properties [_] properties)
          (-form [_] (create-form :map-of properties (mapv -form schemas))))))))

(defn- -collection-schema [name fpred fwrap fempty]
  ^{:type ::into-schema}
  (reify IntoSchema
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
    (-into-schema [_ properties childs opts]
      (let [schemas (mapv #(schema % opts) childs)
            size (count schemas)
            form (create-form :tuple properties (map -form schemas))
            validators (into (array-map) (map-indexed vector (mapv -validator schemas)))]
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
          (-transformer [_ transformer]
            (let [ts (->> schemas
                          (mapv #(-transformer % transformer))
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
    (-into-schema [_ properties childs _]
      (when-not (seq childs)
        (fail! ::no-childs {:name :enum, :properties properties}))
      (let [schema (set childs)
            validator (fn [x] (contains? schema x))]
        ^{:type ::schema}
        (reify Schema
          (-name [_] :enum)
          (-validator [_] validator)
          (-explainer [this path]
            (fn explain [x in acc]
              (if-not (validator x) (conj acc (error path in this x)) acc)))
          ;; TODO: should we try to derive the type from values? e.g. [:enum 1 2] ~> int?
          (-transformer [_ _])
          (-accept [this visitor opts] (visitor this (vec childs) opts))
          (-properties [_] properties)
          (-form [_] (create-form :enum properties childs)))))))

(defn- -re-schema [class?]
  ^{:type ::into-schema}
  (reify IntoSchema
    (-into-schema [_ properties [child :as childs] _]
      (when-not (= 1 (count childs))
        (fail! ::child-error {:name :re, :properties properties, :childs childs, :min 1, :max 1}))
      (let [re (re-pattern child)
            validator (fn [x] (try (boolean (re-find re x)) (catch #?(:clj Exception, :cljs js/Error) _ false)))
            form (if class? re (create-form :re properties childs))]
        ^{:type ::schema}
        (reify Schema
          (-name [_] :re)
          (-validator [_] validator)
          (-explainer [this path]
            (fn explain [x in acc]
              (try
                (if-not (re-find re x)
                  (conj acc (error path in this x)))
                (catch #?(:clj Exception, :cljs js/Error) e
                  (conj acc (error path in this x (:type (ex-data e))))))))
          (-transformer [_ _])
          (-accept [this visitor opts] (visitor this [] opts))
          (-properties [_] properties)
          (-form [_] form))))))

(defn- -fn-schema []
  ^{:type ::into-schema}
  (reify IntoSchema
    (-into-schema [_ properties childs _]
      (when-not (= 1 (count childs))
        (fail! ::child-error {:name :fn, :properties properties, :childs childs, :min 1, :max 1}))
      (let [f (eval (first childs))
            validator (fn [x] (try (f x) (catch #?(:clj Exception, :cljs js/Error) _ false)))]
        ^{:type ::schema}
        (reify Schema
          (-name [_] :fn)
          (-validator [_] validator)
          (-explainer [this path]
            (fn explain [x in acc]
              (try
                (if-not (f x)
                  (conj acc (error path in this x)))
                (catch #?(:clj Exception, :cljs js/Error) e
                  (conj acc (error path in this x (:type (ex-data e))))))))
          (-transformer [_ _])
          (-accept [this visitor opts] (visitor this [] opts))
          (-properties [_] properties)
          (-form [_] (create-form :fn properties childs)))))))

(defn- -maybe-schema []
  ^{:type ::into-schema}
  (reify IntoSchema
    (-into-schema [_ properties childs opts]
      (when-not (= 1 (count childs))
        (fail! ::child-error {:name :vector, :properties properties, :childs childs, :min 1, :max 1}))
      (let [schema' (-> childs first (schema opts))
            validator' (-validator schema')
            form (create-form :maybe properties [(-form schema')])]
        ^{:type ::schema}
        (reify Schema
          (-name [_] :maybe)
          (-validator [_] (fn [x] (or (nil? x) (validator' x))))
          (-explainer [this path]
            (fn explain [x in acc]
              (if-not (or (nil? x) (validator' x)) (conj acc (error path in this x)))))
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
   (let [-get #(or (get registry %) (some-> registry (get (type %)) (-into-schema nil [%] opts)))]
     (cond
       (schema? ?schema) ?schema
       (satisfies? IntoSchema ?schema) (-into-schema ?schema nil nil opts)
       (vector? ?schema) (apply -into-schema (concat [(-get (first ?schema))]
                                                     (-properties-and-childs (rest ?schema)) [opts]))
       :else (or (some-> ?schema -get schema) (fail! ::invalid-schema {:schema ?schema}))))))

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

(defn childs
  ([?schema]
   (childs ?schema nil))
  ([?schema opts]
   (let [schema (schema ?schema opts)
         form (-form schema)]
     (if (vector? form)
       (->> form (drop (if (-properties schema) 2 1)))))))

(defn name
  ([?schema]
   (name ?schema nil))
  ([?schema opts]
   (-name (schema ?schema opts))))

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
  "Creates a value transformer given a transformer and a schema."
  ([?schema t]
   (transformer ?schema nil t))
  ([?schema opts t]
   (or (-transformer (schema ?schema opts) t) identity)))

(defn transform
  "Transforms a value with a given transformer agains a schema."
  ([?schema value t]
   (transform ?schema value nil t))
  ([?schema value opts t]
   (if-let [transform (transformer (schema ?schema) opts t)]
     (transform value)
     value)))

(defn merge
  "Deep-merges two schemas into one with the following rules:

  * if either schemas is `nil`, the other one is used, regardless of value
  * with two :map schemas, both keys and values are merged
  * with any other schemas, 2-arity `:malli.core/merge` function is used, defaulting
    to constantly schema2"
  ([?schema1 ?schema2]
   (merge ?schema1 ?schema2 nil))
  ([?schema1 ?schema2 opts]
   (let [[schema1 schema2 :as schemas] [(if ?schema1 (schema ?schema1 opts))
                                        (if ?schema2 (schema ?schema2 opts))]
         merge' (::merge opts (constantly schema2))]
     (cond
       (not schema1) schema2
       (not schema2) schema1
       (not= :map (name schema1) (name schema2)) (merge' schema1 schema2)
       :else (let [p (clojure.core/merge (properties schema1) (properties schema2))]
               (-> [:map]
                   (cond-> p (conj p))
                   (into (:form
                           (reduce
                             (fn [{:keys [keys] :as acc} [k :as f]]
                               (if (keys k)
                                 (->> (reduce
                                        (fn [acc' [k' :as f']]
                                          (if-not (= k k')
                                            (conj acc' f')
                                            (let [f'' (-optional-entry
                                                        (if (= k k') f f')
                                                        (and (-optional-entry? f) (-optional-entry? f')))]
                                              (conj acc' (conj (pop f'') (merge (peek f') (peek f) opts))))))
                                        [] (:form acc))
                                      (assoc acc :form))
                                 (-> acc
                                     (update :form conj f)
                                     (update :keys conj k))))
                             {:keys #{}, :form []}
                             (mapcat #(-> % (childs opts) (-parse-keys opts) :forms) schemas))))
                   (schema opts)))))))

(defn serialize
  ([?schema]
   (serialize ?schema nil))
  ([?schema opts]
   (pr-str (form ?schema opts))))

(defn deserialize
  ([form]
   (deserialize form nil))
  ([form opts]
   (schema (edamame/parse-string form {:dispatch {\# {\" #(re-pattern %)}}}) opts)))

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
   :set (-collection-schema :set set? set #{})
   :enum (-enum-schema)
   :maybe (-maybe-schema)
   :tuple (-tuple-schema)
   :re (-re-schema false)
   :fn (-fn-schema)})

(def default-registry
  (clojure.core/merge predicate-registry class-registry comparator-registry base-registry))
