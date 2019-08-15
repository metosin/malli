(ns malli.core
  (:refer-clojure :exclude [-name]))

;;
;; protocols
;;

(defprotocol IntoSchema
  (-name [this])
  (-into-schema [this properties childs opts]))

(defprotocol Schema
  (-validator [this])
  (-explainer [this path])
  (-transformer [this transformer])
  (-properties [this])
  (-form [this]))

#?(:clj (defmethod print-method ::into-schema [v ^java.io.Writer w]
          (.write w (str "#IntoSchema{:name " (-name v) "}"))))

#?(:clj (defmethod print-method ::schema [v ^java.io.Writer w]
          (.write w (str (-form v)))))

;;
;; impl
;;

(declare schema)
(declare default-registry)

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

(defn dispatch-name [schema]
  (let [form (-form schema)]
    (if (vector? form) (first form) form)))

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
          (-properties [_] properties)
          (-form [_] (create-form name properties (map -form child-schemas))))))))

(defn- properties-and-childs [xs]
  (if (map? (first xs))
    [(first xs) (rest xs)]
    [nil xs]))

(defn- expand-key [[k ?p ?v] opts f]
  (let [[p v] (if (map? ?p) [?p ?v] [nil ?p])
        v' (f (schema v opts))
        [k' p'] (if (vector? k)
                  (let [[r k'] k]
                    [k' (assoc p :optional (case r :opt true, :req false))])
                  [k p])]
    [k' p' v']))

(defn- parse-keys [childs opts]
  (let [entries (mapv #(expand-key % opts identity) childs)]
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
    (-name [_] :map)
    (-into-schema [_ properties childs opts]
      (when-not (seq childs)
        (fail! ::no-childs {:name :map, :properties properties}))
      (let [{:keys [entries forms]} (parse-keys childs opts)
            form (create-form :map properties forms)]
        ^{:type ::schema}
        (reify Schema
          (-validator [_]
            (let [validators (mapv
                               (fn [[key {:keys [optional]} value]]
                                 (let [valid? (-validator value)
                                       default (boolean optional)]
                                   (fn [m] (if-let [v (key m)] (valid? v) default))))
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
                               (fn [[i [key {:keys [optional] :as key-properties} value]]]
                                 (let [key-distance (if (seq key-properties) 2 1)
                                       explainer (-explainer value (into path [(+ i distance) key-distance]))]
                                   (fn [x in acc]
                                     (if-let [v (key x)]
                                       (explainer v (conj in key) acc)
                                       (if-not optional
                                         (conj acc {:path path
                                                    :in in
                                                    :schema this
                                                    :type ::missing-key
                                                    ::key key}))))))
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
                                    (mapcat (fn [[k _ s]] (if-let [t (-transformer s transformer)] [k t])))
                                    (apply array-map))]
              (if (seq transformers)
                (fn [x]
                  (if (map? x)
                    (reduce-kv (fn [acc k t] (assoc acc k (t (k x)))) x transformers)
                    x)))))
          (-properties [_] properties)
          (-form [_] form))))))

(defn- -collection-schema [name fpred fwrap fempty]
  ^{:type ::into-schema}
  (reify IntoSchema
    (-name [_] name)
    (-into-schema [_ {:keys [min max] :as properties} childs opts]
      (when-not (= 1 (count childs))
        (fail! ::child-error {:name :vector, :properties properties, :childs childs, :min 1, :max 1}))
      (let [schemas (mapv #(schema % opts) childs)
            form (create-form name properties (map -form schemas))
            validate-limits (cond
                              (not (or min max)) (constantly true)
                              (and min max) (fn [x] (let [size (count x)] (<= min size max)))
                              min (fn [x] (let [size (count x)] (<= min size)))
                              max (fn [x] (let [size (count x)] (<= size max))))]
        ^{:type ::schema}
        (reify Schema
          (-validator [_]
            (let [validator (-validator (first schemas))]
              (fn [x] (and (fpred x)
                           (validate-limits x)
                           (reduce (fn [acc v] (if (validator v) acc (reduced false))) true x)))))
          (-explainer [this path]
            (let [schema (first schemas)
                  distance (if (seq properties) 2 1)
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
          (-properties [_] properties)
          (-transformer [_ transformer]
            (if-let [t (-transformer (first schemas) transformer)]
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
          (-properties [_] properties)
          (-form [_] form))))))

(defn- -map-of-schema []
  ^{:type ::into-schema}
  (reify IntoSchema
    (-name [_] :map-of)
    (-into-schema [_ properties childs opts]
      (when-not (and (seq childs) (= 2 (count childs)))
        (fail! ::invalid-map-of))
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
          (-validator [_] (fn [m] (and (map? m) (validate m))))
          (-explainer [this path]
            (let [distance (if (seq properties) 2 1)
                  key-explainer (-explainer key-schema (conj path distance))
                  value-explainer (-explainer value-schema (conj path (inc distance)))]
              (fn explain [m in acc]
                (if-not (map? m)
                  (conj acc {:path path
                             :in in
                             :schema this
                             :value m
                             :type ::invalid-type})
                  (reduce-kv
                    (fn [acc key value]
                      (let [in (conj in key)]
                        (->> acc
                             (key-explainer key in)
                             (value-explainer value in))))
                    acc m)))))
          (-properties [_] properties)
          (-form [_] (create-form :map-of properties (mapv -form schemas))))))))

(defn- -enum-schema []
  ^{:type ::into-schema}
  (reify IntoSchema
    (-name [_] :enum)
    (-into-schema [_ properties childs _]
      (when-not (seq childs)
        (fail! ::no-childs {:name name, :properties properties}))
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
          (-properties [_] properties)
          (-form [_] (create-form :enum properties childs)))))))

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
   (if (schema? ?schema)
     ?schema
     (if (vector? ?schema)
       (apply -into-schema (concat [(get registry (first ?schema))] (properties-and-childs (rest ?schema)) [opts]))
       (if-let [schema' (get registry ?schema)]
         (if (schema? schema')
           schema'
           (-into-schema schema' nil nil opts))
         (fail! ::invalid-schema {:schema ?schema}))))))

(defn form [?schema]
  (-form (schema ?schema)))

(defn properties [?schema]
  (-properties (schema ?schema)))

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
        (if-let [problems (seq (explainer' value in acc))]
          {:schema schema'
           :value value
           :problems problems}))))))

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
  (->> #?(:clj 'clojure.core, :cljs 'cljs.core)
       (ns-publics)
       (filter #(-> % first str last (= \?)))
       (vals)
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
   :map-of (-map-of-schema)
   :vector (-collection-schema :vector vector? vec [])
   :list (-collection-schema :list list? seq nil)
   :set (-collection-schema :set set? set #{})
   :enum (-enum-schema)
   :tuple (-tuple-schema)})

(def default-registry
  (merge predicate-registry comparator-registry base-registry))
