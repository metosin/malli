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
  (-properties [this])
  (-form [this]))

(defn schema? [x]
  (satisfies? Schema x))

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

(defn- -leaf-schema [name ->validator-and-childs]
  ^{:type ::into-schema}
  (reify IntoSchema
    (-name [_] name)
    (-into-schema [_ properties childs opts]
      (let [[validator childs] (->validator-and-childs properties childs opts)]
        ^{:type ::schema}
        (reify Schema
          (-validator [_] validator)
          (-properties [_] properties)
          (-form [_] (create-form name properties childs)))))))

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
      [#(f % child) childs])))

(defn- -composite-schema [name f]
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
          (-properties [_] properties)
          (-form [_] (create-form name properties (map -form child-schemas))))))))

(defn- properties-and-childs [xs]
  (if (map? (first xs))
    [(first xs) (rest xs)]
    [nil xs]))

(defn- expand-key [[k ?p ?v] opts f]
  (let [[p v] (if (map? ?p) [?p ?v] [nil ?p])
        v' (schema v opts)]
    (if-not (vector? k)
      [k {:optional (:optional p false)} (f v')]
      (let [[r k] k]
        [k {:optional (case r :opt true, :req false)} (f v')]))))

(defn- parse-keys [childs opts]
  (let [entries (mapv #(expand-key % opts identity) childs)]
    {:required (->> entries (filter (comp not :optional second)) (mapv first))
     :optional (->> entries (filter (comp :optional second)) (mapv first))
     :keys (->> entries (mapv first))
     :entries entries}))

(defn- -map-schema []
  ^{:type ::into-schema}
  (reify IntoSchema
    (-name [_] :map)
    (-into-schema [_ properties childs opts]
      (when-not (seq childs)
        (fail! ::no-childs {:name :map, :properties properties}))
      (let [{:keys [entries]} (parse-keys childs opts)
            form (create-form :map properties (mapv #(expand-key % opts -form) childs))]
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
          (-properties [_] properties)
          (-form [_] form))))))

(defn- -collection-schema [name f]
  ^{:type ::into-schema}
  (reify IntoSchema
    (-name [_] name)
    (-into-schema [_ {:keys [min max] :as properties} childs opts]
      (when-not (= 1 (count childs))
        (fail! ::child-error {:name :vector, :properties properties, :childs childs, :min 1, :max 1}))
      (let [schemas (mapv #(schema % opts) childs)
            form (create-form name properties (map -form schemas))
            check-limits (cond
                           (not (or min max)) (constantly true)
                           (and min max) (fn [x] (let [size (count x)] (<= min size max)))
                           min (fn [x] (let [size (count x)] (<= min size)))
                           max (fn [x] (let [size (count x)] (<= size max))))]
        ^{:type ::schema}
        (reify Schema
          (-validator [_]
            (let [validator (-validator (first schemas))]
              (fn [x] (and (f x)
                           (check-limits x)
                           (reduce (fn [acc v] (if (validator v) acc (reduced false))) true x)))))
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
  {:and (-composite-schema :and every-pred)
   :or (-composite-schema :or some-fn)
   :map (-map-schema)
   :vector (-collection-schema :vector vector?)
   :list (-collection-schema :list list?)
   :set (-collection-schema :set set?)
   :tuple (-tuple-schema)})

(def default-registry
  (merge predicate-registry comparator-registry base-registry))
