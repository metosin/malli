(ns malli.core)

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

(defmethod print-method ::into-schema [v ^java.io.Writer w]
  (.write w (str "#IntoSchema{:name " (-name v) "}")))

(defmethod print-method ::schema [v ^java.io.Writer w]
  (.write w (str (-form v))))

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

(defn- -leaf-schema [name ->validator]
  ^{:type ::into-schema}
  (reify IntoSchema
    (-name [_] name)
    (-into-schema [_ properties childs opts]
      (let [validator (->validator properties childs opts)]
        ^{:type ::schema}
        (reify Schema
          (-validator [_] validator)
          (-properties [_] properties)
          (-form [_] (create-form name properties childs)))))))

(defn- -fn-schema [name f]
  (-leaf-schema
    name
    (fn [properties childs _]
      (when (seq childs)
        (fail! ::childs-not-allowed {:name name, :properties properties, :childs childs}))
      f)))

(defn- -partial-fn-schema [name f]
  (-leaf-schema
    name
    (fn [properties [x :as childs] _]
      (when-not (= 1 (count childs))
        (fail! ::one-childs-allowed {:name name, :properties properties, :childs childs}))
      #(f % x))))

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
      [k {:required (:required p true)} (f v')]
      (let [[r k] k]
        [k {:required (case r :opt false, :req true)} (f v')]))))

(defn- parse-keys [childs opts]
  (let [entries (mapv #(expand-key % opts identity) childs)]
    {:required (->> entries (filter (comp :required second)) (mapv first))
     :optional (->> entries (filter (comp not :required second)) (mapv first))
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
                               (fn [[key {:keys [required]} value]]
                                 (let [valid? (-validator value)
                                       default (not required)]
                                   (fn [m] (if-let [v (key m)] (valid? v) default))))
                               entries)]
              (fn [m] (and (map? m)
                           (let [i (.iterator ^Iterable validators)]
                             (boolean
                               (loop []
                                 (if (.hasNext i)
                                   (and ((.next i) m) (recur))
                                   true))))))))
          (-properties [_] properties)
          (-form [_] form))))))

(defn- -register [registry k schema]
  (if (contains? registry k)
    (fail! ::schema-already-registered {:key k, :registry registry}))
  (assoc registry k schema))

(defn- -register-var [registry v]
  (let [name (-> v meta :name)
        schema (-fn-schema name @v)]
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
         (-into-schema schema' nil nil opts)
         (fail! ::invalid-schema {:schema ?schema}))))))

(defn form [?schema]
  (-form (schema ?schema)))

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
  (->> 'clojure.core (ns-publics) (filter #(-> % first str last (= \?))) (vals) (reduce -register-var {})))

(def comparator-registry
  (->> [:> :>= :< :<= := :not=] (map (fn [k] [k (-partial-fn-schema k (-> k str (subs 1) symbol resolve deref))])) (into {}) (reduce-kv -register nil)))

(def base-registry
  {:and (-composite-schema :and every-pred)
   :or (-composite-schema :or some-fn)
   :map (-map-schema)})

(def default-registry
  (merge predicate-registry comparator-registry base-registry))

;;
;; spike
;;

(ns user)

(require '[malli.core :as m])

(m/validate int? 1)

(m/form [:and int?])
(m/schema [:and int? pos-int?])
(m/validate [:and int? [:and pos-int?]] 2)
(m/validate [:and int? pos-int?] 2)

(m/validate [:and int? [:or pos-int? neg-int?]] 0)

(m/schema [:map [:x int?]])

(m/schema [:map
           [:x boolean?]
           [[:opt :y] int?]
           [:z string?]])

(m/schema [:map
           [:x boolean?]
           [:y {:required false} int?]
           [:z string?]])

(def Age
  (m/schema
    [:and
     {:title "Age"
      :description "Age of a user"
      :json-schema/example 20}
     int? [:not= 18]]))

(m/validate Age 19)

;; schema-style
[:map
 {:closed true}
 [:x boolean?]
 [[:opt :y] int?]
 [[:req :z] string?]]

;; tuples
[:map
 [:x boolean?]
 [:opt :y int?]
 [:req :z string?]]

;; attrs
[:map
 [:x int?]
 [:y {:required false} int?]
 [:z {:required true} string?]]

;; varargs
[:map
 [:x int?]
 [:y int? :optional]
 [:z string? :required]]
