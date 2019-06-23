(ns malli.malli)

;;
;; protocols
;;

(defprotocol IntoSchema
  (-id [this])
  (-aliases [this])
  (-into-schema [this props childs opts]))

(defprotocol Schema
  (-validator [this opts])
  (-props [this])
  (-form [this]))

(defn schema? [x]
  (satisfies? Schema x))

(defmethod print-method ::into-schema [v ^java.io.Writer w]
  (.write w (str "#Schema{:id " (-id v) "}")))

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

(defn create-form [id props childs]
  (cond
    (and (seq props) (seq childs)) (into [id props] childs)
    (seq props) [id props]
    (seq childs) (into [id] childs)
    :else id))

(defn- var-schema [v]
  (assert (var? v))
  (let [id (-> v meta :name)]
    ^{:type ::into-schema}
    (reify IntoSchema
      (-id [_] id)
      (-aliases [_] [id @v])
      (-into-schema [_ props childs _]
        (when (seq childs)
          (fail! ::childs-not-allowed {:id id, :props props, :childs childs}))
        ^{:type ::schema}
        (reify Schema
          (-validator [_ _] @v)
          (-props [_] props)
          (-form [_] (create-form id props nil)))))))

(defn- composite-schema [id f]
  ^{:type ::into-schema}
  (reify IntoSchema
    (-id [_] id)
    (-aliases [_])
    (-into-schema [_ props childs opts]
      (when-not (seq childs)
        (fail! ::no-childs {:id id, :props props}))
      (let [child-schemas (mapv #(schema % opts) childs)
            validators (distinct (map #(-validator % opts) child-schemas))
            validator (apply f validators)]
        ^{:type ::schema}
        (reify Schema
          (-validator [_ _] validator)
          (-props [_] props)
          (-form [_] (create-form id props (map -form child-schemas))))))))

(defn registry! [registry]
  (reduce-kv
    (fn [acc k v]
      (reduce
        (fn [acc alias]
          (if (contains? acc alias)
            (fail! ::schema-already-registered {:key k, :alias alias, :registry registry})
            (assoc acc alias v)))
        acc (concat [k] (-aliases v))))
    {} registry))

(defn- props-and-childs [xs]
  (if (map? (first xs))
    [(first xs) (rest xs)]
    [nil xs]))

;;
;; public api
;;

(defn schema
  ([?schema]
   (schema ?schema nil))
  ([?schema {:keys [registry] :as opts :or {registry default-registry}}]
   (if (schema? ?schema)
     ?schema
     (let [reg (registry! registry)]
       (if (vector? ?schema)
         (apply -into-schema (concat [(get reg (first ?schema))] (props-and-childs (rest ?schema)) [opts]))
         (if-let [schema' (get reg ?schema)]
           (-into-schema schema' nil nil opts)
           (fail! ::invalid-schema {:schema ?schema})))))))

(defn form [?schema]
  (-form (schema ?schema)))

(defn validator
  ([?schema]
   (validator ?schema nil))
  ([?schema opts]
   (-validator (schema ?schema) opts)))

(defn validate
  ([?schema value]
   (validate ?schema value nil))
  ([?schema value opts]
   ((validator ?schema opts) value)))

;;
;; registry
;;

(def default-registry
  {:int (var-schema #'int?)
   :pos-int (var-schema #'pos-int?)
   :neg-int (var-schema #'neg-int?)
   :string (var-schema #'string?)
   :boolean (var-schema #'boolean?)
   :keyword (var-schema #'keyword?)
   :and (composite-schema :and every-pred)
   :or (composite-schema :or some-fn)})

;;
;; helpers
;;

(def Int (schema :int))

;;
;; spike
;;

(ns user)

(require '[malli.malli :as m])

(m/validate m/Int 1)

(m/form [:and int?])
(m/schema [:and int? pos-int?])
(m/validate [:and int? [:and pos-int?]] 2)
(m/validate [:and int? pos-int?] 2)

(m/validate [:and int? [:or pos-int? neg-int?]] 0)

(comment

  (do "perf tests"

      (require '[clojure.spec.alpha :as s])
      (require '[criterium.core :as cc])

      ;; 3ns
      (let [valid? (fn [x] (and (int? x) (or (pos-int? x) (neg-int? x))))]
        (assert (= [true false true] (map valid? [-1 0 1])))
        (cc/quick-bench
          (valid? 0)))

      ;; 5ns
      (let [valid? (m/validator [:and int? [:or pos-int? neg-int?]])]
        (assert (= [true false true] (map valid? [-1 0 1])))
        (cc/quick-bench
          (valid? 0)))

      ;; 40ns
      (let [spec (s/and int? (s/or :pos-int pos-int? :neg-int neg-int?))]
        (assert (= [true false true] (map (partial s/valid? spec) [-1 0 1])))
        (cc/quick-bench
          (s/valid? spec 0)))))
