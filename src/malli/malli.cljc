(ns malli.malli)

(set! *warn-on-reflection* true)

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
  (.write w (str "#IntoSchema{:id " (-id v) "}")))

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

(defn- -var-schema [v]
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

(defn- -composite-schema [id f]
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

(defn- props-and-childs [xs]
  (if (map? (first xs))
    [(first xs) (rest xs)]
    [nil xs]))

(defn- expand-key [[k v] opts]
  (let [v' (schema v opts)]
    (if-not (vector? k)
      [k {:required true} v']
      (let [[r k] k]
        [k {:required (case r :opt false, :req true)} v']))))

(defn- parse-keys [childs opts]
  (let [entries (mapv #(expand-key % opts) childs)]
    {:required (->> entries (filter (comp :required second)) (mapv first))
     :optional (->> entries (filter (comp not :required second)) (mapv first))
     :keys (->> entries (mapv first))
     :entries entries}))

(defn- -map-schema []
  ^{:type ::into-schema}
  (reify IntoSchema
    (-id [_] :map)
    (-aliases [_])
    (-into-schema [_ props childs opts]
      (when-not (seq childs)
        (fail! ::no-childs {:id :map, :props props}))
      (let [{:keys [entries]} (parse-keys childs opts)
            form (create-form :map props (mapv #(expand-key % opts) childs))]
        ^{:type ::schema}
        (reify Schema
          (-validator [_ opts]
            (let [validators (mapv
                               (fn [[key {:keys [required]} value]]
                                 (let [valid? (-validator value opts)
                                       default (not required)]
                                   (fn [m] (if-let [v (key m)] (valid? v) default))))
                               entries)]
              (fn [m] (let [i (.iterator ^Iterable validators)]
                        (boolean
                          (loop []
                            (if (.hasNext i)
                              (and ((.next i) m) (recur))
                              true)))))))
          (-props [_] props)
          (-form [_] form))))))

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
   (-validator (schema ?schema opts) opts)))

(defn validate
  ([?schema value]
   (validate ?schema value nil))
  ([?schema value opts]
   ((validator ?schema opts) value)))

;;
;; registry
;;

(def default-registry
  {:int (-var-schema #'int?)
   :pos-int (-var-schema #'pos-int?)
   :neg-int (-var-schema #'neg-int?)
   :string (-var-schema #'string?)
   :boolean (-var-schema #'boolean?)
   :keyword (-var-schema #'keyword?)
   :and (-composite-schema :and every-pred)
   :or (-composite-schema :or some-fn)
   :map (-map-schema)})

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

(m/schema [:map [:x int?]])

(m/schema [:map
           [:x boolean?]
           [[:opt :y] int?]
           [:z string?]])

(comment
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
   [:z string? :required]])

(comment

  (do "imports"

      (require '[clojure.spec.alpha :as s])
      (require '[criterium.core :as cc]))

  (do "map perf test"

      (s/def ::x boolean?)
      (s/def ::y int?)
      (s/def ::z string?)

      (let [valid {:x true, :y 1, :z "kikka"}]

        ;; 22ns
        (let [valid? (fn [m]
                       (and (if-let [v (:x m)] (boolean? v) false)
                            (if-let [v (:y m)] (int? v) true)
                            (if-let [v (:z m)] (string? v) false)))]
          (assert (valid? valid))
          (cc/quick-bench
            (valid? valid)))

        ;; 37ns
        (let [valid? (m/validator [:map
                                   [:x boolean?]
                                   [[:opt :y] int?]
                                   [:z string?]])]
          (assert (valid? valid))
          (cc/quick-bench
            (valid? valid)))

        ;; 400ns
        (let [spec (s/keys :req-un [::x ::z] :opt-un [::y])]
          (assert (s/valid? spec valid))
          (cc/quick-bench
            (s/valid? spec valid)))))

  (do "composite tests"

      ;; 300ns -> 0.3ns
      (let [valid? (fn [x] (and (int? x) (or (pos-int? x) (neg-int? x))))]
        (assert (= [true false true] (map valid? [-1 0 1])))
        (cc/quick-bench
          (dotimes [_ 1000]
            (valid? 0))))

      ;; 3000ns -> 3ns
      (let [valid? (m/validator [:and int? [:or pos-int? neg-int?]])]
        (assert (= [true false true] (map valid? [-1 0 1])))
        (cc/quick-bench
          (dotimes [_ 1000]
            (valid? 0))))

      ;; 40000ns -> 40ns
      (let [spec (s/and int? (s/or :pos-int pos-int? :neg-int neg-int?))]
        (assert (= [true false true] (map (partial s/valid? spec) [-1 0 1])))
        (cc/quick-bench
          (dotimes [_ 1000]
            (s/valid? spec 0))))))
