(ns malli.provider
  (:require [malli.core :as m]))

(declare ->infer)
(declare schema)

(def preferences (-> ['int? 'integer? 'double? 'number? 'qualified-keyword? 'keyword? 'symbol? 'string? 'boolean?]
                     (reverse) (zipmap (range)) (assoc 'any? -10 'some? -9)))

(defn- -safe? [f & args] (try (apply f args) (catch #?(:clj Exception, :cljs js/Error) _ false)))

(defn- registry-schemas [registry] (->> registry (vals) (keep (partial -safe? m/schema))))

(defn- ->infer-schemas [{:keys [registry] :or {registry m/default-registry}}]
  (let [registry-schemas (registry-schemas registry)]
    (fn [x] (-> registry-schemas (->> (filter #(-safe? m/validate % x)) (map m/name)) (zipmap (repeat 1))))))

(defn- -infer-map [acc x opts]
  (reduce-kv (fn [acc k v] (update-in acc [:keys k] (->infer opts) v)) acc x))

(defn- -infer-seq [acc x opts]
  (reduce (->infer opts) acc x))

(defn- ->infer [opts]
  (let [infer-schemas (->infer-schemas opts)
        merge+ (fnil (partial merge-with +) {})]
    (fn [acc x]
      (let [type (cond
                   (map? x) :map
                   (set? x) :set
                   (vector? x) :vector
                   (list? x) :list
                   (sequential? x) :sequential
                   :else :value)]
        (-> acc
            (update :count (fnil inc 0))
            (update-in [:types type :count] (fnil inc 0))
            (cond-> (= :value type) (-> (update-in [:types type :values] merge+ {x 1})
                                        (update-in [:types type :schemas] merge+ (infer-schemas x)))
                    (= :map type) (update-in [:types type] (fnil -infer-map {}) x opts)
                    (#{:set :vector :list :sequential} type) (update-in [:types type :values] (fnil -infer-seq {}) x opts)))))))

(defn- -map-schema [{:keys [count] :as stats} opts]
  (->> (:keys stats)
       (map (fn [[k kstats]]
              (let [kschema (schema kstats opts)]
                (if (not= count (:count kstats)) [k {:optional true} kschema] [k kschema]))))
       (into [:map])))

(defn- -value-schema [{:keys [schemas]}]
  (let [max (->> schemas vals (apply max))]
    (->> schemas
         (filter #(= max (val %)))
         (map (fn [[k]] [k (preferences k -1)]))
         (sort-by (comp second) >)
         (ffirst))))

;;
;; public api
;;

(defn stats
  ([xs]
   (stats xs nil))
  ([xs opts]
   (reduce (->infer opts) opts xs)))

(defn schema
  ([stats]
   (schema stats nil))
  ([{:keys [types] :as stats} opts]
   (if (= 1 (count (keys types)))
     (let [type (-> types keys first)]
       (case type
         :value (-value-schema (type types))
         (:set :vector :list :sequential) [type (-> types type :values (schema opts))]
         :map (-map-schema (type types) opts)))
     (into [:or] (map (fn [[type]] (schema (update stats :types select-keys [type]) opts)) types)))))

(defn provide
  ([xs]
   (provide xs nil))
  ([xs opts]
   (-> xs (stats opts) (schema opts))))
