(ns malli.provider
  (:require [malli.core :as m]))

(declare ->infer)
(declare schema)

(defn- -safe? [f & args]
  (try (apply f args) (catch #?(:clj Exception, :cljs js/Error) _ false)))

(defn- registry-schemas [registry]
  (->> registry (vals) (keep (fn [?schema] (try (m/schema ?schema) (catch Exception _))))))

(defn- ->infer-schemas [{:keys [registry] :or {registry m/default-registry}}]
  (let [registry-schemas (registry-schemas registry)]
    (fn [x] (-> registry-schemas (->> (filter #(-safe? m/validate % x)) (map m/name)) (zipmap (repeat 1))))))

(defn- -infer-map [acc x opts]
  (reduce-kv (fn [acc k v] (update-in acc [:keys k] (->infer opts) v)) acc x))

(defn- -infer-seq [acc x opts]
  (reduce (->infer opts) acc x))

(defn- ->infer [opts]
  (let [infer-schemas (->infer-schemas opts)]
    (fn [acc x]
      (let [type (cond
                   (map? x) :map
                   (set? x) :set
                   (vector? x) :vector
                   (sequential? x) :list
                   :else :value)]
        (-> acc
            (update :count (fnil inc 0))
            (update-in [:types type :count] (fnil inc 0))
            (cond-> (= :value type) (-> (update-in [:types type :values] (fnil (partial merge-with +) {}) {x 1})
                                        (update-in [:types type :schemas] (fnil (partial merge-with +) {}) (infer-schemas x)))
                    (= :map type) (update-in [:types type] (fnil -infer-map {}) x opts)
                    (#{:set :vector :list} type) (update-in [:types type :values] (fnil -infer-seq {}) x opts)))))))

(defn- -map-schema [{:keys [count] :as stats} opts]
  (->> (:keys stats)
       (map (fn [[k kstats]]
              (let [kschema (schema kstats opts)]
                (if (not= count (:count kstats)) [k {:optional true} kschema] [k kschema]))))
       (into [:map])))

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
         :value (->> types type :schemas (sort-by second >) ffirst)
         (:set :vector :list) [type (-> types type :values (schema opts))]
         :map (-map-schema (type types) opts)))
     (into [:or] (map (fn [[type]] (schema (update stats :types select-keys [type]) opts)) types)))))

(defn provide
  ([xs]
   (provide xs nil))
  ([xs opts]
   (-> xs (stats opts) (schema opts))))
