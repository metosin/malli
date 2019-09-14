(ns malli.provider
  (:require [malli.core :as m]))

(declare ->infer)

(defn registry-schemas [registry]
  (->> registry (vals) (keep (fn [?schema] (try (m/schema ?schema) (catch Exception _))))))

(defn ->infer-schemas [{:keys [registry] :or {registry m/default-registry}}]
  (let [registry-schemas (registry-schemas registry)]
    (fn [x] (-> registry-schemas (->> (filter #(m/validate % x)) (map m/name)) (zipmap (repeat 1))))))

(defn infer-map [acc x opts]
  (reduce-kv
    (fn [acc k v]
      (update-in acc [:keys k] (->infer opts) v))
    acc x))

(defn infer-seq [acc x opts]
  (reduce (->infer opts) acc x))

(defn ->infer [opts]
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
            (update-in [type :count] (fnil inc 0))
            (cond-> (= :value type) (-> (update-in [type :values] (fnil (partial merge-with +) {}) {x 1})
                                        (update-in [type :schemas] (fnil (partial merge-with +) {}) (infer-schemas x)))
                    (= :map type) (update type (fnil infer-map {}) x opts)
                    (#{:set :vector :list} type) (update-in [type :values] (fnil infer-seq {}) x opts)))))))

(defn collect
  ([x]
   (collect x nil))
  ([x opts]
   (reduce (->infer opts) opts x)))
