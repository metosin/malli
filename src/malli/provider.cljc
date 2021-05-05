(ns malli.provider
  (:require [malli.core :as m]
            [malli.registry :as mr]))

(def preferences (-> ['int? 'integer? 'double? 'number? 'qualified-keyword? 'keyword? 'symbol? 'string? 'boolean?]
                     (reverse) (zipmap (range)) (assoc 'any? -10 'some? -9)))

(defn- -safe? [f & args] (try (apply f args) (catch #?(:clj Exception, :cljs js/Error) _ false)))

(defn- ->inferrer [options]
  (let [schemas (->> options (m/-registry) (mr/-schemas) (vals) (keep #(-safe? m/schema %)) (vec))
        infer-value (fn [x] (-> schemas (->> (filter #(-safe? m/validate % x)) (map m/type)) (zipmap (repeat 1))))
        infer-map (fn [infer] (fn [acc x] (reduce-kv (fn [acc k v] (update-in acc [:keys k] infer v)) acc x)))
        infer-seq (fn [infer] (fn [acc x] (reduce infer acc x)))
        merge+ (fnil #(merge-with + %1 %2) {})]
    (fn infer [acc x]
      (let [type (cond
                   (map? x) :map
                   (set? x) :set
                   (vector? x) :vector
                   (sequential? x) :sequential
                   :else :value)]
        (-> acc
            (update :count (fnil inc 0))
            (update-in [:types type :count] (fnil inc 0))
            (cond-> (= :value type) (-> (update-in [:types type :values] merge+ {x 1})
                                        (update-in [:types type :schemas] merge+ (infer-value x)))
                    (= :map type) (update-in [:types type] (fnil (infer-map infer) {}) x)
                    (#{:set :vector :sequential} type) (update-in [:types type :values] (fnil (infer-seq infer) {}) x)))))))

(defn- -map-schema [{:keys [count] :as stats} schema options]
  (->> (:keys stats)
       (map (fn [[k kstats]]
              (let [kschema (schema kstats options)]
                (if (not= count (:count kstats)) [k {:optional true} kschema] [k kschema]))))
       (into [:map])))

(defn- -value-schema [{:keys [schemas]}]
  (let [max (->> schemas vals (apply max))]
    (->> schemas
         (filter #(= max (val %)))
         (map (fn [[k]] [k (preferences k -1)]))
         (sort-by second >)
         (ffirst))))

;;
;; public api
;;

(defn ^:deprecated stats
  ([xs]
   (stats xs nil))
  ([xs options]
   (reduce (->inferrer options) options xs)))

(defn schema
  ([stats]
   (schema stats nil))
  ([{:keys [types] :as stats} options]
   (cond
     (= 1 (count (keys types)))
     (let [type (-> types keys first)]
       (case type
         :value (-value-schema (type types))
         (:set :vector :sequential) [type (-> types type :values (schema options))]
         :map (-map-schema (type types) schema options)))
     (nil? types) (m/schema any?)
     :else (into [:or] (map (fn [[type]] (schema (update stats :types select-keys [type]) options)) types)))))

(defn provider
  ([]
   (provider nil))
  ([options]
   (let [inferrer (->inferrer options)]
     (fn [xs] (-> (reduce inferrer {} xs) (schema options))))))

(defn provide
  ([xs]
   (provide xs nil))
  ([xs options]
   ((provider options) xs)))
