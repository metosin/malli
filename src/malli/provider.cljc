(ns malli.provider
  (:require [malli.core :as m]
            [malli.registry :as mr]))

(declare ->infer)
(declare schema)

(def preferences (-> ['int? 'integer? 'double? 'number? 'qualified-keyword? 'keyword? 'symbol? 'string? 'boolean?]
                     (reverse) (zipmap (range)) (assoc 'any? -10 'some? -9)))

(defn- -safe? [f & args] (try (apply f args) (catch #?(:clj Exception, :cljs js/Error) _ false)))

(defn- registry-schemas [options] (->> options (m/-registry) (mr/-schemas) (vals) (keep (partial -safe? m/schema))))

(defn- ->infer-schemas [options]
  (fn [x] (-> options (registry-schemas) (->> (filter #(-safe? m/validate % x)) (map m/type)) (zipmap (repeat 1)))))

(defn- -infer-map [acc x options]
  (reduce-kv (fn [acc k v] (update-in acc [:keys k] (->infer options) v)) acc x))

(defn- -infer-seq [acc x options]
  (reduce (->infer options) acc x))

(defn- ->infer [options]
  (let [infer-schemas (->infer-schemas options)
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
                    (= :map type) (update-in [:types type] (fnil -infer-map {}) x options)
                    (#{:set :vector :list :sequential} type) (update-in [:types type :values] (fnil -infer-seq {}) x options)))))))

(defn- -map-schema [{:keys [count] :as stats} options]
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
         (sort-by (comp second) >)
         (ffirst))))

;;
;; public api
;;

(defn stats
  ([xs]
   (stats xs nil))
  ([xs options]
   (reduce (->infer options) options xs)))

(defn schema
  ([stats]
   (schema stats nil))
  ([{:keys [types] :as stats} options]
   (cond
     (= 1 (count (keys types)))
     (let [type (-> types keys first)]
       (case type
         :value (-value-schema (type types))
         (:set :vector :list :sequential) [type (-> types type :values (schema options))]
         :map (-map-schema (type types) options)))
     (nil? types) (m/schema any?)
     :else (into [:or] (map (fn [[type]] (schema (update stats :types select-keys [type]) options)) types)))))

(defn provide
  ([xs]
   (provide xs nil))
  ([xs options]
   (-> xs (stats options) (schema options))))
