(ns malli.provider
  (:require [malli.core :as m]
            [malli.registry :as mr]))

(def -preferences (-> ['int? 'integer? 'double? 'number? 'qualified-keyword? 'keyword? 'symbol? 'string? 'boolean? 'uuid?]
                      (reverse) (zipmap (drop 1 (range))) (assoc :any -13, :or -12, :and -11, 'any? -10, 'some? -9)))

(defn -safe? [f & args] (try (apply f args) (catch #?(:clj Exception, :cljs js/Error) _ false)))

(defn -inferrer [options]
  (let [schemas (->> options (m/-registry) (mr/-schemas) (vals) (filter #(-safe? m/schema %)))
        form->validator (into {} (mapv (juxt m/form m/validator) schemas))
        infer-value (fn [x] (-> (reduce-kv (fn [acc f v] (cond-> acc (-safe? v x) (assoc f 1))) {} form->validator)))
        entry-inferrer (fn [infer] (fn [acc k v] (update acc :keys update k infer v)))
        infer-map (fn [infer] (fn [acc x] (assoc (reduce-kv (entry-inferrer infer) acc x) :value x)))
        infer-seq (fn [infer] (fn [acc x] (reduce infer acc x)))
        merge+ (fnil #(merge-with + %1 %2) {})]
    (fn infer [acc x]
      (let [type (cond (nil? x) :nil
                       (map? x) :map
                       (set? x) :set
                       (vector? x) :vector
                       (sequential? x) :sequential
                       :else :value)]
        (-> acc
            (update :count (fnil inc 0))
            (update-in [:types type :count] (fnil inc 0))
            (cond->
              (= :value type) (-> (update-in [:types type :values] merge+ {x 1})
                                  (update-in [:types type :schemas] merge+ (infer-value x)))
              (= :map type) (update-in [:types type] (fnil (infer-map infer) {}) x)
              (#{:set :vector :sequential} type) (update-in [:types type :values] (fnil (infer-seq infer) {}) x)))))))

(defn -map-schema [{tc :count :as stats} schema {::keys [infer map-of-threshold] :or {map-of-threshold 3} :as options}]
  (let [entries (map (fn [[key vstats]] {:key key, :vs (schema vstats options), :vc (:count vstats)}) (:keys stats))
        kschema* (delay (let [kschemas (map (fn [{:keys [key]}] (schema (infer {} key) options)) entries)]
                          (when (apply = kschemas) (first kschemas))))
        vschema* (delay (let [vschema (schema (reduce infer {} (->> stats :value (vals))) options)]
                          (when (#{:map :maybe} (first vschema)) vschema)))
        vschemas (map :vs entries)]
    (or (when (and (>= (count entries) map-of-threshold) @kschema*)
          (when-let [vschema (if (apply = vschemas) (first vschemas) @vschema*)] [:map-of @kschema* vschema]))
        (into [:map] (map (fn [{:keys [key vs vc]}] (if (not= tc vc) [key {:optional true} vs] [key vs])) entries)))))

(defn -value-schema [{:keys [schemas]}]
  (let [max (->> schemas vals (apply max))]
    (->> schemas
         (filter #(= max (val %)))
         (map (fn [[k]] [k (-preferences k -1)]))
         (sort-by second >)
         (ffirst))))

(defn -schema
  ([stats]
   (-schema stats nil))
  ([{:keys [types] :as stats} options]
   (cond (= 1 (count (keys types))) (let [type (-> types keys first)]
                                      (case type
                                        :nil :nil
                                        :value (-value-schema (type types))
                                        (:set :vector :sequential) [type (-> types type :values (-schema options))]
                                        :map (-map-schema (type types) -schema options)))
         (nil? types) (m/schema any?)
         :else (let [children (map (fn [[type]] (-schema (update stats :types select-keys [type]) options)) types)
                     without-nils (remove #(= % :nil) children)
                     [c1 c2] (map count [children without-nils])]
                 (cond (= 1 c2) (into [:maybe] without-nils)
                       (not= c1 c2) [:maybe (into [:or] without-nils)]
                       :else (into [:or] children))))))

;;
;; public api
;;

(defn provider
  "Returns a inferring function of `values -> schema`. Supports the following options:

  - `:malli.provider/map-of-threshold (default 3), how many entries need for :map-of"
  ([] (provider nil))
  ([options] (let [infer (-inferrer options)]
               (fn [xs] (-> (reduce infer {} xs) (-schema (assoc options ::infer infer)))))))

(defn provide
  "Given an sequence of example values, returms a Schema that can all values are valid against.
   For better performance, user [[provider]] instead. see [[provider]] for available options."
  ([xs] (provide xs nil))
  ([xs options] ((provider options) xs)))
