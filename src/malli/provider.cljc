(ns malli.provider
  (:require [malli.core :as m]
            [malli.registry :as mr]))

(def -preferences (-> [:int 'integer? :double :float 'number? :qualified-keyword :keyword :symbol :string :boolean :uuid 'inst?]
                      (reverse) (zipmap (drop 1 (range))) (assoc 'any? -14 'some? -13, :or -12, :and -11, :any -10, :some -9)))

(defn -safe? [f & args] (try (apply f args) (catch #?(:clj Exception, :cljs js/Error) _ false)))

(defrecord Hinted [value hint])
(defn -hinted [x hint] (->Hinted x hint))
(defn -value-hint [x] (if (instance? Hinted x) [(:value x) (:hint x)] [x (some-> x meta ::hint)]))

(defn -inferrer [options]
  (let [schemas (->> options (m/-registry) (mr/-schemas) (vals) (filter #(-safe? m/schema %)))
        form->validator (into {} (mapv (juxt m/form m/validator) schemas))
        infer-value (fn [x] (-> (reduce-kv (fn [acc f v] (cond-> acc (-safe? v x) (assoc f 1))) {} form->validator)))
        entry-inferrer (fn [infer] (fn [acc k v] (update acc :keys update k infer v)))
        infer-map (fn [infer] (fn [acc x] (update (reduce-kv (entry-inferrer infer) acc x) :data (fnil conj []) x)))
        infer-seq (fn [infer] (fn [acc x] (update (reduce infer acc x) :data (fnil conj []) x)))
        merge+ (fnil #(merge-with + %1 %2) {})]
    (fn infer [acc x]
      (let [[x hint] (-value-hint x)
            type (cond (nil? x) :nil
                       (map? x) :map
                       (set? x) :set
                       (vector? x) :vector
                       (sequential? x) :sequential
                       :else :value)
            ->type #(as-> (update % :count (fnil inc 0)) $
                      (cond-> $ hint (update :hints (fnil conj #{}) hint))
                      (case type
                        (:value :nil) (-> $ (update :values merge+ {x 1}) (update :schemas merge+ (infer-value x)))
                        :map ((infer-map infer) $ x)
                        (:set :vector :sequential) (update $ :values (fnil (infer-seq infer) {}) x)))]
        (-> acc (update :count (fnil inc 0)) (update :types update type ->type))))))

(defn -value-schema [{:keys [schemas hints] :as stats}]
  (or (when-let [hint (and (= 1 (count hints)) (first hints))]
        (case hint :enum (into [:enum] (keys (:values stats))), hint))
      (let [max (->> schemas vals (apply max))]
        (->> schemas (filter #(= max (val %))) (map (fn [[k]] [k (-preferences k -1)])) (sort-by second >) (ffirst)))))

(defn -sequential-schema [{tc :count :as stats} type schema {:keys [::infer ::tuple-threshold] :as options}]
  (let [vstats* (delay (-> stats :types type))
        data* (delay (-> @vstats* :values :data))
        vs* (delay (map (fn [x] (map #(schema (infer {} %)) x)) @data*))
        tuple?* (delay (apply = (map count @vs*)))]
    (or (and (= :vector type)
             (or (when (and (some-> @vstats* :hints (= #{:tuple})) @tuple?*)
                   (into [:tuple] (map #(schema (reduce infer {} %) options) (apply map vector @data*))))
                 (when-let [tuple-threshold (when (and tuple-threshold (= tc (:count @vstats*))) tuple-threshold)]
                   (when (and (>= tc tuple-threshold) @tuple?*)
                     (when (apply = @vs*) (into [:tuple] (first @vs*)))))))
        [type (-> @vstats* :values (schema options))])))

(defn -map-of-accept [stats]
  (let [ks (->> stats :data (mapcat keys))] (> (count (distinct ks)) (Math/pow (count ks) 0.7))))

(defn -map-schema [{tc :count :as stats} schema {:keys [::infer ::map-of-threshold ::map-of-accept] :or {map-of-accept -map-of-accept} :as options}]
  (let [entries (map (fn [[key vstats]] {:key key, :vs (schema vstats options), :vc (:count vstats)}) (:keys stats))
        ks* (delay (schema (reduce infer {} (map :key entries)) options))
        ?ks* (delay (let [kss (map #(schema (infer {} (:key %)) options) entries)] (when (apply = kss) (first kss))))
        vs* (delay (schema (reduce infer {} (->> stats :data (mapcat vals))) options))
        vss (map :vs entries)]
    (or (when (some-> stats :hints (= #{:map-of})) [:map-of @ks* @vs*])
        (when (and (some->> map-of-threshold (>= tc)) @?ks* (apply = vss) (map-of-accept stats)) [:map-of @?ks* (first vss)])
        (into [:map] (map (fn [{:keys [key vs vc]}] (if (not= tc vc) [key {:optional true} vs] [key vs])) entries)))))

(defn -decoded [{:keys [values]} vp t]
  (let [vs (keys values), << (fn [f] (reduce (fn [_ v] (let [v' (f v)] (or (not= v v') (reduced false)))) false vs))]
    (reduce-kv (fn [acc s f] (if (<< f) (reduced s) acc)) t vp)))

(defn -schema
  ([stats] (-schema stats nil))
  ([{:keys [types] :as stats} {:keys [::value-decoders] :as options}]
   (cond (= 1 (count (keys types))) (let [type (-> types keys first)]
                                      (case type
                                        :nil :nil
                                        :value (let [t (type types), vs (-value-schema t), vp (get value-decoders vs)]
                                                 (cond->> vs vp (-decoded t vp)))
                                        (:set :vector :sequential) (-sequential-schema stats type -schema options)
                                        :map (-map-schema (type types) -schema options)))
         (nil? types) :any
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

  - `:malli.provider/tuple-threshold, how many identical value schemas need for :tuple
  - `:malli.provider/map-of-threshold, how many identical value schemas need for :map-of
  - `:malli.provider/map-of-accept, function of type `stats -> boolean` to identify :map-of
  - `:malli.provider/value-decoders, function of `type -> target-type -> value -> decoded-value`"
  ([] (provider nil))
  ([options] (let [infer (-inferrer options)]
               (fn [xs] (-> (reduce infer {} xs) (-schema (assoc options ::infer infer)))))))

(defn provide
  "Given an sequence of example values, returns a Schema that can all values are valid against.
   For better performance, use [[provider]] instead. See [[provider]] for available options."
  ([xs] (provide xs nil))
  ([xs options] ((provider options) xs)))
