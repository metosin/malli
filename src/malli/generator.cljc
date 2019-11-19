(ns malli.generator
  (:require [clojure.test.check.generators :as gen]
            [com.gfredericks.test.chuck.generators :as gen2]
            [clojure.test.check.random :as random]
            [clojure.test.check.rose-tree :as rose]
            [clojure.spec.gen.alpha :as ga]
            [malli.core :as m]
            [clojure.string :as str]))

(declare generator)

(defn- -random [seed] (if seed (random/make-random seed) (random/make-random)))

(defn- -double-gen [opts] (gen/double* (merge {:infinite? false, :NaN? false} opts)))

(defn- -coll-gen [schema f opts]
  (let [{:keys [min max]} (m/properties schema opts)
        gen (-> schema m/children first (generator opts))]
    (gen/fmap f (cond
                  (and min (= min max)) (gen/vector gen min)
                  (and min max) (gen/vector gen min max)
                  min (gen/vector gen min (* 2 min))
                  max (gen/vector gen 0 max)
                  :else (gen/vector gen)))))

(defn- -coll-distict-gen [schema f opts]
  (let [{:keys [min max]} (m/properties schema opts)
        gen (-> schema m/children first (generator opts))]
    (gen/fmap f (gen/vector-distinct gen {:min-elements min, :max-elements max, :max-tries 100}))))

(defn -map-gen [schema opts]
  (let [{:keys [entries]} (m/-parse-keys (m/children schema opts) opts)
        value-gen (fn [k s] (gen/fmap (fn [v] [k v]) (generator s opts)))
        gen-req (->> entries
                     (remove #(-> % second :optional))
                     (map (fn [[k _ s]] (value-gen k s)))
                     (apply gen/tuple))
        gen-opt (->> entries
                     (filter #(-> % second :optional))
                     (map (fn [[k _ s]] (gen/one-of [(gen/return nil) (value-gen k s)])))
                     (apply gen/tuple))]
    (gen/fmap (fn [[req opt]] (into {} (concat req opt))) (gen/tuple gen-req gen-opt))))

(defn -map-of-gen [schema opts]
  (let [[k-gen v-gen] (map #(generator % opts) (m/children schema opts))]
    (gen/fmap (partial into {}) (gen/vector-distinct (gen/tuple k-gen v-gen)))))

#?(:clj
   (defn -re-gen [schema opts]
     (let [re (or (first (m/children schema opts)) (m/form schema opts))]
       (gen2/string-from-regex (re-pattern (str/replace (str re) #"^\^?(.*?)(\$?)$" "$1"))))))

;;
;; generators
;;

(defmulti -generator (fn [schema opts] (m/name schema opts)) :default ::default)

(defmethod -generator ::default [schema opts] (ga/gen-for-pred (m/validator schema opts)))

(defmethod -generator :> [schema opts] (-double-gen {:min (-> schema (m/children opts) first inc)}))
(defmethod -generator :>= [schema opts] (-double-gen {:min (-> schema (m/children opts) first)}))
(defmethod -generator :< [schema opts] (-double-gen {:max (-> schema (m/children opts) first dec)}))
(defmethod -generator :<= [schema opts] (-double-gen {:max (-> schema (m/children opts) first)}))
(defmethod -generator := [schema opts] (gen/return (first (m/children schema opts))))
(defmethod -generator :not= [schema opts] (gen/such-that (partial not= (-> schema (m/children opts) first)) gen/any-printable 100))

(defmethod -generator :and [schema opts] (gen/such-that (m/validator schema opts) (-> schema (m/children opts) first (generator opts)) 100))
(defmethod -generator :or [schema opts] (gen/one-of (mapv #(generator % opts) (m/children schema opts))))
(defmethod -generator :map [schema opts] (-map-gen schema opts))
(defmethod -generator :map-of [schema opts] (-map-of-gen schema opts))
(defmethod -generator :multi [schema opts] (gen/one-of (mapv #(generator (second %) opts) (m/children schema opts))))
(defmethod -generator :vector [schema opts] (-coll-gen schema identity opts))
(defmethod -generator :list [schema opts] (-coll-gen schema (partial apply list) opts))
(defmethod -generator :sequential [schema opts] (-coll-gen schema identity opts))
(defmethod -generator :set [schema opts] (-coll-distict-gen schema set opts))
(defmethod -generator :enum [schema opts] (gen/elements (m/children schema opts)))
(defmethod -generator :maybe [schema opts] (gen/one-of [(gen/return nil) (-> schema (m/children opts) first (generator opts))]))
(defmethod -generator :tuple [schema opts] (apply gen/tuple (mapv #(generator % opts) (m/children schema opts))))
#?(:clj (defmethod -generator :re [schema opts] (-re-gen schema opts)))

(defn- -create [schema opts]
  (let [{:gen/keys [fmap elements]} (m/properties schema opts)
        gen (when-not elements (-generator schema opts))
        elements (when elements (gen/elements elements))]
    (cond
      fmap (gen/fmap (m/eval fmap) (or elements gen (gen/return nil)))
      elements elements
      gen gen
      :else (m/fail! ::no-generator {:schema schema, :opts opts}))))

;;
;; public api
;;

(defn generator
  ([?schema]
   (generator ?schema nil))
  ([?schema opts]
   (-create (m/schema ?schema opts) opts)))

(defn generate
  ([?gen-or-schema]
   (generate ?gen-or-schema nil))
  ([?gen-or-schema {:keys [seed size] :or {size 1} :as opts}]
   (let [gen (if (gen/generator? ?gen-or-schema) ?gen-or-schema (generator ?gen-or-schema opts))]
     (rose/root (gen/call-gen gen (-random seed) size)))))

(defn sample
  ([?gen-or-schema]
   (sample ?gen-or-schema nil))
  ([?gen-or-schema {:keys [seed size] :or {size 10} :as opts}]
   (let [gen (if (gen/generator? ?gen-or-schema) ?gen-or-schema (generator ?gen-or-schema opts))]
     (->> (gen/make-size-range-seq size)
          (map #(rose/root (gen/call-gen gen %1 %2))
               (gen/lazy-random-states (-random seed)))
          (take size)))))
