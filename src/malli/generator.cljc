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

(defn- -double-gen [options] (gen/double* (merge {:infinite? false, :NaN? false} options)))

(defn- -coll-gen [schema f options]
  (let [{:keys [min max]} (m/properties schema options)
        gen (-> schema m/children first (generator options))]
    (gen/fmap f (cond
                  (and min (= min max)) (gen/vector gen min)
                  (and min max) (gen/vector gen min max)
                  min (gen/vector gen min (* 2 min))
                  max (gen/vector gen 0 max)
                  :else (gen/vector gen)))))

(defn- -coll-distict-gen [schema f options]
  (let [{:keys [min max]} (m/properties schema options)
        gen (-> schema m/children first (generator options))]
    (gen/fmap f (gen/vector-distinct gen {:min-elements min, :max-elements max, :max-tries 100}))))

(defn -map-gen [schema options]
  (let [entries (m/map-entries schema)
        value-gen (fn [k o s]
                    (let [{:gen/keys [gen fmap elements]} o
                          gen (or (when elements (gen/elements elements))
                                  gen
                                  (generator s options))]
                      (gen/fmap
                       (fn [v] [k v])
                       (if fmap
                         (gen/fmap (m/eval fmap) gen)
                         gen))))
        gen-req (->> entries
                     (remove #(-> % second :optional))
                     (map (fn [[k o s]] (value-gen k o s)))
                     (apply gen/tuple))
        gen-opt (->> entries
                     (filter #(-> % second :optional))
                     (map (fn [[k o s]] (gen/one-of [(gen/return nil) (value-gen k o s)])))
                     (apply gen/tuple))]
    (gen/fmap (fn [[req opt]] (into {} (concat req opt))) (gen/tuple gen-req gen-opt))))

(defn -map-of-gen [schema options]
  (let [[k-gen v-gen] (map #(generator % options) (m/children schema options))]
    (gen/fmap (partial into {}) (gen/vector-distinct (gen/tuple k-gen v-gen)))))

#?(:clj
   (defn -re-gen [schema options]
     (let [re (or (first (m/children schema options)) (m/form schema options))]
       (gen2/string-from-regex (re-pattern (str/replace (str re) #"^\^?(.*?)(\$?)$" "$1"))))))

;;
;; generators
;;

(defmulti -generator (fn [schema options] (m/name schema options)) :default ::default)

(defmethod -generator ::default [schema options] (ga/gen-for-pred (m/validator schema options)))

(defmethod -generator :> [schema options] (-double-gen {:min (-> schema (m/children options) first inc)}))
(defmethod -generator :>= [schema options] (-double-gen {:min (-> schema (m/children options) first)}))
(defmethod -generator :< [schema options] (-double-gen {:max (-> schema (m/children options) first dec)}))
(defmethod -generator :<= [schema options] (-double-gen {:max (-> schema (m/children options) first)}))
(defmethod -generator := [schema options] (gen/return (first (m/children schema options))))
(defmethod -generator :not= [schema options] (gen/such-that (partial not= (-> schema (m/children options) first)) gen/any-printable 100))

(defmethod -generator :and [schema options] (gen/such-that (m/validator schema options) (-> schema (m/children options) first (generator options)) 100))
(defmethod -generator :or [schema options] (gen/one-of (mapv #(generator % options) (m/children schema options))))
(defmethod -generator :map [schema options] (-map-gen schema options))
(defmethod -generator :map-of [schema options] (-map-of-gen schema options))
(defmethod -generator :multi [schema options] (gen/one-of (mapv #(generator (second %) options) (m/children schema options))))
(defmethod -generator :vector [schema options] (-coll-gen schema identity options))
(defmethod -generator :list [schema options] (-coll-gen schema (partial apply list) options))
(defmethod -generator :sequential [schema options] (-coll-gen schema identity options))
(defmethod -generator :set [schema options] (-coll-distict-gen schema set options))
(defmethod -generator :enum [schema options] (gen/elements (m/children schema options)))
(defmethod -generator :maybe [schema options] (gen/one-of [(gen/return nil) (-> schema (m/children options) first (generator options))]))
(defmethod -generator :tuple [schema options] (apply gen/tuple (mapv #(generator % options) (m/children schema options))))
#?(:clj (defmethod -generator :re [schema options] (-re-gen schema options)))

(defn- -create [schema options]
  (let [{:gen/keys [gen fmap elements]} (m/properties schema options)
        gen (or gen (when-not elements (-generator schema options)))
        elements (when elements (gen/elements elements))]
    (cond
      fmap (gen/fmap (m/eval fmap) (or elements gen (gen/return nil)))
      elements elements
      gen gen
      :else (m/fail! ::no-generator {:schema schema, :options options}))))

;;
;; public api
;;

(defn generator
  ([?schema]
   (generator ?schema nil))
  ([?schema options]
   (-create (m/schema ?schema options) options)))

(defn generate
  ([?gen-or-schema]
   (generate ?gen-or-schema nil))
  ([?gen-or-schema {:keys [seed size] :or {size 1} :as options}]
   (let [gen (if (gen/generator? ?gen-or-schema) ?gen-or-schema (generator ?gen-or-schema options))]
     (rose/root (gen/call-gen gen (-random seed) size)))))

(defn sample
  ([?gen-or-schema]
   (sample ?gen-or-schema nil))
  ([?gen-or-schema {:keys [seed size] :or {size 10} :as options}]
   (let [gen (if (gen/generator? ?gen-or-schema) ?gen-or-schema (generator ?gen-or-schema options))]
     (->> (gen/make-size-range-seq size)
          (map #(rose/root (gen/call-gen gen %1 %2))
               (gen/lazy-random-states (-random seed)))
          (take size)))))
