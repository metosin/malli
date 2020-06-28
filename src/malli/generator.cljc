(ns malli.generator
  (:require [clojure.test.check.generators :as gen]
            #?(:clj [com.gfredericks.test.chuck.generators :as gen2])
            [clojure.string :as str]
            [clojure.test.check.random :as random]
            [clojure.test.check.rose-tree :as rose]
            [clojure.spec.gen.alpha :as ga]
            [malli.core :as m]))

(declare generator -create)

(defprotocol Generator
  (-generator [this options] "returns generator for schema"))

(defn- -random [seed] (if seed (random/make-random seed) (random/make-random)))

(defn- -double-gen [options] (gen/double* (merge {:infinite? false, :NaN? false} options)))

(defn- -string-gen [schema options]
  (let [{:keys [min max]} (m/properties schema options)]
    (cond
      (and min (= min max)) (gen/fmap str/join (gen/vector gen/char min))
      (and min max) (gen/fmap str/join (gen/vector gen/char min max))
      min (gen/fmap str/join (gen/vector gen/char min (* 2 min)))
      max (gen/fmap str/join (gen/vector gen/char 0 max))
      :else gen/string)))

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
        value-gen (fn [k s] (gen/fmap (fn [v] [k v]) (generator s options)))
        gen-req (->> entries
                     (remove #(-> % second :optional))
                     (map (fn [[k _ s]] (value-gen k s)))
                     (apply gen/tuple))
        gen-opt (->> entries
                     (filter #(-> % second :optional))
                     (map (fn [[k _ s]] (gen/one-of [(gen/return nil) (value-gen k s)])))
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

(defmulti -schema-generator (fn [schema options] (m/type schema options)) :default ::default)

(defmethod -schema-generator ::default [schema options] (ga/gen-for-pred (m/validator schema options)))

(defmethod -schema-generator :> [schema options] (-double-gen {:min (-> schema (m/children options) first inc)}))
(defmethod -schema-generator :>= [schema options] (-double-gen {:min (-> schema (m/children options) first)}))
(defmethod -schema-generator :< [schema options] (-double-gen {:max (-> schema (m/children options) first dec)}))
(defmethod -schema-generator :<= [schema options] (-double-gen {:max (-> schema (m/children options) first)}))
(defmethod -schema-generator := [schema options] (gen/return (first (m/children schema options))))
(defmethod -schema-generator :not= [schema options] (gen/such-that (partial not= (-> schema (m/children options) first)) gen/any-printable 100))

(defmethod -schema-generator :and [schema options] (gen/such-that (m/validator schema options) (-> schema (m/children options) first (generator options)) 100))
(defmethod -schema-generator :or [schema options] (gen/one-of (mapv #(generator % options) (m/children schema options))))
(defmethod -schema-generator :map [schema options] (-map-gen schema options))
(defmethod -schema-generator :map-of [schema options] (-map-of-gen schema options))
(defmethod -schema-generator :multi [schema options] (gen/one-of (mapv #(generator (second %) options) (m/children schema options))))
(defmethod -schema-generator :vector [schema options] (-coll-gen schema identity options))
(defmethod -schema-generator :list [schema options] (-coll-gen schema (partial apply list) options))
(defmethod -schema-generator :sequential [schema options] (-coll-gen schema identity options))
(defmethod -schema-generator :set [schema options] (-coll-distict-gen schema set options))
(defmethod -schema-generator :enum [schema options] (gen/elements (m/children schema options)))
(defmethod -schema-generator :maybe [schema options] (gen/one-of [(gen/return nil) (-> schema (m/children options) first (generator options))]))
(defmethod -schema-generator :tuple [schema options] (apply gen/tuple (mapv #(generator % options) (m/children schema options))))
#?(:clj (defmethod -schema-generator :re [schema options] (-re-gen schema options)))
(defmethod -schema-generator :string [schema options] (-string-gen schema options))

;; TODO: ref-max need to favor the non-recursive part, not to fail
(defmethod -schema-generator :ref [schema {::keys [ref-max] :or {ref-max 10} :as options}]
  (let [ref (first (m/children schema options))
        ref-count (get-in options [::ref-count ref] 0)]
    (if (< ref-count ref-max)
      (generator (m/-deref schema) (update-in options [::ref-count ref] (fnil inc 0)))
      (gen/fmap (fn [_] (m/fail! :ref-max-exceeded {:ref-max ref-max, :schema schema})) (gen/return nil)))))

(defmethod -schema-generator :schema [schema options] (-create (m/-deref schema) options))

(defn- -create [schema options]
  (let [{:gen/keys [gen fmap elements]} (m/properties schema options)
        gen (or gen (when-not elements (if (satisfies? Generator schema) (-generator schema options) (-schema-generator schema options))))
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
