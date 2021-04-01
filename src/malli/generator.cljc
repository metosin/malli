(ns malli.generator
  (:require [clojure.test.check.generators :as gen]
            [borkdude.dynaload :as dynaload]
            [clojure.string :as str]
            [clojure.test.check :as check]
            [clojure.test.check.random :as random]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.rose-tree :as rose]
            [clojure.spec.gen.alpha :as ga]
            [malli.core :as m]
            [malli.impl.util :as miu])
  #?(:cljs (:require-macros [malli.generator :refer [-delay]])))

(declare generator generate -create)

(defprotocol Generator
  (-generator [this options] "returns generator for schema"))

(defmacro -delay [& body]
  `((fn [gfnd#] (gen/->Generator (fn [rnd# size#] ((:gen @gfnd#) rnd# size#)))) (delay ~@body)))

(defn- -random [seed] (if seed (random/make-random seed) (random/make-random)))

(defn -recur [schema {::keys [recursion recursion-limit] :or {recursion-limit 4} :as options}]
  (let [form (m/form schema)
        i (get recursion form 0)]
    [(<= i recursion-limit) (update options ::recursion assoc form (inc i))]))

(defn -maybe-recur [schema options]
  (let [[recur options] (-recur schema options)]
    (if recur options)))

(defn -min-max [schema options]
  (let [{:keys [min max] gen-min :gen/min gen-max :gen/max} (m/properties schema options)]
    (when (and min gen-min (< gen-min min))
      (miu/-fail! ::invalid-property {:key :gen/min, :value gen-min, :min min}))
    (when (and max gen-max (> gen-max max))
      (miu/-fail! ::invalid-property {:key :gen/max, :value gen-min, :max min}))
    {:min (or gen-min min)
     :max (or gen-max max)}))

(defn- -double-gen [options] (gen/double* (merge {:infinite? false, :NaN? false} options)))

(defn- -string-gen [schema options]
  (let [{:keys [min max]} (-min-max schema options)]
    (cond
      (and min (= min max)) (gen/fmap str/join (gen/vector gen/char min))
      (and min max) (gen/fmap str/join (gen/vector gen/char min max))
      min (gen/fmap str/join (gen/vector gen/char min (* 2 min)))
      max (gen/fmap str/join (gen/vector gen/char 0 max))
      :else gen/string)))

(defn- -coll-gen [schema f options]
  (let [{:keys [min max]} (-min-max schema options)
        [continue options] (-recur schema options)
        child (-> schema m/children first)
        gen (if continue (generator child options))]
    (gen/fmap f (cond
                  (not gen) (gen/vector gen/any 0 0)
                  (and min (= min max)) (gen/vector gen min)
                  (and min max) (gen/vector gen min max)
                  min (gen/vector gen min (* 2 min))
                  max (gen/vector gen 0 max)
                  :else (gen/vector gen)))))

(defn- -coll-distict-gen [schema f options]
  (let [{:keys [min max]} (-min-max schema options)
        [continue options] (-recur schema options)
        child (-> schema m/children first)
        gen (if-not (and (= :ref (m/type child)) continue (<= (or min 0) 0))
              (generator child options))]
    (gen/fmap f (if gen
                  (gen/vector-distinct gen {:min-elements min, :max-elements max, :max-tries 100})
                  (gen/vector gen/any 0 0)))))

(defn -or-gen [schema options]
  (gen/one-of (keep #(some->> (-maybe-recur % options) (generator %)) (m/children schema options))))

(defn -multi-gen [schema options]
  (gen/one-of (keep #(some->> (-maybe-recur (last %) options) (generator (last %))) (m/entries schema options))))

(defn -map-gen [schema options]
  (let [entries (m/entries schema)
        [continue options] (-recur schema options)
        value-gen (fn [k s] (gen/fmap (fn [v] [k v]) (generator s options)))
        gen-req (->> entries
                     (remove #(-> % last m/properties :optional))
                     (map (fn [[k s]] (value-gen k s)))
                     (apply gen/tuple))
        gen-opt (->> entries
                     (filter #(-> % last m/properties :optional))
                     (map (fn [[k s]] (gen/one-of (into [(gen/return nil)] (if continue [(value-gen k s)])))))
                     (apply gen/tuple))]
    (gen/fmap (fn [[req opt]] (into {} (concat req opt))) (gen/tuple gen-req gen-opt))))

(defn -map-of-gen [schema options]
  (let [[k-gen v-gen] (map #(generator % options) (m/children schema options))]
    (gen/fmap #(into {} %) (gen/vector-distinct (gen/tuple k-gen v-gen)))))

#?(:clj
   (defn -re-gen [schema options]
     ;; [com.gfredericks/test.chuck "0.2.10"+]
     (if-let [string-from-regex @(dynaload/dynaload 'com.gfredericks.test.chuck.generators/string-from-regex {:default nil})]
       (let [re (or (first (m/children schema options)) (m/form schema options))]
         (string-from-regex (re-pattern (str/replace (str re) #"^\^?(.*?)(\$?)$" "$1"))))
       (miu/-fail! :test-chuck-not-available))))

(defn -=>-gen [schema options]
  (let [{:keys [min max input output] :or {max miu/+max-size+}} (m/-function-info schema)
        validate-input (m/validator input)
        output-generator (generator output options)]
    (gen/return
      (fn [& args]
        (let [args (vec args), arity (count args)]
          (when-not (<= min arity max)
            (miu/-fail! ::invalid-arity {:arity arity, :arities #{{:min min, :max max}}, :args args, :schema schema}))
          (when-not (validate-input args)
            (miu/-fail! ::invalid-input {:schema input, :args args}))
          (generate output-generator options))))))

(defn -function-gen [schema options]
  (let [arity->info (->> (for [schema (m/children schema)]
                           (let [{:keys [arity] :as info} (m/-function-info schema)]
                             [arity (assoc info :f (generate schema options))]))
                         (into {}))
        arities (-> arity->info keys set)
        varargs-info (arity->info :varargs)]
    (gen/return
      (fn [& args]
        (let [arity (count args)
              info (arity->info arity)]
          (cond
            info (apply (:f info) args)
            varargs-info (if (< arity (:min varargs-info))
                           (miu/-fail! ::invalid-arity {:arity arity, :arities arities, :args args, :schema schema})
                           (apply (:f varargs-info) args))
            :else (miu/-fail! ::invalid-arity {:arity arity, :arities arities, :args args, :schema schema})))))))

(defn -regex-generator [schema options]
  (if (m/-regex-op? schema)
    (generator schema options)
    (gen/tuple (generator schema options))))

(defn- entry->schema [e] (if (vector? e) (get e 2) e))

(defn -cat-gen [schema options]
  (->> (m/children schema options)
       (map #(-regex-generator (entry->schema %) options))
       (apply gen/tuple)
       (gen/fmap #(apply concat %))))

(defn -alt-gen [schema options]
  (gen/one-of (keep (fn [e]
                      (let [child (entry->schema e)]
                        (some->> (-maybe-recur child options) (-regex-generator child))))
                    (m/children schema options))))

(defn -?-gen [schema options]
  (let [child (m/-get schema 0 nil)]
    (if (m/-regex-op? child)
      (gen/one-of [(generator child options) (gen/return ())])
      (gen/vector (generator child options) 0 1))))

(defn -*-gen [schema options]
  (let [child (m/-get schema 0 nil)]
    (if (m/-regex-op? child)
      (gen/fmap #(apply concat %) (gen/vector (generator child options)))
      (gen/vector (generator child options)))))

(defn -repeat-gen [schema options]
  (let [child (m/-get schema 0 nil)]
    (if (m/-regex-op? child)
      (gen/fmap #(apply concat %) (-coll-gen schema identity options))
      (-coll-gen schema identity options))))

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
(defmethod -schema-generator :not= [schema options] (gen/such-that #(not= % (-> schema (m/children options) first)) gen/any-printable 100))
(defmethod -schema-generator 'pos? [_ _] (gen/one-of [(-double-gen {:min 0.00001}) gen/s-pos-int]))
(defmethod -schema-generator 'neg? [_ _] (gen/one-of [(-double-gen {:max -0.0001}) gen/s-neg-int]))

(defmethod -schema-generator :not [schema options] (gen/such-that (m/validator schema options) (ga/gen-for-pred any?) 100))
(defmethod -schema-generator :and [schema options] (gen/such-that (m/validator schema options) (-> schema (m/children options) first (generator options)) 100))
(defmethod -schema-generator :or [schema options] (-or-gen schema options))
(defmethod -schema-generator :orn [schema options] (-or-gen (m/into-schema :or (m/properties schema) (map last (m/children schema)) (m/options schema)) options))
(defmethod -schema-generator ::m/val [schema options] (generator (first (m/children schema)) options))
(defmethod -schema-generator :map [schema options] (-map-gen schema options))
(defmethod -schema-generator :map-of [schema options] (-map-of-gen schema options))
(defmethod -schema-generator :multi [schema options] (-multi-gen schema options))
(defmethod -schema-generator :vector [schema options] (-coll-gen schema identity options))
(defmethod -schema-generator :sequential [schema options] (-coll-gen schema identity options))
(defmethod -schema-generator :set [schema options] (-coll-distict-gen schema set options))
(defmethod -schema-generator :enum [schema options] (gen/elements (m/children schema options)))

(defmethod -schema-generator :maybe [schema options]
  (let [[continue options] (-recur schema options)]
    (gen/one-of (into [(gen/return nil)] (if continue [(-> schema (m/children options) first (generator options))])))))

(defmethod -schema-generator :tuple [schema options] (apply gen/tuple (mapv #(generator % options) (m/children schema options))))
#?(:clj (defmethod -schema-generator :re [schema options] (-re-gen schema options)))
(defmethod -schema-generator :any [_ _] (ga/gen-for-pred any?))
(defmethod -schema-generator :nil [_ _] (gen/return nil))
(defmethod -schema-generator :string [schema options] (-string-gen schema options))
(defmethod -schema-generator :int [schema options] (gen/large-integer* (-min-max schema options)))
(defmethod -schema-generator :double [schema options] (gen/double* (merge (-min-max schema options) {:infinite? false, :NaN? false})))
(defmethod -schema-generator :boolean [_ _] gen/boolean)
(defmethod -schema-generator :keyword [_ _] gen/keyword)
(defmethod -schema-generator :symbol [_ _] gen/symbol)
(defmethod -schema-generator :qualified-keyword [_ _] (gen/such-that qualified-keyword? gen/keyword-ns))
(defmethod -schema-generator :qualified-symbol [_ _] (gen/such-that qualified-symbol? gen/symbol-ns))
(defmethod -schema-generator :uuid [_ _] gen/uuid)

(defmethod -schema-generator :=> [schema options] (-=>-gen schema options))
(defmethod -schema-generator :function [schema options] (-function-gen schema options))
(defmethod -schema-generator :ref [schema options] (-delay (generator (m/deref schema) options)))
(defmethod -schema-generator :schema [schema options] (generator (m/deref schema) options))
(defmethod -schema-generator ::m/schema [schema options] (generator (m/deref schema) options))

(defmethod -schema-generator :merge [schema options] (generator (m/deref schema) options))
(defmethod -schema-generator :union [schema options] (generator (m/deref schema) options))
(defmethod -schema-generator :select-keys [schema options] (generator (m/deref schema) options))

(defmethod -schema-generator :cat [schema options] (-cat-gen schema options))
(defmethod -schema-generator :catn [schema options] (-cat-gen schema options))
(defmethod -schema-generator :alt [schema options] (-alt-gen schema options))
(defmethod -schema-generator :altn [schema options] (-alt-gen schema options))

(defmethod -schema-generator :? [schema options] (-?-gen schema options))
(defmethod -schema-generator :* [schema options] (-*-gen schema options))
(defmethod -schema-generator :+ [schema options] (gen/not-empty (-*-gen schema options)))
(defmethod -schema-generator :repeat [schema options] (-repeat-gen schema options))

(defn- -create [schema options]
  (let [{:gen/keys [gen fmap elements]} (merge (m/type-properties schema) (m/properties schema))
        gen (or gen (when-not elements (if (satisfies? Generator schema) (-generator schema options) (-schema-generator schema options))))
        elements (when elements (gen/elements elements))]
    (cond
      fmap (gen/fmap (m/eval fmap (or options (m/options schema))) (or elements gen (gen/return nil)))
      elements elements
      gen gen
      :else (miu/-fail! ::no-generator {:schema schema, :options options}))))

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
  ([?gen-or-schema {:keys [seed size] :or {size 30} :as options}]
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

;;
;; functions
;;

(defn function-checker
  ([?schema] (function-checker ?schema nil))
  ([?schema {::keys [=>iterations] :or {=>iterations 100} :as options}]
   (let [schema (m/schema ?schema options)
         check (fn [schema]
                 (let [{:keys [input output]} (m/-function-info schema)
                       input-generator (generator input options)
                       output-validator (m/validator output options)
                       validate (fn [f args] (output-validator (apply f args)))]
                   (fn [f]
                     (let [{:keys [result shrunk]} (->> (prop/for-all* [input-generator] #(validate f %))
                                                        (check/quick-check =>iterations))
                           smallest (-> shrunk :smallest first)]
                       (if-not (true? result)
                         (let [explain-input (m/explain input smallest)
                               response (if-not explain-input
                                          (try (apply f smallest) (catch #?(:clj Exception, :cljs js/Error) e e)))
                               explain-output (if-not explain-input (m/explain output response))]
                           (cond-> shrunk
                                   explain-input (assoc ::explain-input explain-input)
                                   explain-output (assoc ::explain-output explain-output)
                                   (ex-message result) (-> (update :result ex-message)
                                                           (dissoc :result-data)))))))))]
     (condp = (m/-type schema)
       :=> (check schema)
       :function (let [checkers (map #(function-checker % options) (m/-children schema))]
                   (fn [x] (->> checkers (keep #(% x)) (seq))))
       (miu/-fail! ::invalid-function-schema {:type (m/-type schema)})))))
