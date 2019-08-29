(ns malli.generator
  (:require [clojure.test.check.generators :as gen]
            [clojure.test.check.random :as random]
            [clojure.test.check.rose-tree :as rose]
            [clojure.spec.gen.alpha :as ga]
            [malli.core :as m]))

(defn- -random [seed]
  (if seed
    (random/make-random seed)
    (random/make-random)))

(defn generate
  ([gen]
   (generate gen nil))
  ([gen {:keys [seed]}]
   (rose/root (gen/call-gen gen (-random seed) 1))))

(defn sample
  ([gen]
   (sample gen nil))
  ([gen {:keys [seed size] :or {size 10}}]
   (->> (gen/make-size-range-seq size)
        (map #(rose/root (gen/call-gen gen %1 %2))
             (gen/lazy-random-states (-random seed)))
        (take size))))

(defmulti -generator (fn [schema opts] (m/name schema opts)) :default ::default)

(defmethod -generator ::default [schema _]
  (ga/gen-for-pred (m/validator schema)))

(defn generator
  ([?schema]
   (generator ?schema nil))
  ([?schema opts]
   (-generator (m/schema ?schema opts) opts)))

;;
;; spike
;;

(sample
  (gen/elements [:a :b :c])
  {:size 10, :seed nil})
; (:b :c :a :a :a :b :b :b :a :c)

(->> m/predicate-registry
     (filter (comp fn? key))
     (map (fn [[f schema]]
            [(-> schema (m/schema) (m/name))
             (-> f (generator) (sample {:size 4, :seed 0}))]))
     (into {}))


(sample (generator string?) {:size 10, :seed 0})
; ("" "e" "wp" "t5" "L" "ho" "K99" "40" "4r3" "y3V8s")

;;
;; [:and int? neg-int?]
;;

; 1) this is not optimal (lot's of misses)
(sample (gen/such-that int? (generator neg-int?)) {:seed 0})
; (-1 -1 -2 -2 -2 -8 -5 -2 -7 -126)

; 2) this would be better
(sample (gen/such-that neg-int? (generator int?)) {:seed 0})
; (-1 -1 -1 -1 -1 -7 -4 -4 -6 -16)

; 3) this would be optiomal. how is the result different to 1???
(sample (generator neg-int?) {:seed 0})
; (-1 -2 -2 -2 -1 -4 -2 -2 -8 -110)
