(ns malli.constraint.generator-test
  (:require [clojure.test :refer [are deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :refer [for-all]]
            [malli.core :as m]
            [malli.generator :as mg]
            [malli.json-schema-test :as json-schema-test]
            [malli.util :as mu]
            #?(:clj  [malli.test-macros :refer [when-env]]
               :cljs ["@js-joda/timezone/dist/js-joda-timezone-10-year-range"]))
  #?(:cljs (:require-macros [malli.test-macros :refer [when-env]])))

;; these generators don't work well with :and but do with constraints
;; because the generators are much more specific. they also grow
;; and shrink better because such-that fails a lot less often
;; and the size remains more consistent.
(deftest int-constraint-generator-test
  (is (thrown?
        #?(:clj Exception, :cljs js/Error)
        (dotimes [_ 10] (doall (mg/sample [:and int? [:> 739] [:< 741]])))))
  (is (= 740 (mg/generate [:int {:> 739 :< 741}])))
  (is (= 740 (mg/generate [:int {:and [[:not [:<= 739]]
                                       [:not [:>= 741]]]}])))
  (dotimes [_ 100]
    (is (every? #{740}
                (mg/sample [:int {:> 739 :< 741}]
                           {:size 1000}))))
  (is (thrown?
        #?(:clj Exception, :cljs js/Error)
        (dotimes [_ 10]
          (doall (mg/sample [:and int? [:> 10] [:< 100]]
                            {:size 1000})))))
  (is (doall (mg/sample [:int {:> 10 :< 100}]
                        {:seed 123})))
  (is (= (mg/sample [:int {:> 10 :< 100}]
                    {:size 1000
                     :seed 0})
         (mg/sample [:int {:gen/> 10 :gen/< 100}]
                    {:size 1000
                     :seed 0}))))

(deftest double-constraint-generator-test
  (is (thrown?
        #?(:clj Exception, :cljs js/Error)
        (dotimes [_ 10] (doall (mg/sample [:and :double [:> 739] [:< 741]])))))
  (dotimes [_ 10]
    (let [vs (mg/sample [:double {:> 739.000001 :< 739.000002}]
                        {:size 1000})]
      (is (< 500 (count (distinct vs))))
      (is (every? #(< 739.000001 % 739.000002)
                  vs))))
  (dotimes [_ 10]
    (let [vs (mg/sample [:double {:and [[:not [:<= 739.000001]]
                                        [:not [:>= 739.000002]]]}]
                        {:size 1000})]
      (is (< 500 (count (distinct vs))))
      (is (every? #(< 739.000001 % 739.000002)
                  vs))))
  (is (= (mg/sample [:double {:> 10 :< 100}]
                    {:size 1000
                     :seed 0})
         (mg/sample [:double {:gen/> 10 :gen/< 100}]
                    {:size 1000
                     :seed 0}))))

(deftest string-constraint-generate-test
  (testing ":alphanumeric + :alpha :numeric"
    (is (= ["qBoTBneKUb" "RznfuEdSsmp" "pwbdaMNTYxxH" "MtCxHNxEiZPJ" "pRXpSisHqog"
            "QhEHYYfiswSl" "XKYeOExmpprzg" "xMwVWpPDVlAp" "iMrMJGWzETClJ" "syJVWCJeAOPmABe"]
           (vec (mg/sample [:string {:min 10 :alpha true}]
                           {:seed 0}))
           (vec (mg/sample [:string {:min 10 :alpha true :alphanumeric true}]
                           {:seed 0}))))
    (is (= ["0512069087" "81635196348" "416649118043" "105571456853" "94201363561" "815573842818"
            "2789823848768" "650900047134" "4183700601041" "300407523083076"]
           (vec (mg/sample [:string {:min 10 :numeric true}]
                           {:seed 0}))
           (vec (mg/sample [:string {:min 10 :numeric true :alphanumeric true}]
                           {:seed 0}))))
    (is (= ["5" "42" "" "" "" "W" "3" "0" "" "Gr"]
           (vec (mg/sample [:string {:not [:min 3]}]
                           {:seed 1}))))
    (is (every? seq (mg/sample [:string {:not [:max 0]}])))
    ; [:or [:not [:min 1]] [:not [:max 1]]]
    ; [:or [:max 0] [:min 2]]
    ;;FIXME why aren't we seeing min=2 strings?
    (is (every? empty? (mg/sample [:string {:not [:and [:min 1] [:max 1]]}]
                                  {:size 1000})))
    ;;TODO what's the right answer here?
    ;(is (every? empty? (mg/sample [:string {:not [:and [:min 2] [:max 2]]}])))
    (is (thrown-with-msg?
          #?(:clj Exception, :cljs js/Error)
          #":malli\.generator/unsatisfiable-string-constraint"
          (mg/generate [:string {:not [:min 0]}])))
    (is (thrown-with-msg?
          #?(:clj Exception, :cljs js/Error)
          #":malli\.generator/unsatisfiable-string-constraint"
          (mg/generate [:string {:not [:and [:min 0] [:max 10]]}])))
    (is (= "5833307285"
           (mg/generate [:string {:min 10 :not :alpha}]
                        {:seed 0})))
    (is (thrown-with-msg?
          #?(:clj Exception, :cljs js/Error)
          #":malli\.generator/unsatisfiable-string-constraint"
          (mg/generate [:string {:min 10 :numeric true :alpha true}]))))
  (testing ":non-alpha"
    (is (= "5833307285"
           (mg/generate [:string {:min 10 :non-alpha true}]
                        {:seed 0}))))
  (testing ":includes"
    (is (= "54T0oJ7NCbWYeLkvm84iwiZblahblah"
           (mg/generate [:string {:min 10 :includes "blah"}]
                        {:seed 0})))))
