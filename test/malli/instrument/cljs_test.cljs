(ns malli.instrument.cljs-test
  (:require [cljs.test :refer [deftest is testing]]
            [malli.instrument.fn-schemas :refer [sum-nums str-join str-join2 str-join3 str-join4]]
            [malli.core :as m]
            [malli.experimental :as mx]
            [malli.instrument.cljs :as mi]))

(defn plus [x] (inc x))
(m/=> plus [:=> [:cat :int] [:int {:max 6}]])

(def small-int [:int {:max 6}])

(defn minus
  "kukka"
  {:malli/schema [:=> [:cat :int] [:int {:min 6}]]
   :malli/scope #{:input :output}}
  [x] (dec x))

(defn minus-small-int
  "kukka"
  {:malli/schema [:=> [:cat :int] small-int]
   :malli/scope #{:input :output}}
  [x] (dec x))

(defn plus-small-int [x] (inc x))
(m/=> plus-small-int [:=> [:cat :int] small-int])

(def Over100 (m/-simple-schema {:type 'Over100, :pred #(and (int? %) (< 100 %))}))

(defn plus-over-100 [x] (inc x))
(m/=> plus-over-100 [:=> [:cat :int] Over100])

(mx/defn power :- [:int {:max 6}]
  "inlined schema power"
  [x :- :int] (* x x))

(deftest instrument!-test

  (testing "without instrumentation"
    (mi/unstrument! {:filters [(mi/-filter-ns 'malli.instrument.cljs-test)]})

    (is (= "21" (plus "2")))
    (is (= 7 (plus 6)))

    (is (= (plus-small-int 8) 9))
    (is (= (plus-over-100 8) 9))

    (is (= 4 (power "2")))
    (is (= 36 (power 6))))

  (testing "with instrumentation"
    (mi/instrument! {:filters [(mi/-filter-ns 'malli.instrument.cljs-test)]})

    (is (thrown-with-msg? js/Error #":malli.core/invalid-input" (plus "2")))
    (is (thrown-with-msg? js/Error #":malli.core/invalid-output" (plus 6)))

    (is (thrown-with-msg? js/Error #":malli.core/invalid-input" (plus-small-int "2")))
    (is (thrown-with-msg? js/Error #":malli.core/invalid-output" (plus-small-int 8)))

    (is (thrown-with-msg? js/Error #":malli.core/invalid-input" (plus-over-100 "2")))
    (is (thrown-with-msg? js/Error #":malli.core/invalid-output" (plus-over-100 8)))

    (is (thrown-with-msg? js/Error #":malli.core/invalid-input" (power "2")))
    (is (thrown-with-msg? js/Error #":malli.core/invalid-output" (power 6)))))

(mi/collect! {:ns ['malli.instrument.cljs-test 'malli.instrument.fn-schemas]})

(deftest collect!-test

  (testing "with instrumentation"
    (mi/instrument! {:filters [(mi/-filter-ns 'malli.instrument.cljs-test 'malli.instrument.fn-schemas)]})
    (is (thrown-with-msg? js/Error #":malli.core/invalid-input" (str-join [1 "2"])))
    (is (thrown-with-msg? js/Error #":malli.core/invalid-input" (str-join2 [1 "2"])))
    (is (thrown-with-msg? js/Error #":malli.core/invalid-input" (str-join3 [1 "2"])))
    (is (thrown-with-msg? js/Error #":malli.core/invalid-input" (str-join4 [1 "2"])))
    (is (thrown-with-msg? js/Error #":malli.core/invalid-input" (sum-nums "2")))
    (is (thrown-with-msg? js/Error #":malli.core/invalid-input" (minus "2")))
    (is (thrown-with-msg? js/Error #":malli.core/invalid-output" (minus 6)))

    (is (thrown-with-msg? js/Error #":malli.core/invalid-input" (minus-small-int "2")))
    (is (thrown-with-msg? js/Error #":malli.core/invalid-output" (minus-small-int 10))))

  (testing "without instrumentation"
    (mi/unstrument! {:filters [(mi/-filter-ns 'malli.instrument.cljs-test 'malli.instrument.fn-schemas)]})

    (is (= 1 (minus "2")))
    (is (= 5 (minus 6)))
    (is (= (sum-nums [2 "3" 4 5]) "2345"))
    (is (= (str-join [1 "2"]) "12"))
    (is (= (str-join2 [1 "2"]) "12"))
    (is (= (str-join3 [1 "2"]) "12"))
    (is (= (str-join4 [1 "2"]) "12"))

    (is (= 1 (minus-small-int "2")))
    (is (= 9 (minus-small-int 10)))))
