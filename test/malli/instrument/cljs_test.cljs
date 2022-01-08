(ns malli.instrument.cljs-test
  (:require
    [cljs.test :refer [deftest is testing]]
    [malli.core :as m]
    [malli.instrument.cljs :as mi]))

(defn plus [x] (inc x))
(m/=> plus [:=> [:cat :int] [:int {:max 6}]])

(def small-int [:int {:max 6}])

(defn ->plus [] plus)

(defn minus
  "kukka"
  {:malli/schema [:=> [:cat :int] [:int {:min 6}]]
   :malli/scope  #{:input :output}}
  [x] (dec x))

(defn minus-small-int
  "kukka"
  {:malli/schema [:=> [:cat :int] small-int]
   :malli/scope  #{:input :output}}
  [x] (dec x))

(defn ->minus [] minus)

(defn plus-small-int [x] (inc x))
(m/=> plus-small-int [:=> [:cat :int] small-int])

(def Over100 (m/-simple-schema {:type 'Over100, :pred #(and (int? %) (< 100 %))}))

(defn plus-over-100 [x] (inc x))
(m/=> plus-over-100 [:=> [:cat :int] Over100])

(deftest instrument!-test

  (testing "without instrumentation"
    (mi/unstrument! {:filters [(mi/-filter-ns 'malli.instrument.cljs-test)]})
    (is (= "21" ((->plus) "2")))
    (is (= (plus-small-int 8) 9))
    (is (= (plus-over-100 8) 9))
    (is (= 7 ((->plus) 6))))

  (testing "with instrumentation"
    (mi/instrument! {:filters [(mi/-filter-ns 'malli.instrument.cljs-test)]})
    (is (thrown-with-msg? js/Error #":malli.core/invalid-input" (plus-small-int "2")))
    (is (thrown-with-msg? js/Error #":malli.core/invalid-output" (plus-small-int 8)))

    (is (thrown-with-msg? js/Error #":malli.core/invalid-input" (plus-over-100 "2")))
    (is (thrown-with-msg? js/Error #":malli.core/invalid-output" (plus-over-100 8)))

    (is (thrown-with-msg? js/Error #":malli.core/invalid-input" ((->plus) "2")))
    (is (thrown-with-msg? js/Error #":malli.core/invalid-output" ((->plus) 6)))))

(mi/collect!)

(deftest collect!-test

  (testing "without instrumentation"

    (mi/unstrument! {:filters [(mi/-filter-ns 'malli.instrument.cljs-test)]})
    (is (= 1 ((->minus) "2")))
    (is (= 5 ((->minus) 6)))

    (is (= 1 (minus-small-int "2")))
    (is (= 9 (minus-small-int 10))))

  (testing "with instrumentation"
    (mi/instrument! {:filters [(mi/-filter-ns 'malli.instrument.cljs-test)]})
    (is (thrown-with-msg? js/Error #":malli.core/invalid-input" ((->minus) "2")))
    (is (thrown-with-msg? js/Error #":malli.core/invalid-output" ((->minus) 6)))

    (is (thrown-with-msg? js/Error #":malli.core/invalid-input" (minus-small-int "2")))
    (is (thrown-with-msg? js/Error #":malli.core/invalid-output" (minus-small-int 10)))))
