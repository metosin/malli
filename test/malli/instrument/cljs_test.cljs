(ns malli.instrument.cljs-test
  (:require
    [cljs.test :refer [deftest is testing]]
    [malli.core :as m]
    [malli.instrument.cljs :as mi]))

(defn plus [x] (inc x))
(m/=> plus [:=> [:cat :int] [:int {:max 6}]])

(defn ->plus [] plus)

(defn minus
  "kukka"
  {:malli/schema [:=> [:cat :int] [:int {:min 6}]]
   :malli/scope  #{:input :output}}
  [x] (dec x))

(defn ->minus [] minus)

(deftest instrument!-test

  (testing "without instrumentation"
    (mi/unstrument! {:filters [(mi/-filter-ns 'malli.instrument.cljs-test)]})
    (is (= "21" ((->plus) "2")))
    (is (= 7 ((->plus) 6))))

  (testing "with instrumentation"
    (mi/instrument! {:filters [(mi/-filter-ns 'malli.instrument.cljs-test)]})
    (is (thrown-with-msg? js/Error #":malli.core/invalid-input" ((->plus) "2")))
    (is (thrown-with-msg? js/Error #":malli.core/invalid-output" ((->plus) 6)))))

(mi/collect!)

(deftest collect!-test

  (testing "without instrumentation"

    (mi/unstrument! {:filters [(mi/-filter-ns 'malli.instrument.cljs-test)]})
    (is (= 1 ((->minus) "2")))
    (is (= 5 ((->minus) 6))))

  (testing "with instrumentation"
    (mi/instrument! {:filters [(mi/-filter-ns 'malli.instrument.cljs-test)]})
    (is (thrown-with-msg? js/Error #":malli.core/invalid-input" ((->minus) "2")))
    (is (thrown-with-msg? js/Error #":malli.core/invalid-output" ((->minus) 6)))))
