(ns malli.dev.cljs-test
  (:require [clojure.test :refer [deftest is testing]]
            [malli.core :as m]
            [malli.dev.cljs :as md]
            [malli.instrument :as mi]
            [malli.dev.pretty :as pretty]))

(defn plus
  {:malli/schema [:=> [:cat :int] [:int {:max 6}]]}
  [x] (inc x))

(defn ->plus [] plus)

(deftest ^:simple start!-test
  (testing "malli.dev.cljs/start!"
    (testing "without starting"
      (is (= "21" ((->plus) "2")))
      (is (= 7 ((->plus) 6))))

    (testing "instrumentation after starting"
      (md/start! {:ns 'malli.dev.cljs-test :filters [(mi/-filter-ns 'malli.dev.cljs-test)]})
      (is (thrown-with-msg? js/Error #":malli.core/invalid-input" ((->plus) "2")))
      (is (thrown-with-msg? js/Error #":malli.core/invalid-output" ((->plus) 6)))
      (m/-deregister-metadata-function-schemas! :cljs)
      (mi/unstrument! {:filters [(mi/-filter-ns 'malli.dev.cljs-test)]}))

    (testing "instrumentation after starting with reporter"
      (md/start! {:ns 'malli.dev.cljs-test :report (pretty/thrower) :filters [(mi/-filter-ns 'malli.dev.cljs-test)]})
      (is (thrown-with-msg? js/Error #"Invalid function arguments" ((->plus) "2")))
      (is (thrown-with-msg? js/Error #"Invalid function return value" ((->plus) 6)))
      (m/-deregister-metadata-function-schemas! :cljs)
      (mi/unstrument! {:filters [(mi/-filter-ns 'malli.dev.cljs-test)]}))))
