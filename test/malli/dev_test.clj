(ns malli.dev-test
  (:require [clojure.test :refer [deftest is testing]]
            [malli.dev :as md]
            [malli.dev.pretty :as pretty]))

(defn plus
  {:malli/schema [:=> [:cat :int] [:int {:max 6}]]}
  [x] (inc x))

(defn ->plus [] plus)

(deftest start!-test
  (testing "malli.dev/start!"
    (testing "without starting"
      (is (thrown? ClassCastException ((->plus) "2")))
      (is (= 7 ((->plus) 6))))

    (testing "instrumentation after starting"
      (md/start! {:ns *ns*})
      (is (thrown-with-msg? Exception #":malli.core/invalid-input" ((->plus) "2")))
      (is (thrown-with-msg? Exception #":malli.core/invalid-output" ((->plus) 6)))
      (md/stop!))

    (testing "instrumentation after starting with reporter"
      (md/start! {:ns *ns* :report (pretty/thrower)})
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Invalid function arguments" ((->plus) "2")))
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Invalid function return value" ((->plus) 6)))
      (md/stop!))))
