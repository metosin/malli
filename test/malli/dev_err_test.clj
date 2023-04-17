(ns malli.dev-err-test
  (:require [clojure.test :refer [deftest is testing]]
            [malli.dev :as md]
            [malli.dev.pretty :as pretty]))

(defn plus-err
  {:malli/schema [:=> [:cat [:vector]] [:int {:max 6}]]}
  [x] (inc x))

(defn ->plus-err [] plus-err)

(deftest start!-err-test
  (testing "malli.dev/start!"
    (testing "without starting"
      (is (thrown? ClassCastException ((->plus-err) "2")))
      (is (= 7 ((->plus-err) 6))))

    (testing "instrumentation shema error when starting"
      (is (thrown-with-msg?
            Exception #"Schema error when insrumenting function: malli.dev-err-test/plus-err - :malli.core/child-error"
            (md/start! {:ns *ns*})))
      (md/stop!))))

