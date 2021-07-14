(ns malli.instrument-test
  (:require [clojure.test :refer [deftest testing is]]
            [malli.instrument :as mi]
            [malli.core :as m]))

(defn plus [x] (inc x))
(m/=> plus [:=> [:cat :int] [:int {:max 6}]])

(defn ->plus [] plus)

(defn unstrument! [] (with-out-str (mi/unstrument! {:filters [(mi/-filter-ns 'malli.instrument-test)]})))
(defn instrument! [] (with-out-str (mi/instrument! {:filters [(mi/-filter-ns 'malli.instrument-test)]})))
(defn collect! [] (mi/collect! {:ns *ns*}))

(deftest instrument!-test

  (testing "without instrumentation"
    (unstrument!)
    (is (thrown?
          ClassCastException
          ((->plus) "2")))
    (is (= 7 ((->plus) 6))))

  (testing "with instrumentation"
    (instrument!)
    (is (thrown-with-msg?
          Exception
          #":malli.core/invalid-input"
          ((->plus) "2")))
    (is (thrown-with-msg?
          Exception
          #":malli.core/invalid-output"
          ((->plus) 6)))))

(defn minus
  "kukka"
  {:malli/schema [:=> [:cat :int] [:int {:min 6}]]
   :malli/scope #{:input :output}}
  [x] (dec x))

(defn ->minus [] minus)

(collect!)

(deftest collect!-test

  (testing "without instrumentation"
    (unstrument!)
    (is (thrown?
          ClassCastException
          ((->minus) "2")))
    (is (= 5 ((->minus) 6))))

  (testing "with instrumentation"
    (instrument!)
    (is (thrown-with-msg?
          Exception
          #":malli.core/invalid-input"
          ((->minus) "2")))
    (is (thrown-with-msg?
          Exception
          #":malli.core/invalid-output"
          ((->minus) 6)))))
