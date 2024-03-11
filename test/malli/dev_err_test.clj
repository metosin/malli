(ns malli.dev-err-test
  (:require [clojure.test :refer [deftest is testing]]
            [malli.dev :as dev]))

(defn plus-err
  [x] (inc x))

(defn ->plus-err [] plus-err)

(deftest start!-err-test
  (testing "malli.dev/start!"
    (testing "without starting"
      (is (thrown? ClassCastException ((->plus-err) "2")))
      (is (= 7 ((->plus-err) 6))))

    (testing "instrumentation shema error when starting"
      ;; append metadata only during test to prevent conflicts with other tests
      (alter-meta! #'plus-err #(assoc % :malli/schema [:=> [:cat [:vector]] [:int {:max 6}]]))
      (try
        (is (thrown-with-msg?
             Exception #":malli.core/register-function-schema"
             (dev/start! {:ns *ns*, :report (fn [& _args])})))
        (finally
          (with-out-str (dev/stop!))))
      (alter-meta! #'plus-err #(dissoc % :malli/schema)))))
