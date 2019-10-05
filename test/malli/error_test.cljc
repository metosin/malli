(ns malli.error-test
  (:require [clojure.test :refer [deftest testing is are]]
            [malli.error :as me]
            [malli.core :as m]))

(deftest error-message-test
  (let [msg "should be an int"
        fn1 (fn [_ value _] (str "should be an int, was " value))
        fn2 '(fn [_ value _] (str "should be an int, was " value))]
    (doseq [[schema value message opts]
            [;; via schema
             [[int? {:error/message msg}] "kikka" "should be an int"]
             [[int? {:error/fn fn1}] "kikka" "should be an int, was kikka"]
             [[int? {:error/fn fn2}] "kikka" "should be an int, was kikka"]
             [[int? {:error/message msg, :error/fn fn2}] "kikka" "should be an int, was kikka"]
             ;; via defaults
             [[int?] "kikka" "should be an int" {:errors {'int? {:error/message msg}}}]
             [[int?] "kikka" "should be an int, was kikka" {:errors {'int? {:error/fn fn1}}}]
             [[int?] "kikka" "should be an int, was kikka" {:errors {'int? {:error/fn fn2}}}]
             [[int?] "kikka" "should be an int, was kikka" {:errors {'int? {:error/message msg, :error/fn fn2}}}]
             ;; both
             [[int?
               {:error/message msg, :error/fn fn2}]
              "kikka" "should be an int, was kikka"
              {:errors {'int? {:error/message "fail1", :error/fn (constantly "fail2")}}}]]]
      (is (= message (-> (m/explain schema value) :errors first (me/error-message opts)))))))

;; FIXME
#_(deftest merge-errors-test
    (let [schema [:map
                [:a int?]
                [:b pos-int?]
                [:c [pos-int? {:error/message "stay positive"}]]
                [:d
                 [:map
                  [:e any?]
                  [:f [int? {:error/message {:en "should be zip", :fi "pitäisi olla numero"}}]]]]]
        error? (partial me/->SchemaError "invalid")]

      (testing "with default locale"
        (is (= {:a (error? "should be an int")
              :b (error? "unknown error")
              :c (error? "stay positive"),
              :d {:f (error? "should be zip"),
                  :e (me/->SchemaError nil "missing required key")}}
               (-> (m/explain
                   schema
                   {:a "invalid"
                    :b "invalid"
                    :c "invalid"
                    :d {:f "invalid"}})
                   (me/check)))))

      (testing "localization is applied, if available"
        (is (= {:a (error? "should be an int")
              :b (error? "unknown error")
              :c (error? "stay positive"),
              :d {:f (error? "pitäisi olla numero"),
                  :e (me/->SchemaError nil "missing required key")}}
               (-> (m/explain
                   schema
                   {:a "invalid"
                    :b "invalid"
                    :c "invalid"
                    :d {:f "invalid"}})
                   (me/check {:locale :fi})))))))
