(ns malli.error-test
  (:require [clojure.test :refer [deftest testing is are]]
            [malli.error :as me]
            [malli.core :as m]
            [clojure.walk :as walk]))

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

(deftest merge-errors-test
  (let [schema [:map
                [:a int?]
                [:b pos-int?]
                [:c [pos-int? {:error/message "STAY POSITIVE"
                               :error/fn {:fi '(constantly "POSITIIVINEN")}}]]
                [:d
                 [:map
                  [:e any?]
                  [:f [int? {:error/message {:en "SHOULD BE ZIP", :fi "PITÄISI OLLA NUMERO"}}]]]]]
        value {:a "invalid"
               :b "invalid"
               :c "invalid"
               :d {:f "invalid"}}
        with-just-messages (fn [x]
                            (walk/prewalk
                              (fn [x]
                                (if (m/error? x)
                                  (:message x)
                                  x)) x))]

    (testing "with default locale"
      (is (= {:a "should be int"
              :b "should be positive int"
              :c "STAY POSITIVE",
              :d {:e "missing required key"
                  :f "SHOULD BE ZIP"}}
             (-> (m/explain schema value)
                 (me/check)
                 (with-just-messages)))))

    (testing "localization is applied, if available"
      (is (= {:a "NUMERO"
              :b "should be positive int"
              :c "POSITIIVINEN",
              :d {:e "PUUTTUVA AVAIN"
                  :f "PITÄISI OLLA NUMERO"}}
             (-> (m/explain schema value)
                 (me/check
                   {:locale :fi
                    :errors (-> me/default-errors
                                (assoc-in ['int? :error/message :fi] "NUMERO")
                                (assoc-in [::m/missing-key :error/message :fi] "PUUTTUVA AVAIN"))})
                 (with-just-messages)))))))
