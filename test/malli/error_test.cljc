(ns malli.error-test
  (:require [clojure.test :refer [deftest testing is are]]
            [malli.error :as me]
            [malli.core :as m]))

(deftest error-message-test
  (let [msg "should be an int"
        fn1 (fn [{:keys [value]} _] (str "should be an int, was " value))
        fn2 '(fn [{:keys [value]} _] (str "should be an int, was " value))]
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

(deftest humanize-test
  (testing "nil if success"
    (is (nil? (-> int?
                  (m/explain 1)
                  (me/humanize {:wrap :message})))))

  (testing "top-level error"
    (is (= "should be int"
           (-> int?
               (m/explain "1")
               (me/humanize {:wrap :message})))))

  (testing "vector"
    (is (= [nil nil [nil "should be int"]]
           (-> [:vector [:vector int?]]
               (m/explain [[1 2] [2 2] [3 "4"]])
               (me/humanize {:wrap :message})))))

  (testing "set"
    (is (= #{#{"should be int"}}
           (-> [:set [:set int?]]
               (m/explain #{#{1} #{"2"}})
               (me/humanize {:wrap :message})))))

  (testing "invalid type"
    (is (= "invalid type"
           (-> [:list int?]
               (m/explain [1])
               (me/humanize {:wrap :message})))))

  (testing "mixed bag"
    (is (= [nil
            {:x [nil "should be int" "should be int"]}
            {:x "invalid type"}]
           (-> [:vector [:map [:x [:vector int?]]]]
               (m/explain
                 [{:x [1 2 3]}
                  {:x [1 "2" "3"]}
                  {:x #{"whatever"}}])
               (me/humanize {:wrap :message}))))))

(deftest humanize-customization-test
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
               :d {:f "invalid"}}]

    (testing "with default locale"
      (is (= {:a "should be int"
              :b "should be positive int"
              :c "STAY POSITIVE",
              :d {:e "missing required key"
                  :f "SHOULD BE ZIP"}}
             (-> (m/explain schema value)
                 (me/humanize {:wrap :message})))))

    (testing "localization is applied, if available"
      (is (= {:a "NUMERO"
              :b "should be positive int"
              :c "POSITIIVINEN",
              :d {:e "PUUTTUVA AVAIN"
                  :f "PITÄISI OLLA NUMERO"}}
             (-> (m/explain schema value)
                 (me/humanize
                   {:wrap :message
                    :locale :fi
                    :errors (-> me/default-errors
                                (assoc-in ['int? :error/message :fi] "NUMERO")
                                (assoc-in [::m/missing-key :error/message :fi] "PUUTTUVA AVAIN"))})))))))

(deftest composing-with-and-test

  (testing "top-level map-schemas are written in :malli/error"
    (let [schema [:and [:map
                        [:x int?]
                        [:y int?]
                        [:z int?]]
                  [:fn {:error/message "(> x y)"}
                   '(fn [{:keys [x y]}] (> x y))]]]

      (is (= {:z "should be int", :malli/error "(> x y)"}
             (-> schema
                 (m/explain {:x 1 :y 2, :z "1"})
                 (me/humanize {:wrap :message})))))

    (testing ":error/path contributes to path"
      (let [schema [:and [:map
                          [:password string?]
                          [:password2 string?]]
                    [:fn {:error/message "passwords don't match"
                          :error/path [:password2]}
                     '(fn [{:keys [password password2]}]
                        (= password password2))]]]

        (is (= {:password2 "passwords don't match"}
               (-> schema
                   (m/explain {:password "secret"
                               :password2 "faarao"})
                   (me/humanize {:wrap :message})))))))

  (testing "on collections, first error wins"
    (let [schema [:and
                  [:vector int?]
                  [:fn {:error/message "first should be positive"}
                   '(fn [[x]] (pos? x))]
                  [:fn {:error/message "first should be positive (masked)"}
                   '(fn [[x]] (pos? x))]]]
      (is (= "first should be positive"
             (-> schema
                 (m/explain [-2 1])
                 (me/humanize {:wrap :message}))))
      (is (= [nil "should be int"]
             (-> schema
                 (m/explain [-2 "1"])
                 (me/humanize {:wrap :message}))))
      (is (= "invalid type"
             (-> schema
                 (m/explain '(-2 "1"))
                 (me/humanize {:wrap :message}))))))

  (testing "on non-collections, first error wins"
    (let [schema [:and
                  [:fn {:error/message "should be >= 1"} '(fn [x] (or (not (int? x)) (>= x 1)))]
                  int?
                  [:fn {:error/message "should be >= 2"} '(fn [x] (or (not (int? x)) (>= x 2)))]]]

      (is (= "should be >= 1"
             (-> schema
                 (m/explain 0)
                 (me/humanize {:wrap :message}))))
      (is (= "should be int"
             (-> schema
                 (m/explain "kikka")
                 (me/humanize {:wrap :message}))))
      (is (= "should be >= 2"
             (-> schema
                 (m/explain 1)
                 (me/humanize {:wrap :message}))))
      (is (= nil
             (-> schema
                 (m/explain 2)
                 (me/humanize {:wrap :message})))))))
