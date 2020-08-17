(ns malli.error-test
  (:require [clojure.test :refer [deftest testing is are]]
            [malli.error :as me]
            [malli.core :as m]
            [malli.util :as mu]))

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
             [(reify
                m/Schema
                (-type [_])
                (-properties [_])
                (-explainer [this path]
                  (fn [value in acc]
                    (if-not (int? value) (conj acc (m/-error path in this value)) acc)))
                me/SchemaError
                (-error [_] {:error/message "from schema"})) "kikka" "from schema"]
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

(deftest with-spell-checking-test
  (let [get-errors (fn [explanation] (->> explanation :errors (mapv #(select-keys % [:path :type ::me/likely-misspelling-of :message]))))]

    (testing "simple"
      (is (= [{:path [:deliverz]
               :type ::me/misspelled-key
               ::me/likely-misspelling-of [[:deliver]]
               :message "should be spelled :deliver"}]
             (-> [:map
                  [:orders boolean?]
                  [:deliver boolean?]]
                 (mu/closed-schema)
                 (m/explain {:orders true, :deliverz true})
                 (me/with-spell-checking)
                 (me/with-error-messages)
                 (get-errors)))))

    (testing "nested"

      (testing "with defaults"
        (is (= [{:path [0 :address 0 :streetz]
                 :type ::me/misspelled-key
                 ::me/likely-misspelling-of [[0 :address 0 :street1] [0 :address 0 :street2]],
                 :message "should be spelled :street1 or :street2"}]
               (-> [:maybe [:map
                            [:address [:and
                                       [:map
                                        [:street1 string?]
                                        [:street2 string?]]]]]]
                   (mu/closed-schema)
                   (m/explain {:address {:streetz "123"}})
                   (me/with-spell-checking)
                   (me/with-error-messages)
                   (get-errors)))))

      (testing "stripping likely-misspelled-of fields"
        (is (= [{:path [:address :street1]
                 :type ::m/missing-key
                 :message "missing required key"}
                {:path [:address :street2]
                 :type ::m/missing-key
                 :message "missing required key"}
                {:path [:address :streetz]
                 :type ::me/misspelled-key
                 ::me/likely-misspelling-of [[:address :street1] [:address :street2]]
                 :message "should be spelled :street1 or :street2"}]
               (-> [:map
                    [:address [:map
                               [:street1 string?]
                               [:street2 string?]]]]
                   (mu/closed-schema)
                   (m/explain {:address {:streetz "123"}})
                   (me/with-spell-checking {:keep-likely-misspelled-of true})
                   (me/with-error-messages)
                   (get-errors))))))))

(deftest humanize-test
  (testing "nil if success"
    (is (nil? (-> int?
                  (m/explain 1)
                  (me/humanize)))))

  (testing "top-level error"
    (is (= ["should be an int"]
           (-> int?
               (m/explain "1")
               (me/humanize)))))

  (testing "vector"
    (is (= [nil nil [nil ["should be an int"]]]
           (-> [:vector [:vector int?]]
               (m/explain [[1 2] [2 2] [3 "4"]])
               (me/humanize)))))

  (testing "set"
    (is (= #{#{["should be an int"]}}
           (-> [:set [:set int?]]
               (m/explain #{#{1} #{"2"}})
               (me/humanize)))))

  (testing "invalid type"
    (is (= ["invalid type"]
           (-> [:list int?]
               (m/explain [1])
               (me/humanize)))))

  (testing "mixed bag"
    (is (= [nil
            {:x [nil ["should be an int"] ["should be an int"]]}
            {:x ["invalid type"]}]
           (-> [:vector [:map [:x [:vector int?]]]]
               (m/explain
                 [{:x [1 2 3]}
                  {:x [1 "2" "3"]}
                  {:x #{"whatever"}}])
               (me/humanize)))))

  (testing "so nested"
    (is (= {:data [{:x [["should be an int"] nil ["should be an int"]]}
                   {:x [["should be an int"] nil ["should be an int"]]}
                   nil
                   {:x [["should be an int"]]}]}
           (-> [:map [:data [:vector [:map [:x [:vector int?]]]]]]
               (m/explain
                 {:data [{:x ["1" 2 "3"]} {:x ["1" 2 "3"]} {:x [1]} {:x ["1"]} {:x [1]}]})
               (me/humanize)))))

  (testing "disallowed keys in closed maps"
    (is (= {:extra ["disallowed key"]}
           (-> [:map {:closed true} [:x int?]]
               (m/explain {:x 1, :extra "key"})
               (me/humanize)))))

  (testing "multiple errors on same key are accumulated into vector"
    (is (= {:x ["missing required key" "missing required key"]}
           (me/humanize
             {:value {},
              :errors [{:in [:x], :schema [:map [:x int?]], :type ::m/missing-key}
                       {:in [:x], :schema [:map [:x int?]], :type ::m/missing-key}]})))))

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
      (is (= {:a ["should be an int"]
              :b ["should be a positive int"]
              :c ["STAY POSITIVE"],
              :d {:e ["missing required key"]
                  :f ["SHOULD BE ZIP"]}}
             (-> (m/explain schema value)
                 (me/humanize)))))

    (testing "localization is applied, if available"
      (is (= {:a ["NUMERO"]
              :b ["should be a positive int"]
              :c ["POSITIIVINEN"],
              :d {:e ["PUUTTUVA AVAIN"]
                  :f ["PITÄISI OLLA NUMERO"]}}
             (-> (m/explain schema value)
                 (me/humanize
                   {:locale :fi
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

      (is (= {:z ["should be an int"], :malli/error ["(> x y)"]}
             (-> schema
                 (m/explain {:x 1 :y 2, :z "1"})
                 (me/humanize)))))

    (testing ":error/path contributes to path"
      (let [schema [:and [:map
                          [:password string?]
                          [:password2 string?]]
                    [:fn {:error/message "passwords don't match"
                          :error/path [:password2]}
                     '(fn [{:keys [password password2]}]
                        (= password password2))]]]

        (is (= {:password2 ["passwords don't match"]}
               (-> schema
                   (m/explain {:password "secret"
                               :password2 "faarao"})
                   (me/humanize)))))))

  (testing "on collections, first error wins"
    (let [schema [:and
                  [:vector int?]
                  [:fn {:error/message "first should be positive"}
                   '(fn [[x]] (pos? x))]
                  [:fn {:error/message "first should be positive (masked)"}
                   '(fn [[x]] (pos? x))]]]
      (is (= ["first should be positive"]
             (-> schema
                 (m/explain [-2 1])
                 (me/humanize))))
      (is (= [nil ["should be an int"]]
             (-> schema
                 (m/explain [-2 "1"])
                 (me/humanize))))
      (is (= ["invalid type"]
             (-> schema
                 (m/explain '(-2 "1"))
                 (me/humanize))))))

  (testing "on non-collections, first error wins"
    (let [schema [:and
                  [:fn {:error/message "should be >= 1"} '(fn [x] (or (not (int? x)) (>= x 1)))]
                  int?
                  [:fn {:error/message "should be >= 2"} '(fn [x] (or (not (int? x)) (>= x 2)))]]]

      (is (= ["should be >= 1"]
             (-> schema
                 (m/explain 0)
                 (me/humanize))))
      (is (= ["should be an int"]
             (-> schema
                 (m/explain "kikka")
                 (me/humanize))))
      (is (= ["should be >= 2"]
             (-> schema
                 (m/explain 1)
                 (me/humanize))))
      (is (= nil
             (-> schema
                 (m/explain 2)
                 (me/humanize)))))))

(deftest string-test
  (is (= {:a ["should be a string"],
          :b ["should be at least 1 characters"],
          :c ["should be at most 4 characters"],
          :d ["should be between 1 and 4 characters"],
          :e ["should be a string"]
          :f ["should be 4 characters"]}
         (-> [:map
              [:a :string]
              [:b [:string {:min 1}]]
              [:c [:string {:max 4}]]
              [:d [:string {:min 1, :max 4}]]
              [:e [:string {:min 1, :max 4}]]
              [:f [:string {:min 4, :max 4}]]]
             (m/explain
               {:a 123
                :b ""
                :c "invalid"
                :d ""
                :e 123
                :f "invalid"})
             (me/humanize)))))

(deftest int-test
  (is (= {:a ["should be an integer"]
          :b ["should be at least 1"]
          :c ["should be at most 4"]
          :d ["should be between 1 and 4"]
          :e ["should be an integer"]
          :f ["should be 4"]}
         (-> [:map
              [:a :int]
              [:b [:int {:min 1}]]
              [:c [:int {:max 4}]]
              [:d [:int {:min 1, :max 4}]]
              [:e [:int {:min 1, :max 4}]]
              [:f [:int {:min 4, :max 4}]]]
             (m/explain
               {:a "123"
                :b 0
                :c 5
                :d 0
                :e "123"
                :f 5})
             (me/humanize)))))

(deftest re-test
  (testing "success"
    (is (= nil
           (-> [:re #"bla"]
               (m/explain "bla")
               (me/humanize)))))
  (testing "failure"
    (is (= ["should match regex"]
           (-> [:re "#bla"]
               (m/explain "gogo")
               (me/humanize))))))

(deftest enum-test
  (testing "success"
    (is (= nil
           (-> [:enum "foo" "bar"]
               (m/explain "foo")
               (me/humanize)))))
  (testing "error with 1 value"
    (is (= ["should be foo"]
           (-> [:enum "foo"]
               (m/explain "baz")
               (me/humanize)))))
  (testing "error with 2 values"
    (is (= ["should be either foo or bar"]
           (-> [:enum "foo" "bar"]
               (m/explain "baz")
               (me/humanize)))))
  (testing "more than 2 values"
    (is (= ["should be either foo, bar or buzz"]
           (-> [:enum "foo" "bar" "buzz"]
               (m/explain "baz")
               (me/humanize))))
    (is (= ["should be either foo, bar, buzz or biff"]
           (-> [:enum "foo" "bar" "buzz" "biff"]
               (m/explain "baz")
               (me/humanize))))))
