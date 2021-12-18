(ns malli.error-test
  (:require [clojure.test :refer [deftest testing is are]]
            [malli.core-test]
            [malli.error :as me]
            [malli.core :as m]
            [malli.util :as mu]
            [malli.generator :as mg]))

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
              {:errors {'int? {:error/message "fail1", :error/fn (constantly "fail2")}}}]
             ;; type-properties
             [malli.core-test/Over6 5 "should be over 6"]]]
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
           (-> [:set int?]
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

  (testing "multiple errors on same key are preserved"
    (is (= {:x ["missing required key" "missing required key"]}
           (me/humanize
             {:value {},
              :errors [{:in [:x], :schema [:map [:x int?]], :type ::m/missing-key}
                       {:in [:x], :schema [:map [:x int?]], :type ::m/missing-key}]}))))

  (testing "maps can have top level errors and key errors"
    (is (= {:person {:malli/error ["should be a seq"],
                     :name ["missing required key"]}}
           (-> [:map [:person [:and [:map [:name string?]] seq?]]]
               (m/explain {:person {}})
               (me/humanize)))))

  (testing "maps have errors inside"
    (is (= {:person ["should be a seq"]}
           (-> [:map [:person seq?]]
               (m/explain {:person {}})
               (me/humanize))))))

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

(deftest sci-not-available-test
  (testing "sci not available"
    (let [schema (m/schema [:string {:error/fn '(constantly "FAIL")}] {::m/disable-sci true})]
      (is (thrown-with-msg?
            #?(:clj Exception, :cljs js/Error)
            #":malli.core/sci-not-available"
            (-> schema (m/explain ::invalid) (me/with-error-messages))))
      (is (thrown-with-msg?
            #?(:clj Exception, :cljs js/Error)
            #":malli.core/sci-not-available"
            (-> schema (m/explain ::invalid) (me/humanize))))
      (is (thrown-with-msg?
            #?(:clj Exception, :cljs js/Error)
            #":malli.core/sci-not-available"
            (-> [:string {:error/fn '(constantly "FAIL")}]
                (m/explain ::invalid)
                (me/with-error-messages {::m/disable-sci true}))))
      (is (thrown-with-msg?
            #?(:clj Exception, :cljs js/Error)
            #":malli.core/sci-not-available"
            (-> [:string {:error/fn '(constantly "FAIL")}]
                (m/explain ::invalid)
                (me/humanize {::m/disable-sci true}))))
      (testing "direct options win"
        (is (-> schema (m/explain ::invalid) (me/with-error-messages {::m/disable-sci false})))
        (is (-> schema (m/explain ::invalid) (me/humanize {::m/disable-sci false})))))))

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

  (testing "on collections"
    (let [schema [:and
                  [:vector int?]
                  [:fn {:error/message "error1"} '(fn [[x]] (pos? x))]
                  [:fn {:error/message "error2"} '(fn [[x]] (pos? x))]]]
      (testing "value errors are reported over extra top-level errpors"
        (is (= [nil ["should be an int"]]
               (-> schema
                   (m/explain [-2 "1"])
                   (me/humanize)))))
      (testing "without value errors, all top-level errors are collected"
        (is (= ["error1" "error2"]
               (-> schema
                   (m/explain [-2 1])
                   (me/humanize))))
        (is (= ["invalid type" "error1" "error2"]
               (-> schema
                   (m/explain '(-2 "1"))
                   (me/humanize)))))))

  (testing "on non-collections, all errors are collectd"
    (let [schema [:and
                  [:fn {:error/message "should be >= 1"} '(fn [x] (or (not (int? x)) (>= x 1)))]
                  int?
                  [:fn {:error/message "should be >= 2"} '(fn [x] (or (not (int? x)) (>= x 2)))]]]

      (is (= ["should be >= 1" "should be >= 2"]
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

(deftest double-test
  (is (= {:a ["should be a double"]
          :b ["should be at least 1"]
          :c ["should be at most 4"]
          :d ["should be between 1 and 4"]
          :e ["should be a double"]
          :f ["should be 4"]}
         (-> [:map
              [:a :double]
              [:b [:double {:min 1}]]
              [:c [:double {:max 4}]]
              [:d [:double {:min 1, :max 4}]]
              [:e [:double {:min 1, :max 4}]]
              [:f [:double {:min 4, :max 4}]]]
             (m/explain
               {:a "123"
                :b 0.0
                :c 5.0
                :d 0.0
                :e "123"
                :f 5.0})
             (me/humanize)))))

(deftest any-test
  (testing "success"
    (is (= nil
           (-> :any
               (m/explain "bla")
               (me/humanize))))))

(deftest nil-test
  (testing "success"
    (is (= nil
           (-> :nil
               (m/explain nil)
               (me/humanize)))))
  (testing "failure"
    (is (= ["should be nil"]
           (-> :nil
               (m/explain "gogo")
               (me/humanize))))))

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

(deftest function-test
  (is (= ["invalid function"]
         (-> [:=> [:cat int? int?] int?]
             (m/explain malli.core-test/single-arity {::m/function-checker mg/function-checker})
             (me/humanize))))
  (is (= ["invalid function"]
         (-> [:=> [:cat int? int?] int?]
             (m/explain 123)
             (me/humanize)))))

(deftest ifn-test
  (is (= ["should be an ifn"]
         (me/humanize (m/explain ifn? 123)))))

(deftest multi-error-test
  (let [schema [:multi {:dispatch :type}
                ["plus" [:map [:value int?]]]
                ["minus" [:map [:value int?]]]]]

    (is (= {:type ["invalid dispatch value"]}
           (-> schema
               (m/explain {:type "minuz"})
               (me/humanize))))

    (is (= {:type ["did you mean minus"]}
           (-> schema
               (m/explain {:type "minuz"})
               (me/with-spell-checking)
               (me/humanize))))))

(deftest explain-sequential
  (is (= [{:x ["missing required key"]}
          {:x ["missing required key"]}]
         (-> (m/explain
               [:sequential [:map [:x [:sequential [:map [:y number?]]]]]]
               '({:a 10}
                 {:b 10}))
             (me/humanize)))))

(deftest util-schemas-test
  (let [registry (merge (m/default-schemas) (mu/schemas))]
    (doseq [[schema errors] [[[:merge {:title "merge"}
                               [:map [:x int?] [:y int?]]
                               [:map [:z int?]]]
                              {:y ["missing required key"]
                               :z ["missing required key"]}]
                             [[:union {:title "union"}
                               [:map [:x int?] [:y int?]]
                               [:map [:x string?]]]
                              {:y ["missing required key"]}]
                             [[:select-keys {:title "select-keys"}
                               [:map [:x int?] [:y int?]]
                               [:x]]]]
            :let [schema (m/schema schema {:registry registry})]]
      (is (= errors (-> schema (m/explain {:x 1}) (me/humanize)))))))

(deftest sequence-test
  (is (= [nil ["end of input"]]
         (-> [:cat int? int?]
             (m/explain [1])
             (me/humanize))))
  (is (= [nil nil ["input remaining"]]
         (-> [:cat int? int?]
             (m/explain [1 2 3])
             (me/humanize))))
  (is (= [nil nil ["should be an int" "should be a string" "input remaining"]]
         (-> [:cat int? int? [:? int?] [:? string?]]
             (m/explain [1 2 :foo])
             (me/humanize)))))

(deftest error-definion-lookup-test
  (is (= {:foo ["should be an integer"]}
         (-> [:map
              [:foo :int]]
             (m/explain {:foo "1"})
             (me/humanize {:resolve me/-resolve-root-error}))))

  (is (= {:foo ["entry-failure"]}
         (-> [:map
              [:foo {:error/message "entry-failure"} :int]]
             (m/explain {:foo "1"})
             (me/humanize {:resolve me/-resolve-root-error}))))

  (is (= ["map-failure"]
         (-> [:map {:error/message "map-failure"}
              [:foo {:error/message "entry-failure"} :int]]
             (m/explain {:foo "1"})
             (me/humanize {:resolve me/-resolve-root-error}))))

  (testing "entry sees child schema via :error/fn"
    (is (= {:foo ["failure"]}
           (-> [:map
                [:foo {:error/fn (fn [{:keys [schema]} _]
                                   (-> schema m/properties :reason))} [:int {:reason "failure"}]]]
               (m/explain {:foo "1"})
               (me/humanize {:resolve me/-resolve-root-error})))))

  (testing "enum #553"
    (is (= {:a ["should be either a or b"]}
           (-> [:map
                [:a [:enum "a" "b"]]]
               (m/explain {:a nil})
               (me/humanize {:resolve me/-resolve-root-error})))))

  (testing "find over non-maps"
    (is (= [["should be an integer"]]
           (-> [:sequential [:and :int]]
               (m/explain [1 "2"])
               (me/humanize {:resolve me/-resolve-root-error})))))

  (testing "correct paths"
    (is (= ["should be an integer" "should be an integer" "should be an integer"]
           (me/humanize
             (m/explain [:and [:and :int :int :int]] "2")
             {:resolve me/-resolve-direct-error})
           (me/humanize
             (m/explain [:and [:and :int :int :int]] "2")
             {:resolve me/-resolve-root-error}))))

  (testing "collecting all properties"
    (are [schema expected]
      (let [{:keys [errors] :as error} (m/explain schema {:foo "1"})]
        (is (= [expected] (map #(me/-resolve-root-error error % nil) errors))))

      ;; direct
      [:map [:foo [:int {:error/message "direct-failure" ::level :warn}]]]
      [[:foo]
       "direct-failure"
       {:error/message "direct-failure", ::level :warn}]

      ;; entry
      [:map [:foo {:error/message "entry-failure" ::level :warn} :int]]
      [[:foo]
       "entry-failure"
       {:error/message "entry-failure", ::level :warn}]

      ;; one up
      [:map {:error/message "map-failure" ::level :warn} [:foo :int]]
      [[]
       "map-failure"
       {:error/message "map-failure", ::level :warn}])))

(deftest limits
  (is (= {:a [["should be an int"]]
          :b ["should have at least 2 elements"]
          :c ["should have at most 5 elements"]
          :d ["should have between 2 and 5 elements"]
          :e ["should have between 2 and 5 elements"]
          :f ["should have 5 elements"]}
         (-> [:map
              [:a [:vector int?]]
              [:b [:vector {:min 2} int?]]
              [:c [:vector {:max 5} int?]]
              [:d [:vector {:min 2, :max 5} int?]]
              [:e [:vector {:min 2, :max 5} int?]]
              [:f [:vector {:min 5, :max 5} int?]]]
             (m/explain
               {:a ["123"]
                :b [1]
                :c [1 2 3 4 5 6]
                :d [1]
                :e [1.2]
                :f [1 2 3 4]})
             (me/humanize)))))

(defrecord Horror [])

(deftest robust-humanize-form
  (let [f (fn [s] [:fn {:error/message s} (constantly false)])
        => ::irrelevant]
    (are [schema value _ expected]
      (is (= expected (-> (m/explain schema value) (me/humanize))))

      ;; simple cases
      :any :any => nil
      [:and :any :any] :any => nil
      [:and (f "1") :any] :any => ["1"]
      [:and (f "1") (f "1") :any] :any => ["1" "1"]
      [:and (f "1") (f "2")] {:a :map} => ["1" "2"]

      ;; accumulate into maps if error shape is already a map
      [:map [:x [:and [:map [:y :any]] seq?]]] 123 => ["invalid type"]
      [:map [:x [:and [:map [:y :any]] seq?]]] {} => {:x ["missing required key"]}
      [:map [:x [:and [:map [:y :any]] seq?]]] {:x 123} => {:x ["invalid type" "should be a seq"]}
      [:map [:x [:and [:map [:y :any]] seq? (f "kosh")]]] {:x {}} => {:x {:y ["missing required key"]
                                                                          :malli/error ["should be a seq" "kosh"]}}
      [:map [:x [:and [:map [:y :any]] seq?]]] {:x {:y 123}} => {:x ["should be a seq"]}

      ;; records
      [:map [:x [:and [:map [:y :any]] seq?]]] (map->Horror {:x (map->Horror {})}) => {:x {:y ["missing required key"]
                                                                                           :malli/error ["should be a seq"]}}

      ;; don't derive error form from value in case of top-level error
      [:map [:x [:and seq? [:map [:y :any]]]]] 123 => ["invalid type"]
      [:map [:x [:and seq? [:map [:y :any]]]]] {} => {:x ["missing required key"]}
      [:map [:x [:and seq? [:map [:y :any]]]]] {:x 123} => {:x ["should be a seq" "invalid type"]}
      [:map [:x [:and seq? [:map [:y :any]]]]] {:x {}} => {:x ["should be a seq"]}

      ;; tuple sizes
      [:map [:x [:tuple :int :int :int]]] {} => {:x ["missing required key"]}
      [:map [:x [:tuple :int :int :int]]] {:x []} => {:x ["invalid tuple size 0, expected 3"]}
      [:map [:x [:tuple :int :int :int]]] {:x [1, 2]} => {:x ["invalid tuple size 2, expected 3"]}
      [:map [:x [:tuple :int :int :int]]] {:x [1 "2" 3]} => {:x [nil ["should be an integer"]]}
      [:map [:x [:tuple :int :int :int]]] {:x [1 "2" "3"]} => {:x [nil ["should be an integer"] ["should be an integer"]]}
      [:map [:x [:tuple :int [:and :int (f "fails")] :int]]] {:x [1 "2" "3"]} => {:x [nil ["should be an integer" "fails"] ["should be an integer"]]}
      [:map [:x [:tuple :int :int :int]]] {:x [1 2 3]} => nil

      ;; sequences
      [:and [:sequential :int] (f "1") (f "2")] [1 "2"] => [nil ["should be an integer"]]
      [:and [:sequential :int] (f "1") (f "2")] [1 2] => ["1" "2"])))

(deftest multi-humanize-test-428
  (is (= {:user {:type ["invalid dispatch value"]}}
         (-> (m/explain [:map [:user [:multi {:dispatch :type}]]] {:user nil})
             (me/humanize)))))
