(ns malli.error-test
  (:require [clojure.test :refer [are deftest is testing]]
            [malli.core :as m]
            [malli.core-test]
            [malli.error :as me]
            [malli.generator :as mg]
            [malli.util :as mu]
            #?(:clj [malli.test-macros :refer [when-env]]))
  #?(:cljs (:require-macros [malli.test-macros :refer [when-env]]))
  #?(:cljs (:import (goog Uri))))

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
                 (get-errors))))
      (is (= [{:path ["deliverz"]
               :type ::me/misspelled-key
               ::me/likely-misspelling-of [["deliver"]]
               :message #?(:clj "should be spelled \"deliver\""
                           :cljs "should be spelled deliver")}]
             (-> [:map
                  ["orders" boolean?]
                  ["deliver" boolean?]]
                 (mu/closed-schema)
                 (m/explain {"orders" true, "deliverz" true})
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
    (is (= #{#{["should be a keyword"]}}
           (-> [:set [:set keyword?]]
               (m/explain #{#{42 :a {}}})
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

(when-env
 "TEST_SCI"
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
         (is (-> schema (m/explain ::invalid) (me/humanize {::m/disable-sci false}))))))))

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
          :b ["should be at least 1 character"],
          :c ["should be at most 4 characters"],
          :d [["should be at least 1 character"]
              ["should be at most 4 characters"]],
          :e ["should be a string"]
          :f ["should be 4 characters"]
          :g ["should be at most 1 character"]
          :h ["should be 1 character"]}
         (-> [:map
              [:a :string]
              [:b [:string {:min 1}]]
              [:c [:string {:max 4}]]
              [:d [:vector [:string {:min 1, :max 4}]]]
              [:e [:string {:min 1, :max 4}]]
              [:f [:string {:min 4, :max 4}]]
              [:g [:string {:max 1}]]
              [:h [:string {:min 1 :max 1}]]]
             (m/explain
              {:a 123
               :b ""
               :c "invalid"
               :d ["" "12345"]
               :e 123
               :f "invalid"
               :g "ab"
               :h ""})
             (me/humanize)))))

(deftest int-test
  (is (= {:a ["should be an integer"]
          :b ["should be at least 1"]
          :c ["should be at most 4"]
          :d [["should be at least 1"]
              ["should be at most 4"]]
          :e ["should be an integer"]
          :f ["should be 4"]}
         (-> [:map
              [:a :int]
              [:b [:int {:min 1}]]
              [:c [:int {:max 4}]]
              [:d [:vector [:int {:min 1, :max 4}]]]
              [:e [:int {:min 1, :max 4}]]
              [:f [:int {:min 4, :max 4}]]]
             (m/explain
              {:a "123"
               :b 0
               :c 5
               :d [0 5]
               :e "123"
               :f 5})
             (me/humanize)))))

(deftest double-test
  (doseq [t [:double :float]]
    (is (= {:a [(str "should be a " (name t))]
            :b ["should be at least 1"]
            :c ["should be at most 4"]
            :d [["should be at least 1"]
                ["should be at most 4"]]
            :e [(str "should be a " (name t))]
            :f ["should be 4"]}
           (-> [:map
                [:a t]
                [:b [t {:min 1}]]
                [:c [t {:max 4}]]
                [:d [:vector [t {:min 1, :max 4}]]]
                [:e [t {:min 1, :max 4}]]
                [:f [t {:min 4, :max 4}]]]
               (m/explain
                 {:a "123"
                  :b 0.0
                  :c 5.0
                  :d [0.0 5.0]
                  :e "123"
                  :f 5.0})
               (me/humanize))))))

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
    (is (= [#?(:clj "should be \"foo\""
               :cljs "should be foo")]
           (-> [:enum "foo"]
               (m/explain "baz")
               (me/humanize)))))
  (testing "error with 2 values"
    (is (= [#?(:clj "should be either \"foo\" or \"bar\""
               :cljs "should be either foo or bar")]
           (-> [:enum "foo" "bar"]
               (m/explain "baz")
               (me/humanize)))))
  (testing "more than 2 values"
    (is (= [#?(:clj "should be either \"foo\", \"bar\", bar or \"buzz\""
               :cljs "should be either foo, bar, bar or buzz")]
           (-> [:enum "foo" "bar" 'bar "buzz"]
               (m/explain "baz")
               (me/humanize))))
    (is (= [#?(:clj "should be either \"foo\", \"bar\", \"buzz\" or \"biff\""
               :cljs "should be either foo, bar, buzz or biff")]
           (-> [:enum "foo" "bar" "buzz" "biff"]
               (m/explain "baz")
               (me/humanize))))))

(deftest function-test
  (is (= ["should be a valid function"]
         (-> [:=> [:cat int? int?] int?]
             (m/explain malli.core-test/single-arity {::m/function-checker mg/function-checker})
             (me/humanize))))
  (is (= ["should be a valid function"]
         (-> [:=> [:cat int? int?] int?]
             (m/explain 123)
             (me/humanize)))))

(deftest ifn-test
  (is (= ["should be an ifn"]
         (me/humanize (m/explain ifn? 123)))))

(defrecord Horror [])

(deftest multi-error-test
  (let [schema [:multi {:dispatch :type}
                ["plus" [:map [:value int?]]]
                ["minus" [:map [:value int?]]]
                ['minus [:map [:value int?]]]]]

    (is (= {:type ["invalid dispatch value"]}
           (-> schema
               (m/explain {:type "minuz"})
               (me/humanize))))

    (is (= {:type [#?(:clj "did you mean \"minus\" or minus"
                      :cljs "did you mean minus or minus")]}
           (-> schema
               (m/explain {:type "minuz"})
               (me/with-spell-checking)
               (me/humanize)))))

  (testing "explain works even when dispatch is a keyword but value is not a map"
    (is (= ["invalid dispatch value"]
           (-> (m/schema [:multi {:dispatch :x}
                          [:y [:map [:x :keyword]]]])
               (m/explain [])
               (me/humanize))))

    (is (= {:x ["invalid dispatch value"]}
           (-> (m/schema [:multi {:dispatch :x}
                          [:y [:map [:x :keyword]]]])
               (m/explain (map->Horror {:foo :bar}))
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

(def VarSchema [:map [:foo :int]])

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
    (is (= {:a [#?(:clj "should be either \"a\", \"b\", a or b"
                   :cljs "should be either a, b, a or b")]}
           (-> [:map
                [:a [:enum "a" "b" 'a 'b]]]
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
        (= [expected] (map #(me/-resolve-root-error error % nil) errors)))

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
       {:error/message "map-failure", ::level :warn}]))

  (testing ":fn with :error/path #554"
    (is (= {:password2 ["passwords don't match"]}
           (-> [:and [:map
                      [:password string?]
                      [:password2 string?]]
                [:fn {:error/message "passwords don't match"
                      :error/path [:password2]}
                 '(fn [{:keys [password password2]}]
                    (= password password2))]]
               (m/explain {:password "secret"
                           :password2 "faarao"})
               (me/humanize {:resolve me/-resolve-root-error})))))

  (testing "refs #1106"
    (is (= {:foo ["should be an integer"]}
           (me/humanize
            (m/explain [:ref #'VarSchema] {:foo "2"})
            {:resolve me/-resolve-direct-error})))
    (is (= {:foo ["should be an integer"]}
           (me/humanize
            (m/explain [:ref #'VarSchema] {:foo "2"})
            {:resolve me/-resolve-root-error})))))

(deftest limits
  (is (= {:a [["should be an int"]]
          :b ["should have at least 2 elements"]
          :c ["should have at most 5 elements"]
          :d [["should have at least 2 elements"]
              ["should have at most 5 elements"]]
          :e ["should have at least 2 elements"]
          :f ["should have 5 elements"]}
         (-> [:map
              [:a [:vector int?]]
              [:b [:vector {:min 2} int?]]
              [:c [:vector {:max 5} int?]]
              [:d [:vector [:vector {:min 2, :max 5} int?]]]
              [:e [:vector {:min 2, :max 5} int?]]
              [:f [:vector {:min 5, :max 5} int?]]]
             (m/explain
              {:a ["123"]
               :b [1]
               :c [1 2 3 4 5 6]
               :d [[1] [1 2 3 4 5 6 7]]
               :e [1.2]
               :f [1 2 3 4]})
             (me/humanize)))))

(deftest robust-humanize-form
  (let [f (fn [s] [:fn {:error/message s} (constantly false)])
        => ::irrelevant]
    (are [schema value _ expected]
      (= expected (-> (m/explain schema value) (me/humanize)))

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
  (is (= {:user ["invalid dispatch value"]}
         (-> (m/explain [:map [:user [:multi {:dispatch :type}]]] {:user nil})
             (me/humanize)))))

(deftest in-error-test
  (let [Address [:map {:closed true}
                 [:id :string]
                 [:tags [:set :keyword]]
                 [:numbers [:sequential :int]]
                 [:address [:map
                            [:street :string]
                            [:city :string]
                            [:zip :int]
                            [:lonlat [:tuple :double :double]]]]]
        address {:id "Lillan"
                 :EXTRA "KEY"
                 :tags #{:artesan "coffee" :garden "ground"}
                 :numbers (list 1 "2" 3 4 "5" 6 7)
                 :address {:street "Ahlmanintie 29"
                           :zip 33100
                           :lonlat [61.4858322, "23.7832851,17"]}}]

    (testing "with defaults"
      (is (= {:EXTRA "KEY"
              :tags #{"coffee" "ground"}
              :numbers [nil "2" nil nil "5"]
              :address {:lonlat [nil "23.7832851,17"]}}
             (-> (m/explain Address address)
                 (me/error-value)))))

    (testing "custom accept"
      (is (= {:EXTRA "KEY"
              :tags #{"coffee" "ground"}
              :numbers [nil "2" nil nil "5"]
              :address {:city nil
                        :lonlat [nil "23.7832851,17"]}}
             (-> (m/explain Address address)
                 (me/error-value {::me/accept-error (constantly true)})))))

    (testing "masked valid values"
      (let [explain (m/explain Address address)]
        (is (= {:id '...
                :EXTRA "KEY"
                :tags #{"coffee" "ground" '...}
                :numbers ['... "2" '... '... "5" '... '...]
                :address {:street '...
                          :zip '...
                          :lonlat ['... "23.7832851,17"]}}
               (me/error-value explain {::me/mask-valid-values '...})))

        (is (= [{:EXTRA '..., :address '..., :id '..., :numbers '..., :tags #{"coffee" '...}}
                {:EXTRA '..., :address '..., :id '..., :numbers '..., :tags #{"ground" '...}}
                {:EXTRA '..., :address '..., :id '... :numbers ['... "2" '... '... '... '... '...], :tags '...}
                {:EXTRA '..., :address '..., :id '..., :numbers ['... '... '... '... "5" '... '...], :tags '...}
                {:EXTRA '..., :address '..., :id '..., :numbers '..., :tags '...}
                {:EXTRA '..., :address {:lonlat ['... "23.7832851,17"], :street '..., :zip '...}, :id '..., :numbers '..., :tags '...}
                {:EXTRA "KEY", :address '..., :id '..., :numbers '..., :tags '...}]
               (for [error (:errors explain)]
                 (me/error-value (assoc explain :errors [error]) {::me/mask-valid-values '...}))))))

    (testing "masked nested maps #1096"
      (is (= {"foo" "foo"}
             (-> (m/explain [:map-of :keyword [:map-of :keyword :any]] {"foo" {:bar 1}})
                 (me/error-value {::me/mask-valid-values '...})))))

    (testing "custom painting of errors"
      (is (= {:EXTRA {:value "KEY", :type :malli.core/extra-key}
              :tags #{{:value "ground"} {:value "coffee"}}
              :numbers [nil {:value "2"} nil nil {:value "5"}]
              :address {:lonlat [nil {:value "23.7832851,17"}]}}
             (-> (m/explain Address address)
                 (me/error-value {::me/wrap-error #(select-keys % [:value :type])}))))

      (testing "keeping valid values"
        (is (= {:EXTRA {:type :malli.core/extra-key, :value "KEY"}
                :address {:lonlat [61.4858322 {:value "23.7832851,17"}]
                          :street "Ahlmanintie 29"
                          :zip 33100}
                :id "Lillan"
                :numbers [1 {:value "2"} 3 4 {:value "5"} 6 7]
                :tags #{:artesan :garden {:value "coffee"} {:value "ground"}}}
               (-> (m/explain Address address)
                   (me/error-value {::me/wrap-error #(select-keys % [:value :type])
                                    ::me/keep-valid-values true}))))))))

#?(:clj
   (deftest pr-str-humanize-test
     (is (= ["should be \"a\""] (me/humanize (m/explain [:enum "a"] 1))))
     (is (= ["should be a"] (me/humanize (m/explain [:enum 'a] 1))))
     (is (= ["should be either \"a\" or \"b\""] (me/humanize (m/explain [:enum "a" "b"] 1))))
     (is (= ["should be either a or b"] (me/humanize (m/explain [:enum 'a 'b] 1))))
     (is (= ["should be \"a\""] (me/humanize (m/explain [:= "a"] 1))))
     (is (= ["should be a"] (me/humanize (m/explain [:= 'a] 1))))
     (is (= ["should not be \"a\""] (me/humanize (m/explain [:not= "a"] "a"))))
     (is (= ["should not be a"] (me/humanize (m/explain [:not= 'a] 'a))))))

(deftest not-humanize-test
  (is (= ["should not be any"] (me/humanize (m/explain [:not any?] true))))
  (is (= ["should not be some"] (me/humanize (m/explain [:not some?] true))))
  (is (= ["should not be a number"] (me/humanize (m/explain [:not number?] 1))))
  (is (= ["should not be an integer"] (me/humanize (m/explain [:not integer?] 1))))
  (is (= ["should not be an int"] (me/humanize (m/explain [:not int?] 1))))
  (is (= ["should not be a positive int"] (me/humanize (m/explain [:not pos-int?] 1))))
  (is (= ["should not be a negative int"] (me/humanize (m/explain [:not neg-int?] -1))))
  (is (= ["should not be a non-negative int"] (me/humanize (m/explain [:not nat-int?] 1))))
  (is (= ["should not be positive"] (me/humanize (m/explain [:not pos?] 1))))
  (is (= ["should not be negative"] (me/humanize (m/explain [:not neg?] -1))))
  (is (= ["should not be a float"] (me/humanize (m/explain [:not float?] 1.23))))
  (is (= ["should not be a double"] (me/humanize (m/explain [:not double?] 1.23))))
  (is (= ["should not be a boolean"] (me/humanize (m/explain [:not boolean?] true))))
  (is (= ["should not be a string"] (me/humanize (m/explain [:not string?] ""))))
  (is (= ["should not be an ident"] (me/humanize (m/explain [:not ident?] 'a))))
  (is (= ["should not be a simple ident"] (me/humanize (m/explain [:not simple-ident?] 'a))))
  (is (= ["should not be a qualified ident"] (me/humanize (m/explain [:not qualified-ident?] ::a))))
  (is (= ["should not be a keyword"] (me/humanize (m/explain [:not keyword?] :a))))
  (is (= ["should not be a simple keyword"] (me/humanize (m/explain [:not simple-keyword?] :a))))
  (is (= ["should not be a qualified keyword"] (me/humanize (m/explain [:not qualified-keyword?] ::a))))
  (is (= ["should not be a symbol"] (me/humanize (m/explain [:not symbol?] 'a))))
  (is (= ["should not be a simple symbol"] (me/humanize (m/explain [:not simple-symbol?] 'a))))
  (is (= ["should not be a qualified symbol"] (me/humanize (m/explain [:not qualified-symbol?] `a))))
  (is (= ["should not be a uuid"] (me/humanize (m/explain [:not uuid?] (random-uuid)))))
  (is (= ["should not be a uri"] (me/humanize (m/explain [:not uri?] (#?(:clj java.net.URI.
                                                                         :cljs Uri.
                                                                         :default (throw (ex-info "Create URI" {})))
                                                                              "http://asdf.com")))))
  #?(:clj (is (= ["should not be a decimal"] (me/humanize (m/explain [:not decimal?] 1M)))))
  (is (= ["should not be an inst"] (me/humanize (m/explain [:not inst?] #inst "2018-04-27T18:25:37Z"))))
  (is (= ["should not be seqable"] (me/humanize (m/explain [:not seqable?] nil))))
  (is (= ["should not be indexed"] (me/humanize (m/explain [:not indexed?] []))))
  (is (= ["should not be a map"] (me/humanize (m/explain [:not map?] {}))))
  (is (= ["should not be a vector"] (me/humanize (m/explain [:not vector?] []))))
  (is (= ["should not be a list"] (me/humanize (m/explain [:not list?] (list)))))
  (is (= ["should not be a seq"] (me/humanize (m/explain [:not seq?] (list)))))
  (is (= ["should not be a char"] (me/humanize (m/explain [:not char?] \a))))
  (is (= ["should not be a set"] (me/humanize (m/explain [:not set?] #{}))))
  (is (= ["should not be nil"] (me/humanize (m/explain [:not nil?] nil))))
  (is (= ["should not be false"] (me/humanize (m/explain [:not false?] false))))
  (is (= ["should not be true"] (me/humanize (m/explain [:not true?] true))))
  (is (= ["should not be zero"] (me/humanize (m/explain [:not zero?] 0))))
  #?(:clj (is (= ["should not be a rational"] (me/humanize (m/explain [:not rational?] 1/2)))))
  (is (= ["should not be a coll"] (me/humanize (m/explain [:not coll?] []))))
  (is (= ["should not be empty"] (me/humanize (m/explain [:not empty?] []))))
  (is (= ["should not be associative"] (me/humanize (m/explain [:not associative?] []))))
  (is (= ["should not be sequential"] (me/humanize (m/explain [:not sequential?] []))))
  #?(:clj (is (= ["should not be a ratio"] (me/humanize (m/explain [:not ratio?] 1/2)))))
  #?(:clj (is (= ["should not be bytes"] (me/humanize (m/explain [:not bytes?] (byte-array 0))))))
  (is (= ["should not match regex"] (me/humanize (m/explain [:not [:re #""]] ""))))
  (is (= ["should not be a valid function"] (me/humanize (m/explain [:not [:=> :cat :any]] (fn [])))))
  (is (= ["should not be an ifn"] (me/humanize (m/explain [:not ifn?] (fn [])))))
  (is (= ["should not be a fn"] (me/humanize (m/explain [:not fn?] (fn [])))))
  (is (= ["should not be 1"] (me/humanize (m/explain [:not [:enum 1]] 1))))
  (is (= ["should not be either 1, 2 or 3"] (me/humanize (m/explain [:not [:enum 1 2 3]] 1))))
  (is (= ["should not be any"] (me/humanize (m/explain [:not :any] 1))))
  (is (= ["should not be nil"] (me/humanize (m/explain [:not :nil] nil))))
  (is (= ["should not be a string"] (me/humanize (m/explain [:not :string] "a"))))
  (is (= ["should not be at least 1 character"] (me/humanize (m/explain [:not [:string {:min 1}]] "a"))))
  (is (= ["should not be at most 1 character"] (me/humanize (m/explain [:not [:string {:max 1}]] "a"))))
  (is (= ["should not be 1 character"] (me/humanize (m/explain [:not [:string {:min 1 :max 1}]] "a"))))
  (is (= ["should not be an integer"] (me/humanize (m/explain [:not :int] 1))))
  (is (= ["should not be at least 1"] (me/humanize (m/explain [:not [:int {:min 1}]] 1))))
  (is (= ["should not be at most 1"] (me/humanize (m/explain [:not [:int {:max 1}]] 1))))
  (is (= ["should not be 1"] (me/humanize (m/explain [:not [:int {:min 1 :max 1}]] 1))))
  (is (= ["should not be a double"] (me/humanize (m/explain [:not :double] 1.5))))
  (is (= ["should not be at least 1.5"] (me/humanize (m/explain [:not [:double {:min 1.5}]] 1.5))))
  (is (= ["should not be at most 1.5"] (me/humanize (m/explain [:not [:double {:max 1.5}]] 1.5))))
  (is (= ["should not be 1.5"] (me/humanize (m/explain [:not [:double {:min 1.5 :max 1.5}]] 1.5))))
  (is (= ["should not be a boolean"] (me/humanize (m/explain [:not :boolean] true))))
  (is (= ["should not be a keyword"] (me/humanize (m/explain [:not :keyword] :a))))
  (is (= ["should not be a symbol"] (me/humanize (m/explain [:not :symbol] 'a))))
  (is (= ["should not be a qualified keyword"] (me/humanize (m/explain [:not :qualified-keyword] ::a))))
  (is (= ["should not be a qualified symbol"] (me/humanize (m/explain [:not :qualified-symbol] `a))))
  (is (= ["should not be a uuid"] (me/humanize (m/explain [:not :uuid] (random-uuid)))))
  (is (= ["should be at most 1"] (me/humanize (m/explain [:not [:> 1]] 2))))
  (is (= ["should be smaller than 1"] (me/humanize (m/explain [:not [:>= 1]] 2))))
  (is (= ["should be at least 1"] (me/humanize (m/explain [:not [:< 1]] 0))))
  (is (= ["should be larger than 1"] (me/humanize (m/explain [:not [:<= 1]] 0))))
  (is (= ["should not be 1"] (me/humanize (m/explain [:not [:= 1]] 1))))
  (is (= ["should be 1"] (me/humanize (m/explain [:not [:not= 1]] nil)))))

(deftest nested-not-humanize-test
  (testing ":="
    (is (= ["should be 1"]     (me/humanize (m/explain [:= 1] nil))))
    (is (= ["should not be 1"] (me/humanize (m/explain [:not [:= 1]] 1))))
    (is (= ["should be 1"]     (me/humanize (m/explain [:not [:not [:= 1]]] nil))))
    (is (= ["should not be 1"] (me/humanize (m/explain [:not [:not [:not [:= 1]]]] 1))))
    (is (= ["should be 1"]     (me/humanize (m/explain [:not [:not [:not [:not [:= 1]]]]] nil)))))
  (testing ":>"
    (is (= ["should be larger than 1"] (me/humanize (m/explain [:> 1] 0))))
    (is (= ["should be at most 1"]     (me/humanize (m/explain [:not [:> 1]] 2))))
    (is (= ["should be larger than 1"] (me/humanize (m/explain [:not [:not [:> 1]]] 0))))
    (is (= ["should be at most 1"]     (me/humanize (m/explain [:not [:not [:not [:> 1]]]] 2))))
    (is (= ["should be larger than 1"] (me/humanize (m/explain [:not [:not [:not [:not [:> 1]]]]] 0)))))
  (testing ":>="
    (is (= ["should be at least 1"]     (me/humanize (m/explain [:>= 1] 0))))
    (is (= ["should be smaller than 1"] (me/humanize (m/explain [:not [:>= 1]] 2))))
    (is (= ["should be at least 1"]     (me/humanize (m/explain [:not [:not [:>= 1]]] 0))))
    (is (= ["should be smaller than 1"] (me/humanize (m/explain [:not [:not [:not [:>= 1]]]] 2))))
    (is (= ["should be at least 1"]     (me/humanize (m/explain [:not [:not [:not [:not [:>= 1]]]]] 0)))))
  (testing ":<"
    (is (= ["should be smaller than 1"] (me/humanize (m/explain [:< 1] 2))))
    (is (= ["should be at least 1"]     (me/humanize (m/explain [:not [:< 1]] 0))))
    (is (= ["should be smaller than 1"] (me/humanize (m/explain [:not [:not [:< 1]]] 2))))
    (is (= ["should be at least 1"]     (me/humanize (m/explain [:not [:not [:not [:< 1]]]] 0))))
    (is (= ["should be smaller than 1"] (me/humanize (m/explain [:not [:not [:not [:not [:< 1]]]]] 2)))))
  (testing ":<="
    (is (= ["should be at most 1"]     (me/humanize (m/explain [:<= 1] 2))))
    (is (= ["should be larger than 1"] (me/humanize (m/explain [:not [:<= 1]] 0))))
    (is (= ["should be at most 1"]     (me/humanize (m/explain [:not [:not [:<= 1]]] 2))))
    (is (= ["should be larger than 1"] (me/humanize (m/explain [:not [:not [:not [:<= 1]]]] 0))))
    (is (= ["should be at most 1"]     (me/humanize (m/explain [:not [:not [:not [:not [:<= 1]]]]] 2))))))

(deftest custom-negating-test
  (is (= ["should be a multiple of 3"]
         (me/humanize (m/explain [:fn {:error/message {:en "should be a multiple of 3"}} #(= 0 (mod % 3))] 2))))
  (is (= ["should not be a multiple of 3"]
         (me/humanize (m/explain [:not [:fn {:error/message {:en "should be a multiple of 3"}} #(= 0 (mod % 3))]] 3))))
  (is (= ["should not be a multiple of 3 negated=false"]
         (me/humanize (m/explain [:fn {:error/fn {:en (fn [{:keys [negated]} _] (str "should not be a multiple of 3 negated="
                                                                                     (boolean negated)))}}
                                  #(not= 0 (mod % 3))] 0))))
  (is (= ["should be a multiple of 3 negating=true"]
         (me/humanize (m/explain [:not [:fn {:error/fn {:en (fn [{:keys [negated]} _] (str "should not be a multiple of 3 negating="
                                                                                            (boolean negated)))}}
                                        #(not= 0 (mod % 3))]] 1))))
  (testing ":negated disables implicit negation"
    (is (= ["should not avoid being a multiple of 3"]
           (me/humanize (m/explain [:not [:fn {:error/fn {:en (fn [{:keys [negated]} _]
                                                                (if negated
                                                                  (negated "should not avoid being a multiple of 3")
                                                                  "should not be a multiple of 3"))}}
                                          #(not= 0 (mod % 3))]] 1))))))
