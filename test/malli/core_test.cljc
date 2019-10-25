(ns malli.core-test
  (:require [clojure.test :refer [deftest testing is are]]
            [malli.core :as m]
            [malli.edn :as me]
            [malli.transform :as transform]))

(defn with-schema-forms [result]
  (some-> result
          (update :schema m/form)
          (update :errors (partial map (fn [error]
                                         (-> error
                                             (update :schema m/form)
                                             (update :type (fnil identity nil))
                                             (update :message (fnil identity nil))
                                             (m/map->SchemaError)))))))

(defn results= [& results]
  (apply = (map with-schema-forms results)))

(defn over-the-wire [?schema]
  (-> ?schema (me/write-string) (me/read-string)))

(deftest keyword->string
  (is (= "abba" (m/keyword->string :abba)))
  (is (= "jabba/abba" (m/keyword->string :jabba/abba)))
  (is (= "abba" (m/keyword->string "abba"))))

(deftest expand-key-test
  (are [schema expected]
    (= expected (second (#'m/-expand-key schema nil identity)))

    [:x int?] nil
    [:x {:optional true} int?] {:optional true}
    [:x {:optional false} int?] {:optional false}))

(defn visitor [schema childs _]
  (into [(m/name schema)] (seq childs)))

(deftest eval-test
  (is (= 2 ((m/eval inc) 1)))
  (is (= 2 ((m/eval 'inc) 1)))
  (is (= 2 ((m/eval '#(inc %)) 1)))
  (is (= 2 ((m/eval '#(inc %1)) 1)))
  (is (= 2 ((m/eval '(fn [x] (inc x))) 1)))
  (is (= 2 ((m/eval "(fn [x] (inc x))") 1))))

(deftest validation-test

  (testing "coercion"
    (is (= true
           (m/validate int? 1)
           (m/validate (m/schema int?) 1)
           (m/validate (get m/default-registry int?) 1)
           ((m/validator int?) 1)
           ((m/validator (m/schema int?)) 1))))

  (testing "function schemas"
    (let [schema (m/schema int?)]

      (is (true? (m/validate schema 1)))
      (is (false? (m/validate schema "1")))
      (is (false? (m/validate schema [1])))

      (is (nil? (m/explain schema 1)))
      (is (results= {:schema schema
                     :value "1"
                     :errors [{:path [], :in [], :schema schema, :value "1"}]}
                    {:schema schema
                     :value "1"
                     :errors [(m/error [] [] schema "1")]}
                    (m/explain schema "1")))

      (is (= 1 (m/transform schema "1" transform/string-transformer)))
      (is (= "1" (m/transform schema "1" transform/json-transformer)))

      (is (true? (m/validate (over-the-wire schema) 1)))

      (is (= ['int?] (m/accept schema visitor)))

      (is (= 'int? (m/form schema)))))

  (testing "composite schemas"
    (let [schema (m/schema [:and int? [:or pos-int? neg-int?]])]

      (is (true? (m/validate schema 1)))
      (is (true? (m/validate schema -1)))
      (is (false? (m/validate schema 0)))
      (is (false? (m/validate schema "1")))
      (is (false? (m/validate schema [1])))

      (is (nil? (m/explain schema 1)))
      (is (results= {:schema schema,
                     :value 0,
                     :errors [{:path [2 1], :in [], :schema pos-int?, :value 0}
                              {:path [2 2], :in [], :schema neg-int?, :value 0}]}
                    (m/explain schema 0)))

      (is (= 1 (m/transform schema "1" transform/string-transformer)))
      (is (= "1" (m/transform schema "1" transform/json-transformer)))

      (is (true? (m/validate (over-the-wire schema) 1)))

      (is (= [:and ['int?] [:or ['pos-int?] ['neg-int?]]] (m/accept schema visitor)))

      (is (= [:and 'int? [:or 'pos-int? 'neg-int?]] (m/form schema))))

    (testing "explain with branches"
      (let [schema [:and pos-int? neg-int?]]
        (is (results= {:schema schema,
                       :value -1,
                       :errors [{:path [1], :in [], :schema pos-int?, :value -1}]}
                      (m/explain schema -1))))
      (let [schema [:and pos-int? neg-int?]]
        (is (results= {:schema schema,
                       :value 1,
                       :errors [{:path [2], :in [], :schema neg-int?, :value 1}]}
                      (m/explain schema 1))))))

  (testing "comparator schemas"
    (let [schema (m/schema [:> 0])]

      (is (true? (m/validate schema 1)))
      (is (false? (m/validate schema 0)))
      (is (false? (m/validate schema "abba")))

      (is (nil? (m/explain schema 1)))
      (is (results= {:schema [:> 0], :value 0, :errors [{:path [], :in [], :schema [:> 0], :value 0}]}
                    (m/explain schema 0)))

      (is (= 1 (m/transform schema "1" transform/string-transformer)))
      (is (= "1" (m/transform schema "1" transform/json-transformer)))

      (is (true? (m/validate (over-the-wire schema) 1)))

      (is (= [:> 0] (m/accept schema visitor)))

      (is (= [:> 0] (m/form schema)))))

  (testing "enum schemas"
    (let [schema (m/schema [:enum 1 2])]

      (is (true? (m/validate schema 1)))
      (is (false? (m/validate schema 0)))
      (is (false? (m/validate schema "abba")))

      (is (nil? (m/explain schema 1)))
      (is (results= {:schema [:enum 1 2], :value 0, :errors [{:path [], :in [], :schema [:enum 1 2], :value 0}]}
                    (m/explain schema 0)))

      ;; TODO: infer type from :enum
      #_(is (= 1 (m/transform schema "1" transform/string-transformer)))
      #_(is (= "1" (m/transform schema "1" transform/json-transformer)))

      (is (true? (m/validate (over-the-wire schema) 1)))

      (is (= [:enum 1 2] (m/accept schema visitor)))

      (is (= [:enum 1 2] (m/form schema)))))

  (testing "maybe schemas"
    (let [schema (m/schema [:maybe int?])]

      (is (true? (m/validate schema 1)))
      (is (true? (m/validate schema nil)))
      (is (false? (m/validate schema "abba")))

      (is (nil? (m/explain schema 1)))
      (is (results= {:schema [:maybe int?], :value "abba", :errors [{:path [], :in [], :schema [:maybe int?], :value "abba"}]}
                    (m/explain schema "abba")))

      (is (= 1 (m/transform schema "1" transform/string-transformer)))
      (is (= "1" (m/transform schema "1" transform/json-transformer)))

      (is (true? (m/validate (over-the-wire schema) 1)))

      (is (= [:maybe ['int?]] (m/accept schema visitor)))

      (is (= [:maybe 'int?] (m/form schema)))))

  (testing "re schemas"
    (doseq [form [[:re "^[a-z]+\\.[a-z]+$"]
                  [:re #"^[a-z]+\.[a-z]+$"]
                  #"^[a-z]+\.[a-z]+$"]]
      (let [schema (m/schema form)]

        (is (true? (m/validate schema "a.b")))
        (is (false? (m/validate schema "abba")))
        (is (false? (m/validate schema ".b")))
        (is (false? (m/validate schema false)))

        (is (nil? (m/explain schema "a.b")))
        (is (results= {:schema schema, :value "abba", :errors [{:path [], :in [], :schema schema, :value "abba"}]}
                      (m/explain schema "abba")))

        (is (true? (m/validate (over-the-wire schema) "a.b")))

        (is (= [:re] (m/accept schema visitor)))

        (is (= form (m/form schema))))))

  (testing "fn schemas"
    (doseq [fn ['(fn [x] (and (int? x) (< 10 x 18)))
                "(fn [x] (and (int? x) (< 10 x 18)))"]]
      (let [schema (m/schema [:fn {:description "number between 10 and 18"} fn])]

        (is (true? (m/validate schema 12)))
        (is (false? (m/validate schema 1)))
        (is (false? (m/validate schema 20)))
        (is (false? (m/validate schema "invalid")))

        (is (nil? (m/explain schema 12)))
        (is (results= {:schema schema, :value "abba", :errors [{:path [], :in [], :schema schema, :value "abba"}]}
                      (m/explain schema "abba")))

        (is (true? (m/validate (over-the-wire schema) 12)))

        (is (= [:fn] (m/accept schema visitor)))

        (is (= [:fn {:description "number between 10 and 18"} fn]
               (m/form schema)))))

    (testing "non-terminating functions fail fast"
      (let [schema [:fn '(fn [x] (< x (apply max (range))))]]
        (is (false? (m/validate schema 1)))
        (is (results= {:schema schema
                       :value 1
                       :errors [{:path []
                                 :in []
                                 :schema schema
                                 :value 1
                                 :type :sci.error/realized-beyond-max}]}
                      (m/explain schema 1))))))

  (testing "map schemas"
    (let [schema (m/schema
                   [:map
                    [:x boolean?]
                    [:y {:optional true} int?]
                    [:z {:optional false} string?]])
          valid {:x true, :y 1, :z "kikka"}
          valid2 {:x false, :y 1, :z "kikka"}
          invalid {:x true, :y "invalid", :z "kikka", :extra "ok"}]

      (is (true? (m/validate schema valid)))
      (is (true? (m/validate schema valid2)))
      (is (false? (m/validate schema invalid)))
      (is (false? (m/validate schema "not-a-map")))

      (is (nil? (m/explain schema valid)))
      (is (nil? (m/explain schema valid2)))

      (is (results= {:schema schema
                     :value {:y "invalid" :z "kikka"}
                     :errors
                     [{:path [1 0], :in [:x], :schema schema, :type ::m/missing-key}
                      {:path [2 2], :in [:y], :schema int?, :value "invalid"}]}
                    (m/explain schema {:y "invalid" :z "kikka"})))

      (is (results= {:schema schema,
                     :value {:x "invalid"},
                     :errors [{:path [1 1], :in [:x], :schema boolean?, :value "invalid"}
                              {:path [3 0],
                               :in [:z],
                               :schema schema,
                               :type :malli.core/missing-key}]}
                    (m/explain schema {:x "invalid"})))

      (is (results= {:schema schema
                     :value "not-a-map"
                     :errors [{:path [], :in [], :schema schema, :value "not-a-map", :type ::m/invalid-type}]}
                    (m/explain schema "not-a-map")))

      (is (= {:x true} (m/transform schema {:x "true"} transform/string-transformer)))
      (is (= {:x true, :y 1} (m/transform schema {:x "true", :y "1"} transform/string-transformer)))
      (is (= {:x "true", :y "1"} (m/transform schema {:x "true", :y "1"} transform/json-transformer)))
      (is (= {:x true, :y 1} (m/transform schema {:x true, :y 1, :a 1} transform/strip-extra-keys-transformer)))
      (is (= {:x_key true, :y_key 2} (m/transform schema {:x true, :y 2}
                                                  (transform/key-transformer
                                                    (fn [key]
                                                      (-> key name (str "_key") keyword))))))

      (is (true? (m/validate (over-the-wire schema) valid)))

      (is (= [:map ['boolean?] ['int?] ['string?]] (m/accept schema visitor)))

      (is (= [:map
              [:x 'boolean?]
              [:y {:optional true} 'int?]
              [:z {:optional false} 'string?]]
             (m/form schema))))

    (is (true? (m/validate [:map [:b boolean?]] {:b true})))
    (is (true? (m/validate [:map [:b boolean?]] {:b false})))
    (is (true? (m/validate [:map [:n nil?]] {:n nil}))))

  (testing "accumulating errors #84"
    (let [re #"b"
          schema [:map
                  [:a string?]
                  [:b [:fn 'pos?]]
                  [:c re]
                  [:d [:maybe int?]]]
          value {:b -1, :c "", :d "not"}]
      (results= {:schema schema,
                 :value value,
                 :errors [{:path [1 0], :in [:a], :schema schema, :type :malli.core/missing-key}
                          {:path [2 1], :in [:b], :schema [:fn 'pos?], :value -1}
                          {:path [3 1], :in [:c], :schema re, :value ""}
                          {:path [4 1], :in [:d], :schema [:maybe int?], :value "not"}]}
                (m/explain schema value))))

  (testing "map-of schema"

    (is (true? (m/validate [:map-of string? int?] {"age" 18})))
    (is (true? (m/validate [:map-of keyword? int?] {:age 18})))
    (is (false? (m/validate [:map-of string? int?] {:age "18"})))
    (is (false? (m/validate [:map-of string? int?] 1)))

    (is (nil? (m/explain [:map-of string? int?] {"age" 18})))
    (is (some? (m/explain [:map-of string? int?] ::invalid)))
    (is (results= {:schema [:map-of string? int?],
                   :value {:age 18},
                   :errors [{:path [1],
                             :in [:age],
                             :schema string?,
                             :value :age}]}
                  (m/explain [:map-of string? int?] {:age 18})))
    (is (results= {:schema [:map-of string? int?],
                   :value {:age "18"},
                   :errors [{:path [1],
                             :in [:age],
                             :schema string?,
                             :value :age}
                            {:path [2],
                             :in [:age],
                             :schema int?,
                             :value "18"}]}
                  (m/explain [:map-of string? int?] {:age "18"})))

    (is (= {1 1} (m/transform [:map-of int? pos-int?] {"1" "1"} transform/string-transformer)))

    (is (true? (m/validate (over-the-wire [:map-of string? int?]) {"age" 18})))

    (is (= [:map-of ['int?] ['pos-int?]] (m/accept [:map-of int? pos-int?] visitor)))

    (testing "keyword keys are transformed via strings"
      (is (= {1 1} (m/transform [:map-of int? pos-int?] {:1 "1"} transform/string-transformer)))))

  (testing "sequence schemas"

    (testing "validation"
      (let [expectations {"vector" [[true [:vector int?] [1 2 3]]
                                    [false [:vector int?] [1 "2" 3]]
                                    [false [:vector int?] [1 2 "3"]]
                                    [false [:vector int?] [nil]]
                                    [false [:vector int?] "invalid"]

                                    [true [:vector {:min 3} int?] [1 2 3]]
                                    [false [:vector {:min 4} int?] [1 2 3]]

                                    [true [:vector {:max 3} int?] [1 2 3]]
                                    [false [:vector {:max 2} int?] [1 2 3]]

                                    [true [:vector {:min 1, :max 3} int?] [1 2 3]]
                                    [false [:vector {:min 4, :max 4} int?] [1 2 3]]

                                    [false [:vector int?] '(1 2 3)]
                                    [false [:vector int?] #{1 2 3}]]

                          "list" [[true [:list int?] '(1 2 3)]
                                  [false [:list int?] '(1 "2" 3)]
                                  [false [:list int?] '(1 2 "3")]
                                  [false [:vector int?] '(nil)]
                                  [false [:list int?] "invalid"]

                                  [true [:list {:min 3} int?] '(1 2 3)]
                                  [false [:list {:min 4} int?] '(1 2 3)]

                                  [true [:list {:max 3} int?] '(1 2 3)]
                                  [false [:list {:max 2} int?] '(1 2 3)]

                                  [true [:list {:min 1, :max 3} int?] '(1 2 3)]
                                  [false [:list {:min 4, :max 4} int?] '(1 2 3)]

                                  [false [:list int?] [1 2 3]]
                                  [false [:list int?] #{1 2 3}]]

                          "set" [[true [:set int?] #{1 2 3}]
                                 [false [:set int?] #{1 "2" 3}]
                                 [false [:set int?] #{1 2 "3"}]
                                 [false [:set int?] #{nil}]
                                 [false [:set int?] "invalid"]

                                 [true [:set {:min 3} int?] #{1 2 3}]
                                 [false [:set {:min 4} int?] #{1 2 3}]

                                 [true [:set {:max 3} int?] #{1 2 3}]
                                 [false [:set {:max 2} int?] #{1 2 3}]

                                 [true [:set {:min 1, :max 3} int?] #{1 2 3}]
                                 [false [:set {:min 4, :max 4} int?] #{1 2 3}]

                                 [false [:set int?] '(1 2 3)]
                                 [false [:set int?] [1 2 3]]]

                          "tuple" [[true [:tuple int?] [1]]
                                   [true [:tuple int? string?] [1 "2"]]
                                   [false [:tuple int?] ["1"]]
                                   [false [:tuple int?] [nil]]
                                   [false [:tuple int?] "invalid"]

                                   ;; ignored
                                   [true [:tuple {:min 3} int?] [1]]
                                   [true [:tuple {:min 4} int?] [1]]

                                   ;; ignored
                                   [true [:tuple {:max 3} int?] [1]]
                                   [true [:tuple {:max 2} int?] [1]]

                                   ;; ignored
                                   [true [:tuple {:min 1, :max 3} int?] [1]]
                                   [true [:tuple {:min 4, :max 4} int?] [1]]

                                   [false [:tuple int?] '(1)]
                                   [false [:tuple int?] #{1}]]}]

        (doseq [[name data] expectations
                [expected schema value] data]
          (testing name
            (is (= expected (m/validate schema value)))
            (is (= expected (m/validate (over-the-wire schema) value)))))))

    (testing "explain"
      (let [expectations {"vector" (let [schema [:vector {:min 2, :max 3} int?]]

                                     [[schema [1 2]
                                       nil]

                                      [schema 1
                                       {:schema schema
                                        :value 1
                                        :errors [{:path [], :in [], :type ::m/invalid-type, :schema schema, :value 1}]}]

                                      [schema [1]
                                       {:schema schema
                                        :value [1]
                                        :errors [{:path [], :in [], :type ::m/limits, :schema schema, :value [1]}]}]

                                      [schema [1 2 3 4]
                                       {:schema schema
                                        :value [1 2 3 4]
                                        :errors [{:path [], :in [], :type ::m/limits, :schema schema, :value [1 2 3 4]}]}]

                                      [schema [1 2 "3"]
                                       {:schema schema
                                        :value [1 2 "3"]
                                        :errors [{:path [2], :in [2], :schema int?, :value "3"}]}]])

                          "list" (let [schema [:list {:min 2, :max 3} int?]]

                                   [[schema '(1 2)
                                     nil]

                                    [schema 1
                                     {:schema schema
                                      :value 1
                                      :errors [{:path [], :in [], :type ::m/invalid-type, :schema schema, :value 1}]}]

                                    [schema '(1)
                                     {:schema schema
                                      :value '(1)
                                      :errors [{:path [], :in [], :type ::m/limits, :schema schema, :value '(1)}]}]

                                    [schema '(1 2 3 4)
                                     {:schema schema
                                      :value '(1 2 3 4)
                                      :errors [{:path [], :in [], :type ::m/limits, :schema schema, :value '(1 2 3 4)}]}]

                                    [schema '(1 2 "3")
                                     {:schema schema
                                      :value '(1 2 "3")
                                      :errors [{:path [2], :in [2], :schema int?, :value "3"}]}]])

                          "set" (let [schema [:set {:min 2, :max 3} int?]]

                                  [[schema #{1 2}
                                    nil]

                                   [schema 1
                                    {:schema schema
                                     :value 1
                                     :errors [{:path [], :in [], :type ::m/invalid-type, :schema schema, :value 1}]}]

                                   [schema #{1}
                                    {:schema schema
                                     :value #{1}
                                     :errors [{:path [], :in [], :type ::m/limits, :schema schema, :value #{1}}]}]

                                   [schema #{1 2 3 4}
                                    {:schema schema
                                     :value #{1 2 3 4}
                                     :errors [{:path [], :in [], :type ::m/limits, :schema schema, :value #{1 2 3 4}}]}]

                                   [schema #{1 2 "3"}
                                    {:schema schema
                                     :value #{1 2 "3"}
                                     :errors [{:path [2], :in [0], :schema int?, :value "3"}]}]])

                          "tuple" (let [schema [:tuple int? string?]]

                                    [[schema [1 "2"]
                                      nil]

                                     [schema 1
                                      {:schema schema
                                       :value 1
                                       :errors [{:path [], :in [], :type ::m/invalid-type, :schema schema, :value 1}]}]

                                     [schema [1]
                                      {:schema schema
                                       :value [1]
                                       :errors [{:path [], :in [], :type ::m/tuple-size, :schema schema, :value [1]}]}]

                                     [schema [1 2]
                                      {:schema schema
                                       :value [1 2]
                                       :errors [{:path [2], :in [1], :schema string?, :value 2}]}]])
                          "map+enum" (let [schema [:map [:x [:enum "x"]]
                                                   [:y [:enum "y"]]]]

                                       [[schema {:x "x" :y "y"}
                                         nil]

                                        [schema {:x "non-x" :y "y"}
                                         {:schema schema
                                          :value {:x "non-x" :y "y"}
                                          :errors [{:path [1 1], :in [:x], :schema [:enum "x"], :value "non-x"}]}]

                                        [schema {:x "x" :y "non-y"}
                                         {:schema schema
                                          :value {:x "x" :y "non-y"}
                                          :errors [{:path [2 1], :in [:y], :schema [:enum "y"], :value "non-y"}]}]

                                        [schema {:x "non-x" :y "non-y"}
                                         {:schema schema
                                          :value {:x "non-x" :y "non-y"}
                                          :errors [{:path [1 1], :in [:x], :schema [:enum "x"], :value "non-x"}
                                                   {:path [2 1], :in [:y], :schema [:enum "y"], :value "non-y"}]}]])}]

        (doseq [[name data] expectations
                [schema value expected] data]
          (testing name
            (is (results= expected (m/explain schema value)))))))

    (testing "visit"
      (doseq [name [:vector :list :set]]
        (is (= [name ['int?]] (m/accept [name int?] visitor))))
      (is (= [:tuple ['int?] ['int?]] (m/accept [:tuple int? int?] visitor))))))

(deftest path-with-properties-test
  (let [?path #(-> % :errors first :path)]

    (is (= [1] (?path (m/explain [:and int?] "2"))))
    (is (= [2] (?path (m/explain [:and {:name "int?"} int?] "2"))))

    (is (= [1] (?path (m/explain [:vector int?] ["2"]))))
    (is (= [2] (?path (m/explain [:vector {:name "int?"} [int?]] ["2"]))))

    (is (= [1] (?path (m/explain [:tuple int?] ["2"]))))
    (is (= [2] (?path (m/explain [:tuple {:name "int?"} [int?]] ["2"]))))

    (is (= [1 1] (?path (m/explain [:map [:x int?]] {:x "1"}))))
    (is (= [2 1] (?path (m/explain [:map {:name int?} [:x int?]] {:x "1"}))))
    (is (= [2 2] (?path (m/explain [:map {:name int?} [:x {:optional false} int?]] {:x "1"}))))))

(deftest properties-test
  (testing "properties can be set and retrieved"
    (let [properties {:title "kikka"}]
      (is (= properties
             (m/properties [:and properties int?])
             (m/properties [int? properties]))))))

(deftest childs-test
  (testing "childs can be set and retrieved"
    (is (= ['int? 'pos-int?]
           (m/childs [:and {:a 1} int? pos-int?])
           (m/childs [:and int? pos-int?])))))

(deftest round-trip-test
  (testing "schemas can be roundtripped"
    (let [schema (m/schema
                   [:map
                    [:x boolean?]
                    [:y {:optional true} int?]
                    [:z string?]])
          schema' (over-the-wire schema)
          valid {:x true, :y 1, :z "kikka"}]
      (is (= true
             (m/validate schema valid)
             (m/validate schema' valid)))
      (is (= (m/form schema) (m/form schema'))))))

(deftest custom-registry-test
  (let [registry (merge
                   m/comparator-registry
                   m/base-registry
                   {:int (m/fn-schema :int int?)
                    :string (m/fn-schema :string string?)})]
    (is (true? (m/validate [:or :int :string] 123 {:registry registry})))
    (is (false? (m/validate [:or :int :string] 'kikka {:registry registry})))))

(deftest merge-test
  (let [or-merge-strategy (fn [s1 s2] (m/schema [:or s1 s2]))]
    (are [?s1 ?s2 opts expected]
      (= (m/form expected) (m/form (m/merge ?s1 ?s2 opts)))

      int? int? {} int?
      int? pos-int? {} pos-int?
      int? nil {} int?
      nil pos-int? {} pos-int?

      ;; merge-strategy can be changed
      int? pos-int? {::m/merge or-merge-strategy} [:or int? pos-int?]

      [:map [:x int?]]
      [:map [:x {:optional true} pos-int?]]
      {}
      [:map [:x pos-int?]]

      [:map [:x int?]]
      [:map [:x {:optional true} pos-int?]]
      {}
      [:map [:x pos-int?]]

      ;; TODO: should retain the :optional key?
      [:map [:x {:optional false} int?]]
      [:map [:x {:optional true} pos-int?]]
      {}
      [:map [:x pos-int?]]

      ;; map forms are deep-merged
      [:map {:title "parameters"}
       [:parameters
        [:map
         [:query-params {:title "query1"}
          [:map [:x int?]]]]]]
      [:map {:description "description"}
       [:parameters
        [:map
         [:query-params {:title "query2"}
          [:map [:x string?] [:y int?]]]
         [:body-params
          [:map [:z int?]]]]]]
      {}
      [:map {:title "parameters", :description "description"}
       [:parameters
        [:map
         [:query-params {:title "query2"}
          [:map [:x string?] [:y int?]]]
         [:body-params
          [:map [:z int?]]]]]]

      ;; merge-stragy works with nested maps too
      [:map {:title "parameters"}
       [:parameters
        [:map
         [:query-params {:title "query1"}
          [:map [:x int?]]]]]]
      [:map {:description "description"}
       [:parameters
        [:map
         [:query-params {:title "query2"}
          [:map [:x string?] [:y int?]]]
         [:body-params
          [:map [:z int?]]]]]]
      {::m/merge or-merge-strategy}
      [:map {:title "parameters", :description "description"}
       [:parameters
        [:map
         [:query-params {:title "query2"}
          [:map [:x [:or int? string?]] [:y int?]]]
         [:body-params
          [:map [:z int?]]]]]])))

(deftest select-keys-test
  (are [s1 ks expected]
    (= (m/form expected) (m/form (m/select-keys s1 ks)))

    [:map [:x int?] [:y {:optional true} string?]]
    [:x]
    [:map [:x int?]]

    [:map [:x int?] [:y {:optional true} string?]]
    [:y]
    [:map [:y {:optional true} string?]]))

(deftest dissoc-test
  (are [s1 k expected]
    (= (m/form expected) (m/form (m/dissoc s1 k)))

    [:map [:x int?] [:y {:optional true} string?]]
    :x
    [:map [:y {:optional true} string?]]

    [:map [:x int?] [:y {:optional true} string?]]
    :y
    [:map [:x int?]]))

(deftest assoc-test
  (are [s1 k v expected]
    (= (m/form expected) (m/form (m/assoc s1 k v)))

    [:map [:x int?]]
    :y string?
    [:map [:x int?] [:y string?]]

    [:map [:x int?]]
    :x string?
    [:map [:x string?]]))

(deftest update-in-test
  (let [s [:map [:x int?] [:y [:map [:z string?] [:w int?]]]]]
    (is (= (m/form [:map [:x int?] [:y [:map [:z string?]]]])
           (m/form (m/update-in s [:y] m/dissoc :w))))

    (is (= (m/form [:map [:x int?] [:y [:map [:z string?] [:w string?]]]])
           (m/form (m/update-in s [:y :w] (fn [_] string?)))))))


