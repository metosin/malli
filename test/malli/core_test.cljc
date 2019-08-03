(ns malli.core-test
  (:require [clojure.test :refer [deftest testing is are]]
            [malli.core :as m]
            #?@(:clj  [[clojure.edn]]
                :cljs [[cljs.reader]])))

(deftest validation-test

  (testing "coercion"
    (is (= true
           (m/validate int? 1)
           (m/validate (m/schema int?) 1)
           ((m/validator int?) 1)
           ((m/validator (m/schema int?)) 1))))

  (testing "function schemas"
    (let [schema (m/schema int?)]
      (is (true? (m/validate schema 1)))
      (is (false? (m/validate schema "1")))
      (is (= 'int? (m/form schema)))))

  (testing "composite schemas"
    (let [schema (m/schema [:and int? [:or pos-int? neg-int?]])]
      (is (true? (m/validate schema 1)))
      (is (true? (m/validate schema -1)))
      (is (false? (m/validate schema 0)))
      (is (= [:and 'int? [:or 'pos-int? 'neg-int?]]
             (m/form schema)))))

  (testing "comparator schemas"
    (let [schema (m/schema [:and int? [:>= 10] [:< 100]])]
      (is (true? (m/validate schema 10)))
      (is (true? (m/validate schema 99)))
      (is (false? (m/validate schema 100)))
      (is (= [:and 'int? [:>= 10] [:< 100]]
             (m/form schema)))))

  (testing "map schemas"
    (let [schema1 (m/schema
                    [:map
                     [:x boolean?]
                     [[:opt :y] int?]
                     [:z string?]])
          schema2 (m/schema
                    [:map
                     [:x boolean?]
                     [:y {:optional true} int?]
                     [:z string?]])
          valid {:x true, :y 1, :z "kikka"}
          invalid {:x true, :y "invalid", :z "kikka"}]
      (doseq [schema [schema1 schema2]]
        (is (true? (m/validate schema valid)))
        (is (false? (m/validate schema invalid))))
      (is (= [:map
              [:x {:optional false} 'boolean?]
              [:y {:optional true} 'int?]
              [:z {:optional false} 'string?]]
             (m/form schema1)
             (m/form schema2)))))

  (testing "sequence schemas"

    (testing "vector"
      (are [expected schema value]
        (is (= expected (m/validate schema value)))

        true [:vector int?] [1 2 3]
        false [:vector int?] [1 "2" 3]

        true [:vector {:min 3} int?] [1 2 3]
        false [:vector {:min 4} int?] [1 2 3]

        true [:vector {:max 3} int?] [1 2 3]
        false [:vector {:max 2} int?] [1 2 3]

        true [:vector {:min 1, :max 3} int?] [1 2 3]
        false [:vector {:min 4, :max 4} int?] [1 2 3]

        false [:vector int?] '(1 2 3)
        false [:vector int?] #{1 2 3})

      (is (= [:vector 'int?] (m/form [:vector int?]))))

    (testing "list"
      (are [expected schema value]
        (is (= expected (m/validate schema value)))

        true [:list int?] '(1 2 3)
        false [:list int?] '(1 "2" 3)

        true [:list {:min 3} int?] '(1 2 3)
        false [:list {:min 4} int?] '(1 2 3)

        true [:list {:max 3} int?] '(1 2 3)
        false [:list {:max 2} int?] '(1 2 3)

        true [:list {:min 1, :max 3} int?] '(1 2 3)
        false [:list {:min 4, :max 4} int?] '(1 2 3)

        false [:list int?] [1 2 3]
        false [:list int?] #{1 2 3})

      (is (= [:list 'int?] (m/form [:list int?]))))

    (testing "set"
      (are [expected schema value]
        (is (= expected (m/validate schema value)))

        true [:set int?] #{1 2 3}
        false [:set int?] #{1 "2" 3}

        true [:set {:min 3} int?] #{1 2 3}
        false [:set {:min 4} int?] #{1 2 3}

        true [:set {:max 3} int?] #{1 2 3}
        false [:set {:max 2} int?] #{1 2 3}

        true [:set {:min 1, :max 3} int?] #{1 2 3}
        false [:set {:min 4, :max 4} int?] #{1 2 3}

        false [:set int?] '(1 2 3)
        false [:set int?] [1 2 3])

      (is (= [:set 'int?] (m/form [:set int?]))))

    (testing "tuple"
      (are [expected schema value]
        (is (= expected (m/validate schema value)))

        true [:tuple int?] [1]
        true [:tuple int? string?] [1 "2"]
        false [:tuple int?] ["1"]

        ;; ignored
        true [:tuple {:min 3} int?] [1]
        true [:tuple {:min 4} int?] [1]

        ;; ignored
        true [:tuple {:max 3} int?] [1]
        true [:tuple {:max 2} int?] [1]

        ;; ignored
        true [:tuple {:min 1, :max 3} int?] [1]
        true [:tuple {:min 4, :max 4} int?] [1]

        false [:tuple int?] '(1)
        false [:tuple int?] #{1})

      (is (= [:tuple 'int? 'string?] (m/form [:tuple int? string?]))))))

(deftest properties-test
  (testing "properties can be set and retrieved"
    (let [properties {:title "kikka"}]
      (is (= properties
             (m/properties [:and properties int?])
             (m/properties [int? properties]))))))

(deftest round-trip-test
  (testing "schemas can be roundtripped"
    (let [schema (m/schema
                   [:map
                    [:x boolean?]
                    [[:opt :y] int?]
                    [:z string?]])
          schema' (-> schema
                      (m/form)
                      (pr-str)
                      (#?(:clj  clojure.edn/read-string,
                          :cljs cljs.reader/read-string))
                      (m/schema))
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
