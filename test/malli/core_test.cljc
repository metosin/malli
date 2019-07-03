(ns malli.core-test
  (:require [clojure.test :refer [deftest testing is]]
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
                     [:y {:required false} int?]
                     [:z string?]])
          valid {:x true, :y 1, :z "kikka"}
          invalid {:x true, :y "invalid", :z "kikka"}]
      (doseq [schema [schema1 schema2]]
        (is (true? (m/validate schema valid)))
        (is (false? (m/validate schema invalid))))
      (is (= [:map
              [:x {:required true} 'boolean?]
              [:y {:required false} 'int?]
              [:z {:required true} 'string?]]
             (m/form schema1)
             (m/form schema2))))))

(deftest properties-test
  (testing "properties can be set and retrieved"
    (let [properties {:title "kikka"}]
      (is (= properties
             (m/properties [:schema properties])
             (m/properties [:schema properties int?])
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
