(ns malli.protobuf3-test
  (:require [clojure.test :refer [deftest is testing are]]
            [malli.protobuf3-schema :as pbuf]
            [clojure.string :as str]))

(deftest test-to-snake-case
  (testing "to-snake-case function"
    (are [input expected] (= expected (pbuf/to-snake-case input))
      "camelCase" "camel_case"
      "PascalCase" "pascal_case"
      "snake_case" "snake_case"
      "UPPER_CASE" "upper_case"
      "mixedCASE_with_underscores" "mixed_case_with_underscores"
      "alreadysnakecase" "alreadysnakecase")))

(deftest test-to-pascal-case
  (testing "to-pascal-case function"
    (are [input expected] (= expected (pbuf/to-pascal-case input))
      "camel_case" "CamelCase"
      "pascal_case" "PascalCase"
      "snake_case" "SnakeCase"
      "UPPER_CASE" "UpperCase"
      "mixed_CASE_with_underscores" "MixedCaseWithUnderscores"
      "alreadypascalcase" "Alreadypascalcase")))

(deftest test-malli-type->protobuf-type
  (testing "malli-type->protobuf-type function"
    (are [input expected] (= expected (pbuf/malli-type->protobuf-type input))
      clojure.core/string? "string"
      clojure.core/int? "int32"
      clojure.core/boolean? "bool"
      :string "string"
      :int "int32"
      :double "double"
      :boolean "bool"
      :keyword "string"
      :symbol "string"
      :uuid "string"
      :uri "string"
      :inst "google.protobuf.Timestamp"
      :nil "google.protobuf.NullValue"
      :unknown "bytes")))

(deftest test-transform-map-schema
  (testing "transform-map-schema function"
    (let [schema [:map
                  [:name string?]
                  [:age int?]]
          result (pbuf/transform-map-schema schema "TestMessage")]
      (is (= "message" (:type result)))
      (is (= "Testmessage" (:name result)))
      (is (= 2 (count (:fields result))))
      (is (= {:name "name" :type {:name "string"} :index 1} (first (:fields result))))
      (is (= {:name "age" :type {:name "int32"} :index 2} (second (:fields result)))))))

(deftest test-transform-vector-schema
  (testing "transform-vector-schema function"
    (let [schema [:vector string?]
          result (pbuf/transform-vector-schema schema "TestVector")]
      (is (= "repeated" (:type result)))
      (is (= {:name "string"} (:value-type result))))))

(deftest test-transform-enum-schema
  (testing "transform-enum-schema function"
    (let [schema [:enum :active :inactive]
          result (pbuf/transform-enum-schema schema "Status")]
      (is (= "enum" (:type result)))
      (is (= "Status" (:name result)))
      (is (= 2 (count (:values result))))
      (is (= {:name "ACTIVE" :index 0} (first (:values result))))
      (is (= {:name "INACTIVE" :index 1} (second (:values result)))))))

(deftest test-transform-schema
  (testing "transform-schema function"
    (testing "with map schema"
      (let [schema [:map [:name string?]]
            result (pbuf/transform-schema schema "TestMessage")]
        (is (= "message" (:type result)))
        (is (= "Testmessage" (:name result)))))

    (testing "with vector schema"
      (let [schema [:vector string?]
            result (pbuf/transform-schema schema "TestVector")]
        (is (= "repeated" (:type result)))))

    (testing "with enum schema"
      (let [schema [:enum :status :active :inactive]
            result (pbuf/transform-schema schema "Status")]
        (is (= "enum" (:type result)))))

    (testing "with primitive type"
      (let [schema string?
            result (pbuf/transform-schema schema "TestString")]
        (is (= {:name "string"} result))))))

(deftest test-transform-schema-primitive-types
  (testing "transform-schema with primitive types"
    (are [schema expected] (= expected (pbuf/transform-schema schema "TestField"))
      string? {:name "string"}
      int? {:name "int32"}
      boolean? {:name "bool"}
      double? {:name "double"}
      :string {:name "string"}
      :int {:name "int32"}
      :boolean {:name "bool"}
      :double {:name "double"})))

(deftest test-transform-schema-enum
  (testing "transform-schema with enum"
    (let [schema [:enum :red :green :blue]
          result (pbuf/transform-schema schema "Color")
          expected {:type "enum"
                    :name "Color"
                    :values [{:name "RED" :index 0}
                             {:name "GREEN" :index 1}
                             {:name "BLUE" :index 2}]}]
      (is (= expected result)))))

(deftest test-transform-schema-vector
  (testing "transform-schema with vector"
    (let [schema [:vector string?]
          result (pbuf/transform-schema schema "StringList")
          expected {:type "repeated"
                    :value-type {:name "string"}}]
      (is (= expected result)))))

(deftest test-transform-schema-nested-map
  (testing "transform-schema with nested map"
    (let [schema [:map
                  [:name string?]
                  [:age int?]]
          result (pbuf/transform-schema schema "Person")
          expected {:type "message"
                    :name "Person"
                    :fields [{:name "name" :type {:name "string"} :index 1}
                             {:name "age" :type {:name "int32"} :index 2}]}]
      (is (= expected result)))))

(deftest test-transform-schema-nested-vector
  (testing "transform-schema with nested vector"
    (let [schema [:vector [:map
                           [:x int?]
                           [:y int?]]]
          result (pbuf/transform-schema schema "PointList")
          expected {:type "repeated"
                    :value-type {:type "message"
                                 :name "Pointlist"
                                 :fields '({:name "x" :type {:name "int32"} :index 1}
                                           {:name "y" :type {:name "int32"} :index 2})}}]
      (is (= expected result)))))

(deftest test-transform-schema-deep-nested-structure
  (testing "transform-schema with deeply nested maps and vectors"
    (let [schema [:map
                  [:id string?]
                  [:metadata [:map
                              [:created_at inst?]
                              [:tags [:vector string?]]]]
                  [:data [:vector [:map
                                   [:name string?]
                                   [:details [:map
                                              [:type [:enum :type-a :type-b :type-c]]
                                              [:properties [:vector [:map
                                                                     [:key string?]
                                                                     [:value [:or string? int? boolean?]]
                                                                     [:nested [:vector [:map
                                                                                        [:sub_key string?]
                                                                                        [:sub_value any?]]]]]]]]]]]]]
          result (pbuf/transform-schema schema "DeepNestedStructure")
          expected {:type "message"
                    :name "Deepnestedstructure"
                    :fields [{:name "id" :type {:name "string"} :index 1}
                             {:name "metadata"
                              :type {:type "message"
                                     :name "Metadata"
                                     :fields [{:name "created_at" :type {:name "bytes"} :index 1}
                                              {:name "tags" :type {:type "repeated" :value-type {:name "string"}} :index 2}]}
                              :index 2}
                             {:name "data"
                              :type {:type "repeated"
                                     :value-type
                                     {:type "message"
                                      :name "Data"
                                      :fields
                                      [{:name "name" :type {:name "string"} :index 1}
                                       {:name "details"
                                        :type {:type "message"
                                               :name "Details"
                                               :fields
                                               [{:name "type"
                                                 :type {:type "enum"
                                                        :name "Type"
                                                        :values [{:name "TYPE_A" :index 0}
                                                                 {:name "TYPE_B" :index 1}
                                                                 {:name "TYPE_C" :index 2}]}
                                                 :index 1}
                                                {:name "properties"
                                                 :type {:type "repeated"
                                                        :value-type
                                                        {:type "message"
                                                         :name "Properties"
                                                         :fields
                                                         [{:name "key" :type {:name "string"} :index 1}
                                                          {:name "value" :type {:name "bytes"} :index 2}
                                                          {:name "nested"
                                                           :type {:type "repeated"
                                                                  :value-type
                                                                  {:type "message"
                                                                   :name "Nested"
                                                                   :fields
                                                                   [{:name "sub_key" :type {:name "string"} :index 1}
                                                                    {:name "sub_value" :type {:name "bytes"} :index 2}]}}
                                                           :index 3}]}}
                                                 :index 2}]}
                                        :index 2}]}}
                              :index 3}]}]
      (is (= expected result)))))

(deftest test-collect-definitions
  (testing "collect-definitions function"
    (let [schema {:type "message"
                  :name "TestMessage"
                  :fields [{:name "sub_message"
                            :type {:type "message"
                                   :name "SubMessage"
                                   :fields [{:name "enum_field"
                                             :type {:type "enum"
                                                    :name "TestEnum"
                                                    :values [{:name "VALUE1" :index 0}]}}]}}]}
          result (pbuf/collect-definitions schema)]
      (is (= 3 (count result)))
      (is (some #(= "TestMessage" (:name %)) result))
      (is (some #(= "SubMessage" (:name %)) result))
      (is (some #(= "TestEnum" (:name %)) result)))))

(defn normalize-whitespace [s]
  (-> s
      (str/replace #"\s+" " ")
      (str/replace #"\n+" "\n")
      str/trim))

(deftest test-generate-field
  (testing "generate-field function"
    (are [input expected] (= expected (normalize-whitespace (pbuf/generate-field input)))
      {:type {:name "string"} :name "name" :index 1}
      "string name = 1;"

      {:type {:name "int32"} :name "age" :index 2}
      "int32 age = 2;"

      {:type {:type "repeated" :value-type {:name "string"}} :name "hobbies" :index 3}
      "repeated string hobbies = 3;"

      {:type {:type "enum" :name "Status"} :name "status" :index 4}
      "Status status = 4;")))

(deftest test-generate-message
  (testing "generate-message function"
    (let [input {:name "Person"
                 :fields [{:type {:name "string"} :name "name" :index 1}
                          {:type {:name "int32"} :name "age" :index 2}]}
          expected (normalize-whitespace "message Person { string name = 1; int32 age = 2; }")
          result (normalize-whitespace (pbuf/generate-message input))]
      (is (= expected result)))))

(deftest test-generate-enum
  (testing "generate-enum function"
    (let [input {:name "Status"
                 :values [{:name "ACTIVE" :index 0}
                          {:name "INACTIVE" :index 1}]}
          expected "enum Status { ACTIVE = 0; INACTIVE = 1; }"
          result (normalize-whitespace (pbuf/generate-enum input))]
      (is (= expected result)))))

(deftest test-generate-definition
  (testing "generate-definition function for message"
    (let [input {:type "message"
                 :name "Person"
                 :fields [{:type {:name "string"} :name "name" :index 1}]}
          expected "message Person { string name = 1; }"
          result (normalize-whitespace (pbuf/generate-definition input))]
      (is (= expected result))))

  (testing "generate-definition function for enum"
    (let [input {:type "enum"
                 :name "Status"
                 :values [{:name "ACTIVE" :index 0}]}
          expected "enum Status { ACTIVE = 0; }"
          result (normalize-whitespace (pbuf/generate-definition input))]
      (is (= expected result)))))

(deftest test-sort-definitions
  (testing "sort-definitions function"
    (let [input [{:type "message" :name "Person" :fields []}
                 {:type "enum" :name "Status" :values []}
                 {:type "message" :name "Address" :fields []}
                 {:type "message" :name "Message" :fields []}]
          expected [{:type "enum" :name "Status" :values []}
                    {:type "message" :name "Person" :fields []}
                    {:type "message" :name "Address" :fields []}
                    {:type "message" :name "Message" :fields []}]
          result (pbuf/sort-definitions input)]
      (is (= expected result)))))

(deftest test-generate-protobuf3
  (testing "generate-protobuf3 function with various schema types"
    (testing "Simple message with primitive types"
      (let [schema {:type "message"
                    :name "Message"
                    :fields [{:name "name" :type {:name "string"} :index 1}
                             {:name "age" :type {:name "int32"} :index 2}
                             {:name "is_student" :type {:name "bool"} :index 3}]}
            result (normalize-whitespace (pbuf/generate-protobuf3 schema))
            expected (normalize-whitespace "
              syntax = \"proto3\";
              message Message {
                string name = 1;
                int32 age = 2;
                bool is_student = 3;
              }")]
        (is (= expected result))))

    (testing "Message with enum"
      (let [schema {:type "message"
                    :name "Message"
                    :fields [{:name "name" :type {:name "string"} :index 1}
                             {:name "status" :type {:type "enum"
                                                    :name "Status"
                                                    :values [{:name "ACTIVE" :index 0}
                                                             {:name "INACTIVE" :index 1}]} :index 2}]}
            result (normalize-whitespace (pbuf/generate-protobuf3 schema))
            expected (normalize-whitespace "
              syntax = \"proto3\";
              enum Status {
                ACTIVE = 0;
                INACTIVE = 1;
              }
              message Message {
                string name = 1;
                Status status = 2;
              }")]
        (is (= expected result))))

    (testing "Message with nested map"
      (let [schema {:type "message"
                    :name "Message"
                    :fields [{:name "name" :type {:name "string"} :index 1}
                             {:name "address" :type {:type "message"
                                                     :name "Address"
                                                     :fields [{:name "street" :type {:name "string"} :index 1}
                                                              {:name "city" :type {:name "string"} :index 2}]} :index 2}]}
            result (normalize-whitespace (pbuf/generate-protobuf3 schema))
            expected (normalize-whitespace "
              syntax = \"proto3\";
              message Address {
                string street = 1;
                string city = 2;
              }
              message Message {
                string name = 1;
                Address address = 2;
              }")]
        (is (= expected result))))

    (testing "Message with nested vector"
      (let [schema {:type "message"
                    :name "Message"
                    :fields [{:name "name" :type {:name "string"} :index 1}
                             {:name "scores" :type {:type "repeated"
                                                    :value-type {:name "int32"}} :index 2}]}
            result (normalize-whitespace (pbuf/generate-protobuf3 schema))
            expected (normalize-whitespace "
              syntax = \"proto3\";
              message Message {
                string name = 1;
                repeated int32 scores = 2;
              }")]
        (is (= expected result))))

    (testing "Complex nested structure"
      (let [schema {:type "message"
                    :name "Message"
                    :fields [{:name "name" :type {:name "string"} :index 1}
                             {:name "departments" :type {:type "repeated"
                                                         :value-type {:type "message"
                                                                      :name "Department"
                                                                      :fields [{:name "name" :type {:name "string"} :index 1}
                                                                               {:name "employees" :type {:type "repeated"
                                                                                                         :value-type {:type "message"
                                                                                                                      :name "Employee"
                                                                                                                      :fields [{:name "name" :type {:name "string"} :index 1}
                                                                                                                               {:name "id" :type {:name "int32"} :index 2}
                                                                                                                               {:name "role" :type {:type "enum"
                                                                                                                                                    :name "Role"
                                                                                                                                                    :values [{:name "MANAGER" :index 0}
                                                                                                                                                             {:name "DEVELOPER" :index 1}
                                                                                                                                                             {:name "DESIGNER" :index 2}]} :index 3}]}} :index 2}]}} :index 2}]}
            result (normalize-whitespace (pbuf/generate-protobuf3 schema))
            expected (normalize-whitespace "
              syntax = \"proto3\";
              enum Role {
                MANAGER = 0;
                DEVELOPER = 1;
                DESIGNER = 2;
              }
              message Department {
                string name = 1;
                repeated Employee employees = 2;
              }
              message Employee {
                string name = 1;
                int32 id = 2;
                Role role = 3;
              }

              message Message {
                string name = 1;
                repeated Department departments = 2;
              }")]
        (is (= expected result))))))

(deftest test-transform
  (testing "transform function with various schema types"
    (testing "Simple message with primitive types"
      (let [schema [:map
                    [:name string?]
                    [:age int?]
                    [:is-student boolean?]]
            result (normalize-whitespace (pbuf/transform schema))
            expected (normalize-whitespace "
              syntax = \"proto3\";
              message Message {
                string name = 1;
                int32 age = 2;
                bool is_student = 3;
              }")]
        (is (= expected result))))

    (testing "Message with enum"
      (let [schema [:map
                    [:name string?]
                    [:status [:enum :active :inactive]]]
            result (normalize-whitespace (pbuf/transform schema))
            expected (normalize-whitespace "
              syntax = \"proto3\";
              enum Status {
                ACTIVE = 0;
                INACTIVE = 1;
              }
              message Message {
                string name = 1;
                Status status = 2;
              }")]
        (is (= expected result))))

    (testing "Message with nested map"
      (let [schema [:map
                    [:name string?]
                    [:address [:map
                               [:street string?]
                               [:city string?]]]]
            result (normalize-whitespace (pbuf/transform schema))
            expected (normalize-whitespace "
              syntax = \"proto3\";
              message Address {
                string street = 1;
                string city = 2;
              }
              message Message {
                string name = 1;
                Address address = 2;
              }")]
        (is (= expected result))))

    (testing "Message with nested vector"
      (let [schema [:map
                    [:name string?]
                    [:scores [:vector int?]]]
            result (normalize-whitespace (pbuf/transform schema))
            expected (normalize-whitespace "
              syntax = \"proto3\";
              message Message {
                string name = 1;
                repeated int32 scores = 2;
              }")]
        (is (= expected result))))

    (testing "Complex nested structure"
      (let [schema [:map
                    [:name string?]
                    [:departments [:vector
                                   [:map
                                    [:name string?]
                                    [:employees [:vector
                                                 [:map
                                                  [:name string?]
                                                  [:id int?]
                                                  [:role [:enum :manager :developer :designer]]]]]]]]]
            result (normalize-whitespace (pbuf/transform schema))
            expected (normalize-whitespace "
              syntax = \"proto3\";
              enum Role {
                MANAGER = 0;
                DEVELOPER = 1;
                DESIGNER = 2;
              }

              message Departments {
                string name = 1;
                repeated Employees employees = 2;
              }

              message Employees {
                string name = 1;
                int32 id = 2;
                Role role = 3;
              }

              message Message {
                string name = 1;
                repeated Departments departments = 2;
              }")]
        (is (= expected result))))))

(deftest test-transform-deep-nested-structure
  (testing "transform function with deeply nested maps and vectors"
    (let [schema [:map
                  [:id string?]
                  [:metadata [:map
                              [:created_at inst?]
                              [:tags [:vector string?]]]]
                  [:data [:vector [:map
                                   [:name string?]
                                   [:details [:map
                                              [:type [:enum :type-a :type-b :type-c]]
                                              [:properties [:vector [:map
                                                                     [:key string?]
                                                                     [:value [:or string? int? boolean?]]
                                                                     [:nested [:vector [:map
                                                                                        [:sub_key string?]
                                                                                        [:sub_value any?]]]]]]]]]]]]]
          result (normalize-whitespace (pbuf/transform schema))
          expected (normalize-whitespace "
            syntax = \"proto3\";

            enum Type {
              TYPE_A = 0;
              TYPE_B = 1;
              TYPE_C = 2;
            }

            message Metadata {
              bytes created_at = 1;
              repeated string tags = 2;
            }

            message Data {
              string name = 1;
              Details details = 2;
            }

            message Details {
              Type type = 1;
              repeated Properties properties = 2;
            }

            message Properties {
              string key = 1;
              bytes value = 2;
              repeated Nested nested = 3;
            }

            message Nested {
              string sub_key = 1;
              bytes sub_value = 2;
            }

            message Message {
              string id = 1;
              Metadata metadata = 2;
              repeated Data data = 3;
            }
          ")]
      (is (= expected result)))))
