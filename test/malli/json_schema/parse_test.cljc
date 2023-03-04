(ns malli.json-schema.parse-test
  (:require [clojure.test :refer [deftest is testing]]
            [malli.core :as m]
            [malli.core-test]
            [malli.json-schema :as json-schema]
            [malli.json-schema.parse :as sut]
            [malli.util :as mu]))

(def expectations
  [;; predicates
   [(m/schema pos-int?) {:type "integer", :minimum 1}]
   [pos? {:type "number" :exclusiveMinimum 0}]
   [number? {:type "number"}]
   ;; comparators
   [[:> 6] {:type "number", :exclusiveMinimum 6}]
   [[:>= 6] {:type "number", :minimum 6}]
   [[:< 6] {:type "number", :exclusiveMaximum 6}]
   [[:<= 6] {:type "number", :maximum 6}]
   [[:= "x"] {:const "x"}]
   ;; base
   [[:not :string] {:not {:type "string"}}]
   [[:and int? pos-int?] {:allOf [{:type "integer"}
                                  {:type "integer", :minimum 1}]}]
   [[:or int? :string] {:anyOf [{:type "integer"} {:type "string"}]}]
   [[:map
     [:a :string]
     [:b {:optional true} :string]
     [:c :string]] {:type "object"
     :properties {:a {:type "string"}
                  :b {:type "string"}
                  :c {:type "string"}}
     :required [:a :c]}]
   [[:or [:map [:type :string] [:size int?]] [:map [:type :string] [:name :string] [:address [:map [:country :string]]]] :string]
    {:anyOf [{:type "object",
              :properties {:type {:type "string"}
                           :size {:type "integer"}},
              :required [:type :size]}
             {:type "object",
              :properties {:type {:type "string"},
                           :name {:type "string"},
                           :address {:type "object"
                                     :properties {:country {:type "string"}}
                                     :required [:country]}},
              :required [:type :name :address]}
             {:type "string"}]}]
   [[:or [:map [:type :string] [:size int?]] [:map [:type :string] [:name :string] [:address [:map [:country :string]]]] :string]
    {:oneOf [{:type "object",
              :properties {:type {:type "string"}
                           :size {:type "integer"}},
              :required [:type :size]}
             {:type "object",
              :properties {:type {:type "string"},
                           :name {:type "string"},
                           :address {:type "object"
                                     :properties {:country {:type "string"}}
                                     :required [:country]}},
              :required [:type :name :address]}
             {:type "string"}]}]
   [[:map-of :string :string] {:type "object"
                               :additionalProperties {:type "string"}}]
   [[:vector :string] {:type "array", :items {:type "string"}}]
   [[:set :string] {:type "array"
                    :items {:type "string"}
                    :uniqueItems true}]
   [[:enum 1 2 "3"] {:enum [1 2 "3"]}]
   [[:enum 1 2 3] {:type "integer" :enum [1 2 3]}]
   [[:enum 1.1 2.2 3.3] {:type "number" :enum [1.1 2.2 3.3]}]
   [[:enum "kikka" "kukka"] {:type "string" :enum ["kikka" "kukka"]}]
   [[:enum :kikka :kukka] {:type "string" :enum [:kikka :kukka]}]
   [[:enum 'kikka 'kukka] {:type "string" :enum ['kikka 'kukka]}]
   [[:or :string :nil] {:oneOf [{:type "string"} {:type "null"}]}]
   [[:tuple :string :string] {:type "array"
                              :items [{:type "string"} {:type "string"}]
                              :additionalItems false}]
   [[:re "^[a-z]+\\.[a-z]+$"] {:type "string", :pattern "^[a-z]+\\.[a-z]+$"}]
   [:any {}]
   [:nil {:type "null"}]
   [[:string {:min 1, :max 4}] {:type "string", :minLength 1, :maxLength 4}]
   [[:and [:<= 4] pos-int?] {:type "integer", :minimum 1, :maximum 4}]
   [[:and [:<= 4] [:>= 1]] {:type "number", :minimum 1, :maximum 4}]
   [:uuid {:type "string", :format "uuid"}]

   [int? {:type "integer"}]
   ;; type-properties
   [[:>= 6] {:type "integer", :format "int64", :minimum 6}]
   [[:>= {:json-schema/example 42} 6] {:type "integer", :format "int64", :minimum 6, :example 42}]])

(deftest json-schema-test
  (doseq [[schema json-schema] expectations]
    (testing json-schema
      (is (mu/equals (m/schema schema)
                     (sut/schema->malli json-schema)))))

  #_(testing "with properties"
      (is (= {:allOf [{:type "integer"}]
              :title "age"
              :description "blabla"
              :default 42}
             (json-schema/transform
              [:and {:title "age"
                     :description "blabla"
                     :default 42} int?])))
      (is (= {:allOf [{:type "integer"}]
              :title "age2"
              :description "blabla2"
              :default 422
              :example 422}
             (json-schema/transform
              [:and {:title "age"
                     :json-schema/title "age2"
                     :description "blabla"
                     :json-schema/description "blabla2"
                     :default 42
                     :json-schema/default 422
                     :json-schema/example 422} int?])))))
