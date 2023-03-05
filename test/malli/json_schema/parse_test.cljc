(ns malli.json-schema.parse-test
  (:require [clojure.test :refer [deftest is testing]]
            [malli.core :as m]
            [malli.core-test]
            [malli.json-schema :as json-schema]
            [malli.json-schema.parse :as sut]
            [malli.util :as mu]))

(def expectations
  [ ;; predicates
   [[:and :int [:>= 1]] {:type "integer", :minimum 1} :one-way true]
   [[:and :int [:>= 1]] {:allOf [{:type "integer"} {:type "number", :minimum 1}]}]
   [[:> 0] {:type "number" :exclusiveMinimum 0}]
   [:double {:type "number"}]
   ;; comparators
   [[:> 6] {:type "number", :exclusiveMinimum 6}]
   [[:>= 6] {:type "number", :minimum 6}]
   [[:< 6] {:type "number", :exclusiveMaximum 6}]
   [[:<= 6] {:type "number", :maximum 6}]
   [[:= "x"] {:const "x"}]
   ;; base
   [[:not :string] {:not {:type "string"}}]
   [[:and :int [:and :int [:>= 1]]] {:allOf [{:type "integer"}
                                             {:type "integer", :minimum 1}]} :one-way true]
   [[:or :int :string] {:anyOf [{:type "integer"} {:type "string"}]}]
   [[:map
     [:a :string]
     [:b {:optional true} :string]
     [:c :string]] {:type "object"
     :properties {:a {:type "string"}
                  :b {:type "string"}
                  :c {:type "string"}}
     :required [:a :c]}]
   [[:or [:map [:type :string] [:size :int]] [:map [:type :string] [:name :string] [:address [:map [:country :string]]]] :string]
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
             {:type "string"}]} :one-way true]
   [[:map-of :string :string] {:type "object"
                               :additionalProperties {:type "string"}}]
   [[:vector :string] {:type "array", :items {:type "string"}}]
   [[:set :string] {:type "array"
                    :items {:type "string"}
                    :uniqueItems true}]
   [[:enum 1 2 "3"] {:enum [1 2 "3"]}]
   [[:and :int [:enum 1 2 3]] {:type "integer" :enum [1 2 3]} :one-way true]
   [[:enum 1.1 2.2 3.3] {:type "number" :enum [1.1 2.2 3.3]}]
   [[:enum "kikka" "kukka"] {:type "string" :enum ["kikka" "kukka"]}]
   [[:enum :kikka :kukka] {:type "string" :enum [:kikka :kukka]}]
   [[:enum 'kikka 'kukka] {:type "string" :enum ['kikka 'kukka]}]
   [[:or :string :nil] {:oneOf [{:type "string"} {:type "null"}]} :one-way true]
   [[:or :string :nil] {:anyOf [{:type "string"} {:type "null"}]}]
   [[:tuple :string :string] {:type "array"
                              :items [{:type "string"} {:type "string"}]
                              :additionalItems false}]
   [[:re "^[a-z]+\\.[a-z]+$"] {:type "string", :pattern "^[a-z]+\\.[a-z]+$"}]
   [:any {}]
   [:nil {:type "null"}]
   [[:string {:min 1, :max 4}] {:type "string", :minLength 1, :maxLength 4}]
   [[:and :int [:<= 4] [:>= 1]] {:type "integer", :minimum 1, :maximum 4} :one-way true]
   [[:and [:<= 4] [:>= 1]] {:type "number", :minimum 1, :maximum 4} :one-way true]
   [:uuid {:type "string", :format "uuid"}]

   [:int {:type "integer"}]
   ;; type-properties
   [[:and :int [:>= 6]] {:type "integer", :format "int64", :minimum 6} :one-way true]
   [[:and {:json-schema/example 42} :int [:>= 6]] {:type "integer", :format "int64", :minimum 6, :example 42} :one-way true]])

(deftest json-schema-test
  (doseq [[schema json-schema & {:keys [one-way]}] expectations]
    (testing json-schema
      (is (= schema
             (m/form (sut/schema->malli json-schema)))))

    (when-not one-way
      (testing (str "round trip " json-schema "\n" schema)
        (is (= json-schema
               (-> json-schema sut/schema->malli malli.json-schema/transform))))))

  (testing "full override"
    (is (= [:map {:json-schema {:type "file"}} [:file :any]]
           (m/form (sut/schema->malli {:type "file"})))))

  (testing "with properties"
    (is (= [:map
            [:x1 [:string {:json-schema/title "x"}]]
            [:x2 [:any #:json-schema{:default "x" :title "x"}]]
            [:x3 [:string #:json-schema{:title "x" :default "x"}]]
            [:x4 {:optional true} [:any #:json-schema{:title "x-string" :default "x2"}]]]

           (m/form (sut/schema->malli {:type "object",
                                       :properties {:x1 {:title "x", :type "string"}
                                                    :x2 {:title "x", :default "x"}
                                                    :x3 {:title "x", :type "string", :default "x"}
                                                    :x4 {:title "x-string", :default "x2"}},
                                       :required [:x1 :x2 :x3]}))))

    #_(testing "custom type"
        (is (= [:map
                [:x5 {:json-schema/type "x-string"} :string]]
               (m/form (sut/schema->malli {:type "object", :properties {:x5 {:type "x-string"}}, :required [:x5]})))))

    (is (= [:and {:json-schema/title "age"
                  :json-schema/description "blabla"
                  :json-schema/default 42} :int]
           (m/form (sut/schema->malli {:allOf [{:type "integer"}]
                                       :title "age"
                                       :description "blabla"
                                       :default 42}))))))
