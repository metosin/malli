(ns malli.swagger-test
  (:require [clojure.test :refer [deftest testing is are]]
            [malli.swagger :as swagger]))

(def expectations
  [;; predicates
   [pos-int? {:type "integer", :format "int64", :minimum 1}]
   [float? {:type "number" :format "float"}]
   ;; comparators
   [[:> 6] {:type "number", :format "double", :exclusiveMinimum 6}]
   [[:>= 6] {:type "number", :format "double", :minimum 6}]
   [[:< 6] {:type "number", :format "double", :exclusiveMaximum 6}]
   [[:<= 6] {:type "number", :format "double", :maximum 6}]
   ;; base
   [[:and int? pos-int?] {:type "integer"
                          :format "int64"
                          :x-allOf [{:type "integer", :format "int64"}
                                    {:type "integer", :format "int64", :minimum 1}]}]
   [[:or int? string?] {:type "integer"
                        :format "int64"
                        :x-anyOf [{:type "integer", :format "int64"}
                                  {:type "string"}]}]
   [[:map
     [:a string?]
     [:b {:optional true} string?]
     [:c {:optional false} string?]] {:type "object"
                                      :properties {:a {:type "string"}
                                                   :b {:type "string"}
                                                   :c {:type "string"}}
                                      :required [:a :c]}]
   [[:map-of string? string?] {:type "object"
                               :additionalProperties {:type "string"}}]
   [[:vector string?] {:type "array", :items [{:type "string"}]}]
   [[:list string?] {:type "array", :items [{:type "string"}]}]
   [[:sequential string?] {:type "array", :items [{:type "string"}]}]
   [[:set string?] {:type "array"
                    :items [{:type "string"}]
                    :uniqueItems true}]
   [[:enum 1 2 3] {:enum [1 2 3]}]
   [[:maybe string?] {:type "string", :x-nullable true}]
   [[:tuple string? string?] {:type "array"
                              :items {}
                              :x-items [{:type "string"}
                                        {:type "string"}]}]
   [[:re "^[a-z]+\\.[a-z]+$"] {:type "string", :pattern "^[a-z]+\\.[a-z]+$"}]])

(deftest swagger-test
  (doseq [[schema swagger-schema] expectations]
    (is (= swagger-schema (swagger/transform schema))))
  (testing "with properties"
    (is (= {:title "age"
            :type "integer"
            :format "int64"
            :description "blabla"
            :default 42
            :x-allOf [{:type "integer", :format "int64"}]}
           (swagger/transform
             [:and {:title "age"
                    :description "blabla"
                    :default 42} int?]))
        (is (= {:title "age"
                :type "integer"
                :format "int64"
                :description "blabla"
                :default 422
                :example 422
                :x-allOf [{:type "integer", :format "int64"}]}
               (swagger/transform
                 [:and {:title "age"
                        :json-schema/title "age2"
                        :json-schema/swagger "age3"
                        :description "blabla"
                        :json-schema/description "blabla2"
                        :default 42
                        :swagger/default 422
                        :swagger/example 422} int?]))))))
