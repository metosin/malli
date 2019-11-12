(ns malli.json-schema-test
  (:require [clojure.test :refer [deftest testing is are]]
            [malli.json-schema :as json-schema]))

(def expectations
  [;; predicates
   [pos-int? {:type "integer", :format "int64", :minimum 1}]
   [float? {:type "number"}]
   ;; comparators
   [[:> 6] {:type "number", :format "double", :exclusiveMinimum 6}]
   [[:>= 6] {:type "number", :format "double", :minimum 6}]
   [[:< 6] {:type "number", :format "double", :exclusiveMaximum 6}]
   [[:<= 6] {:type "number", :format "double", :maximum 6}]
   ;; base
   [[:and int? pos-int?] {:allOf [{:type "integer", :format "int64"}
                                  {:type "integer", :format "int64" :minimum 1}]}]
   [[:or int? string?] {:anyOf [{:type "integer", :format "int64"} {:type "string"}]}]
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
   [[:maybe string?] {:oneOf [{:type "string"} {:type "null"}]}]
   [[:tuple string? string?] {:type "array"
                              :items [{:type "string"} {:type "string"}]
                              :additionalItems false}]
   [[:re "^[a-z]+\\.[a-z]+$"] {:type "string", :pattern "^[a-z]+\\.[a-z]+$"}]])

(deftest json-schema-test
  (doseq [[schema json-schema] expectations]
    (is (= json-schema (json-schema/transform schema))))
  (testing "with properties"
    (is (= {:allOf [{:type "integer", :format "int64"}]
            :title "age"
            :description "blabla"
            :default 42}
           (json-schema/transform
             [:and {:title "age"
                    :description "blabla"
                    :default 42} int?]))
        (is (= {:allOf [{:type "integer", :format "int64"}]
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
                        :json-schema/example 422} int?]))))))
