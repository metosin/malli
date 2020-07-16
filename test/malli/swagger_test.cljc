(ns malli.swagger-test
  (:require [clojure.test :refer [deftest testing is are]]
            [malli.swagger :as swagger]
            [malli.core :as m]))

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
   [[:multi {:dispatch :type
             :decode/string '(fn [x] (update x :type keyword))}
     [:sized [:map [:type keyword?] [:size int?]]]
     [:human [:map [:type keyword?] [:name string?] [:address [:map [:country keyword?]]]]]]
    {:type "object",
     :properties {:type {:type "string"}
                  :size {:type "integer"
                         :format "int64"}},
     :required [:type :size],
     :x-anyOf [{:type "object",
                :properties {:type {:type "string"}
                             :size {:type "integer"
                                    :format "int64"}},
                :required [:type :size]}
               {:type "object",
                :properties {:type {:type "string"},
                             :name {:type "string"},
                             :address {:type "object"
                                       :properties {:country {:type "string"}}
                                       :required [:country]}},
                :required [:type :name :address]}]}]
   [[:map-of string? string?] {:type "object"
                               :additionalProperties {:type "string"}}]
   [[:vector string?] {:type "array", :items {:type "string"}}]
   [[:list string?] {:type "array", :items {:type "string"}}]
   [[:sequential string?] {:type "array", :items {:type "string"}}]
   [[:set string?] {:type "array"
                    :items {:type "string"}
                    :uniqueItems true}]
   [[:enum 1 2 3] {:enum [1 2 3]}]
   [[:maybe string?] {:type "string", :x-nullable true}]
   [[:tuple string? string?] {:type "array"
                              :items {}
                              :x-items [{:type "string"}
                                        {:type "string"}]}]
   [[:re "^[a-z]+\\.[a-z]+$"] {:type "string", :pattern "^[a-z]+\\.[a-z]+$"}]
   [[:string {:min 1, :max 4}] {:type "string", :minLength 1, :maxLength 4}]
   ;; protocols
   [(reify
      m/Schema
      (-properties [_])
      (-walk [t w p o] (m/-outer w t p nil o))
      swagger/SwaggerSchema
      (-accept [_ _ _] {:type "custom"})) {:type "custom"}]])

(deftest swagger-test
  (doseq [[schema swagger-schema] expectations]
    (is (= swagger-schema (swagger/transform schema))))

  (testing "full override"
    (is (= {:type "file"}
           (swagger/transform
             [:map {:swagger {:type "file"}} [:file any?]])))
    (is (= {:type "file"}
           (swagger/transform
             [:map {:json-schema {:type "file"}} [:file any?]])))
    (is (= {:type "file"}
           (swagger/transform
             [:map {:swagger {:type "file"}
                    :json-schema {:type "file2"}} [:file any?]]))))

  (testing "map-entry overrides"
    (is (= {:type "object",
            :properties {:x1 {:title "x", :type "string"},
                         :x2 {:title "x"},
                         :x3 {:title "x", :type "string", :default "x"},
                         :x4 {:title "x-string", :default "x2"},
                         :x5 {:type "x-string"}},
            :required [:x1 :x2 :x3 :x4 :x5]}
           (swagger/transform
             [:map
              [:x1 {:swagger/title "x"} :string]
              [:x2 {:swagger {:title "x"}} [:string {:swagger/default "x"}]]
              [:x3 {:swagger/title "x"} [:string {:swagger/default "x"}]]
              [:x4 {:swagger/title "x-string"} [:string {:swagger {:default "x2"}}]]
              [:x5 {:swagger {:type "x-string"}} [:string {:swagger {:default "x"}}]]]))))

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
                    :default 42} int?])))
    (is (= {:title "age2"
            :type "integer"
            :format "int64"
            :description "blabla2"
            :default 422
            :example 422
            :x-allOf [{:type "integer", :format "int64"}]}
           (swagger/transform
             [:and {:title "age"
                    :json-schema/title "age2"
                    :description "blabla"
                    :json-schema/description "blabla2"
                    :default 42
                    :json-schema/default 422
                    :json-schema/example 422} int?])))
    (is (= {:title "age3"
            :type "integer"
            :format "int64"
            :description "blabla3"
            :default 4222
            :example 4222
            :x-allOf [{:type "integer", :format "int64"}]}
           (swagger/transform
             [:and {:title "age"
                    :json-schema/title "age2"
                    :swagger/title "age3"
                    :description "blabla"
                    :json-schema/description "blabla2"
                    :swagger/description "blabla3"
                    :default 42
                    :json-schema/default 422
                    :swagger/default 4222
                    :json-schema/example 422
                    :swagger/example 4222} int?])))))
