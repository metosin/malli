(ns malli.json-schema-test
  (:require [clojure.test :refer [deftest testing is are]]
            [malli.core-test]
            [malli.json-schema :as json-schema]
            [malli.core :as m]
            [malli.util :as mu]))

(def expectations
  [;; predicates
   [pos-int? {:type "integer", :minimum 1}]
   [float? {:type "number"}]
   ;; comparators
   [[:> 6] {:type "number", :exclusiveMinimum 6}]
   [[:>= 6] {:type "number", :minimum 6}]
   [[:< 6] {:type "number", :exclusiveMaximum 6}]
   [[:<= 6] {:type "number", :maximum 6}]
   [[:= "x"] {:const "x"}]
   ;; base
   [[:not string?] {:not {:type "string"}}]
   [[:and int? pos-int?] {:allOf [{:type "integer"}
                                  {:type "integer", :minimum 1}]}]
   [[:or int? string?] {:anyOf [{:type "integer"} {:type "string"}]}]
   [[:orn [:i int?] [:s string?]] {:anyOf [{:type "integer"} {:type "string"}]}]
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
     [:sized [:map {:gen/fmap '#(assoc % :type :sized)} [:type keyword?] [:size int?]]]
     [:human [:map {:gen/fmap '#(assoc % :type :human)} [:type keyword?] [:name string?] [:address [:map [:country keyword?]]]]]
     [::m/default :string]]
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
   [[:map-of string? string?] {:type "object"
                               :additionalProperties {:type "string"}}]
   [[:vector string?] {:type "array", :items {:type "string"}}]
   [[:sequential string?] {:type "array", :items {:type "string"}}]
   [[:set string?] {:type "array"
                    :items {:type "string"}
                    :uniqueItems true}]
   [[:enum 1 2 3] {:enum [1 2 3]}]
   [[:maybe string?] {:oneOf [{:type "string"} {:type "null"}]}]
   [[:tuple string? string?] {:type "array"
                              :items [{:type "string"} {:type "string"}]
                              :additionalItems false}]
   [[:re "^[a-z]+\\.[a-z]+$"] {:type "string", :pattern "^[a-z]+\\.[a-z]+$"}]
   [[:fn {:gen/elements [1]} int?] {}]
   [:any {}]
   [:nil {:type "null"}]
   [[:string {:min 1, :max 4}] {:type "string", :minLength 1, :maxLength 4}]
   [[:int {:min 1, :max 4}] {:type "integer", :minimum 1, :maximum 4}]
   [[:double {:min 1, :max 4}] {:type "number", :minimum 1, :maximum 4}]
   [:keyword {:type "string"}]
   [:qualified-keyword {:type "string"}]
   [:symbol {:type "string"}]
   [:qualified-symbol {:type "string"}]
   [:uuid {:type "string", :format "uuid"}]

   [[:=> :cat int?] {} :fn]
   [[:function [:=> :cat int?]] {} :fn]
   [ifn? {}]

   [integer? {:type "integer"}]
   #?@(:clj [[ratio? {:type "number"}]
             [rational? {:type "number"}]]
       :cljs [])
   ;; protocols
   [(reify
      m/Schema
      (-properties [_])
      (-parent [_] (reify m/IntoSchema (-type [_]) (-type-properties [_])))
      (-form [_])
      (-validator [_] int?)
      (-walk [t w p o] (m/-outer w t p nil o))
      json-schema/JsonSchema
      (-accept [_ _ _] {:type "custom"})) {:type "custom"}]
   ;; type-properties
   [malli.core-test/Over6 {:type "integer", :format "int64", :minimum 6}]
   [[malli.core-test/Over6 {:json-schema/example 42}] {:type "integer", :format "int64", :minimum 6, :example 42}]])

(deftest json-schema-test
  (doseq [[schema json-schema] expectations]
    (is (= json-schema (json-schema/transform schema))))

  (testing "full override"
    (is (= {:type "file"}
           (json-schema/transform
             [:map {:json-schema {:type "file"}} [:file any?]]))))

  (testing "Having all attributes optional in input should not output a required at all even empty. JSON-Schema validation will failed on this
            (see http://json-schema.org/understanding-json-schema/reference/object.html#required-properties and
             the rule: \"In Draft 4, required must contain at least one string.\")"
    (is (= {:type "object",
            :properties {:x1 {:title "x", :type "string"},
                         :x2 {:title "x"},
                         :x3 {:title "x", :type "string", :default "x"},
                         :x4 {:title "x-string", :default "x2"},
                         :x5 {:type "x-string"}},}
           (json-schema/transform
            [:map
             [:x1 {:json-schema/title "x"          :optional true} :string]
             [:x2 {:json-schema {:title "x"}       :optional true} [:string {:json-schema/default "x"}]]
             [:x3 {:json-schema/title "x"          :optional true} [:string {:json-schema/default "x"}]]
             [:x4 {:json-schema/title "x-string"   :optional true} [:string {:json-schema {:default "x2"}}]]
             [:x5 {:json-schema {:type "x-string"} :optional true} [:string {:json-schema {:default "x"}}]]]))))

  (testing "map-entry overrides"
    (is (= {:type "object",
            :properties {:x1 {:title "x", :type "string"},
                         :x2 {:title "x"},
                         :x3 {:title "x", :type "string", :default "x"},
                         :x4 {:title "x-string", :default "x2"},
                         :x5 {:type "x-string"}},
            :required [:x1 :x2 :x3 :x4 :x5]}
           (json-schema/transform
             [:map
              [:x1 {:json-schema/title "x"} :string]
              [:x2 {:json-schema {:title "x"}} [:string {:json-schema/default "x"}]]
              [:x3 {:json-schema/title "x"} [:string {:json-schema/default "x"}]]
              [:x4 {:json-schema/title "x-string"} [:string {:json-schema {:default "x2"}}]]
              [:x5 {:json-schema {:type "x-string"}} [:string {:json-schema {:default "x"}}]]]))))

  (testing "with properties"
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

(deftest util-schemas-test
  (let [registry (merge (m/default-schemas) (mu/schemas))]

    (testing "merge"
      (is (= {:title "merge",
              :type "object",
              :properties {:x {:type "integer", :example 42},
                           :y {:type "integer"},
                           :z {:type "integer"}},
              :required [:x :y :z]}
             (json-schema/transform
               [:merge {:title "merge"}
                [:map [:x {:json-schema/example 42} int?] [:y int?]]
                [:map [:z int?]]]
               {:registry registry}))))

    (testing "union"
      (is (= {:title "union",
              :type "object",
              :properties {:x {:anyOf [{:type "integer"} {:type "string"}]}
                           :y {:type "integer"}},
              :required [:x :y]}
             (json-schema/transform
               [:union {:title "union"}
                [:map [:x int?] [:y int?]]
                [:map [:x string?]]]
               {:registry registry}))))

    (testing "select-keys"
      (is (= {:title "select-keys"
              :type "object"
              :properties {:x {:type "integer"}}
              :required [:x]}
             (json-schema/transform
               [:select-keys {:title "select-keys"}
                [:map [:x int?] [:y int?]]
                [:x]]
               {:registry registry}))))))

(deftest references-test
  (is (= {:$ref "#/definitions/Order",
          :definitions {"Country" {:type "object",
                                   :properties {:name {:enum [:FI :PO]},
                                                :neighbors {:type "array"
                                                            :items {:$ref "#/definitions/Country"}}},
                                   :required [:name :neighbors]},
                        "Burger" {:type "object",
                                  :properties {:name {:type "string"},
                                               :description {:type "string"},
                                               :origin {:oneOf [{:$ref "#/definitions/Country"} {:type "null"}]},
                                               :price {:type "integer"
                                                       :minimum 1}},
                                  :required [:name :origin :price]},
                        "OrderLine" {:type "object",
                                     :properties {:burger {:$ref "#/definitions/Burger"},
                                                  :amount {:type "integer"}},
                                     :required [:burger :amount]},
                        "Order" {:type "object",
                                 :properties {:lines {:type "array"
                                                      :items {:$ref "#/definitions/OrderLine"}},
                                              :delivery {:type "object",
                                                         :properties {:delivered {:type "boolean"},
                                                                      :address {:type "object",
                                                                                :properties {:street {:type "string"},
                                                                                             :zip {:type "integer"},
                                                                                             :country {:$ref "#/definitions/Country"}},
                                                                                :required [:street :zip :country]}},
                                                         :required [:delivered :address]}},
                                 :required [:lines :delivery]}}}
         (json-schema/transform
           [:schema
            {:registry {"Country" [:map
                                   [:name [:enum :FI :PO]]
                                   [:neighbors [:vector [:ref "Country"]]]]
                        "Burger" [:map
                                  [:name string?]
                                  [:description {:optional true} string?]
                                  [:origin [:maybe "Country"]]
                                  [:price pos-int?]]
                        "OrderLine" [:map
                                     [:burger "Burger"]
                                     [:amount int?]]
                        "Order" [:map
                                 [:lines [:vector "OrderLine"]]
                                 [:delivery [:map
                                             [:delivered boolean?]
                                             [:address [:map
                                                        [:street string?]
                                                        [:zip int?]
                                                        [:country "Country"]]]]]]}}
            "Order"]))))

(deftest function-schema-test
  (is (= {} (json-schema/transform [:=> [:cat int? int?] int?]))))
