(ns malli.swagger-test
  (:require [clojure.test :refer [deftest is testing]]
            [malli.core :as m]
            [malli.core-test]
            [malli.swagger :as swagger]
            [malli.util :as mu]))

(def expectations
  [;; predicates
   [pos-int? {:type "integer", :format "int64", :minimum 1}]
   [float? {:type "number" :format "float"}]
   ;; comparators
   [[:> 6] {:type "number", :exclusiveMinimum 6}]
   [[:>= 6] {:type "number", :minimum 6}]
   [[:< 6] {:type "number", :exclusiveMaximum 6}]
   [[:<= 6] {:type "number", :maximum 6}]
   ;; base
   [[:not string?] {:x-not {:type "string"}}]
   [[:and int? pos-int?] {:type "integer"
                          :format "int64"
                          :x-allOf [{:type "integer", :format "int64"}
                                    {:type "integer", :format "int64", :minimum 1}]}]
   [[:or int? string?] {:type "integer"
                        :format "int64"
                        :x-anyOf [{:type "integer", :format "int64"}
                                  {:type "string"}]}]
   [[:or int? :nil] {:type "integer"
                     :format "int64"
                     :x-anyOf [{:type "integer", :format "int64"}
                               {:type "null"}]}]
   [[:or :nil int?] {:type "integer"
                     :format "int64"
                     :x-anyOf [{:type "null"}
                               {:type "integer", :format "int64"}]}]
   [[:or [:or :nil int?] [:or :nil int?]] {:type "integer"
                                           :format "int64"
                                           :x-anyOf [{:type "integer", :format "int64", :x-anyOf [{:type "null"} {:type "integer", :format "int64"}]}
                                                     {:type "integer", :format "int64", :x-anyOf [{:type "null"} {:type "integer", :format "int64"}]}]}]
   [[:and int? :nil] {:type "integer"
                      :format "int64"
                      :x-allOf [{:type "integer", :format "int64"}
                                {:type "null"}]}]
   [[:and :nil int?] {:type "integer"
                      :format "int64"
                      :x-allOf [{:type "null"}
                                {:type "integer", :format "int64"}]}]
   [[:multi {:dispatch :whatever}
     [:a int?]
     [:b :nil]]
    {:type "integer"
     :format "int64"
     :x-anyOf [{:type "integer", :format "int64"}
               {:type "null"}]}]
   [[:multi {:dispatch :whatever}
     [:a :nil]
     [:b int?]]
    {:type "integer"
     :format "int64"
     :x-anyOf [{:type "null"}
               {:type "integer", :format "int64"}]}]
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
    {:type "object"
     :properties {:type {:type "string"}
                  :size {:type "integer"
                         :format "int64"}}
     :required [:type :size]
     :x-anyOf [{:type "object"
                :properties {:type {:type "string"}
                             :size {:type "integer"
                                    :format "int64"}}
                :required [:type :size]}
               {:type "object"
                :properties {:type {:type "string"}
                             :name {:type "string"}
                             :address {:type "object"
                                       :properties {:country {:type "string"}}
                                       :required [:country]}}
                :required [:type :name :address]}]}]
   [[:map-of string? string?] {:type "object"
                               :additionalProperties {:type "string"}}]
   [[:vector string?] {:type "array", :items {:type "string"}}]
   [[:sequential string?] {:type "array", :items {:type "string"}}]
   [[:set string?] {:type "array"
                    :items {:type "string"}
                    :uniqueItems true}]
   [[:enum 1 2 "3"] {:enum [1 2 "3"]}]
   [[:enum 1 2 3] {:type "integer" :enum [1 2 3]}]
   [[:enum 1.1 2.2 3.3] {:type "number" :enum [1.1 2.2 3.3]}]
   [[:enum "kikka" "kukka"] {:type "string" :enum ["kikka" "kukka"]}]
   [[:enum :kikka :kukka] {:type "string" :enum [:kikka :kukka]}]
   [[:maybe string?] {:type "string", :x-nullable true}]
   [[:tuple string? string?] {:type "array"
                              :items {}
                              :x-items [{:type "string"}
                                        {:type "string"}]}]
   [[:re "^[a-z]+\\.[a-z]+$"] {:type "string", :pattern "^[a-z]+\\.[a-z]+$"}]
   [[:string {:min 1, :max 4}] {:type "string", :minLength 1, :maxLength 4}]
   [[:int {:min 1, :max 4}] {:type "integer", :format "int64", :minimum 1, :maximum 4}]
   [[:double {:min 1, :max 4}] {:type "number", :format "double" :minimum 1, :maximum 4}]
   [:keyword {:type "string"}]
   [:qualified-keyword {:type "string"}]
   [:symbol {:type "string"}]
   [:qualified-symbol {:type "string"}]
   [:uuid {:type "string", :format "uuid"}]

   [integer? {:type "integer" :format "int32"}]
   #?@(:clj  [[ratio? {:type "number"}]
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
      swagger/SwaggerSchema
      (-accept [_ _ _] {:type "custom"})) {:type "custom"}]
   ;; type-properties
   [malli.core-test/Over6 {:type "integer", :format "int64", :minimum 6}]
   [[malli.core-test/Over6 {:json-schema/example 42}] {:type "integer", :format "int64", :minimum 6, :example 42}]])

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
    (is (= {:type "object"
            :properties {:x1 {:title "x", :type "string"}
                         :x2 {:title "x"}
                         :x3 {:title "x", :type "string", :default "x"}
                         :x4 {:title "x-string", :default "x2"}
                         :x5 {:type "x-string"}}
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

(deftest null-base-test
  (is (thrown-with-msg?
       #?(:clj Exception, :cljs js/Error)
       #":malli\.swagger/non-null-base-needed"
       (swagger/transform [:or :nil :nil])))
  (is (thrown-with-msg?
       #?(:clj Exception, :cljs js/Error)
       #":malli\.swagger/non-null-base-needed"
       (swagger/transform :nil)))
  (is (thrown-with-msg?
       #?(:clj Exception, :cljs js/Error)
       #":malli\.swagger/non-null-base-needed"
       (swagger/transform [:maybe :nil]))))

(deftest util-schemas-test
  (let [registry (merge (m/default-schemas) (mu/schemas))]

    (testing "merge"
      (is (= {:title "merge"
              :type "object"
              :properties {:x {:type "integer", :format "int64", :example 42}
                           :y {:type "integer", :format "int64"}
                           :z {:type "integer", :format "int64"}}
              :required [:x :y :z]}
             (swagger/transform
              [:merge {:title "merge"}
               [:map [:x {:swagger/example 42} int?] [:y int?]]
               [:map [:z int?]]]
              {:registry registry}))))

    (testing "union"
      (is (= {:title "union"
              :type "object"
              :properties {:x {:format "int64"
                               :type "integer"
                               :x-anyOf [{:format "int64"
                                          :type "integer"}
                                         {:type "string"}]}
                           :y {:type "integer", :format "int64"}}
              :required [:x :y]}
             (swagger/transform
              [:union {:title "union"}
               [:map [:x int?] [:y int?]]
               [:map [:x string?]]]
              {:registry registry}))))

    (testing "select-keys"
      (is (= {:title "select-keys"
              :type "object"
              :properties {:x {:type "integer", :format "int64"}}
              :required [:x]}
             (swagger/transform
              [:select-keys {:title "select-keys"}
               [:map [:x int?] [:y int?]]
               [:x]]
              {:registry registry}))))))

(deftest references-test
  (is (= {:$ref "#/definitions/Order"
          :definitions {"Country" {:type "object"
                                   :properties {:name {:type "string"
                                                       :enum [:FI :PO]}
                                                :neighbors {:type "array"
                                                            :items {:$ref "#/definitions/Country"}}}
                                   :required [:name :neighbors]}
                        "Burger" {:type "object"
                                  :properties {:name {:type "string"}
                                               :description {:type "string"}
                                               :origin {:$ref "#/definitions/Country"
                                                        :x-nullable true}
                                               :price {:type "integer"
                                                       :format "int64"
                                                       :minimum 1}}
                                  :required [:name :origin :price]}
                        "OrderLine" {:type "object"
                                     :properties {:burger {:$ref "#/definitions/Burger"}
                                                  :amount {:type "integer"
                                                           :format "int64"}}
                                     :required [:burger :amount]}
                        "Order" {:type "object"
                                 :properties {:lines {:type "array"
                                                      :items {:$ref "#/definitions/OrderLine"}}
                                              :delivery {:type "object"
                                                         :properties {:delivered {:type "boolean"}
                                                                      :address {:type "object"
                                                                                :properties {:street {:type "string"}
                                                                                             :zip {:type "integer"
                                                                                                   :format "int64"}
                                                                                             :country {:$ref "#/definitions/Country"}}
                                                                                :required [:street :zip :country]}}
                                                         :required [:delivered :address]}}
                                 :required [:lines :delivery]}}}
         (swagger/transform
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

(def Request [:map-of :keyword :any])
(def QueryB [:string {:min 10}])
(def Query [:map [:a :int] [:b #'QueryB]])
(def SuccessWorked [:= "worked"])
(def Success [:map [:it #'SuccessWorked]])

(deftest swagger-spec-test
  (testing "generates swagger for ::parameters and ::responses w/ basic schema"
    (is (= {:parameters [{:description ""
                          :in "body"
                          :name "body"
                          :required true
                          :schema {:properties {:foo {:type "string"}}
                                   :required [:foo] :type "object"}}
                         {:description ""
                          :in "query"
                          :name :a
                          :required true
                          :type "string"}
                         {:description ""
                          :in "query"
                          :name :b
                          :required true
                          :type "string"}
                         {:description ""
                          :in "header"
                          :name :c
                          :required true
                          :type "string"}]
            :responses {200 {:description ""
                             :schema {:properties {:bar {:type "string"}}
                                      :required [:bar], :type "object"}}}}
           (swagger/swagger-spec {::swagger/parameters
                                  {:body [:map [:foo :string]]
                                   :query [:map [:a :string] [:b :string]]
                                   :header [:map [:c :string]]}
                                  ::swagger/responses
                                  {200 {:schema [:map [:bar :keyword]]}}}))))
  (testing "generates swagger for ::parameters w/ basic schema + registry"
    (let [registry (merge (m/type-schemas)
                          {::body [:string {:min 1}]})]
      (is (= {:definitions {"malli.swagger-test.body" {:minLength 1, :type "string"}}
              :parameters [{:description ""
                            :in "body"
                            :name "body"
                            :required true
                            :schema {:$ref "#/definitions/malli.swagger-test.body"}}]}
             (swagger/swagger-spec {::swagger/parameters
                                    {:body (m/schema ::body
                                                     {:registry registry})}})))))

  (testing "generates swagger for ::responses w/ basic schema + registry"
    (let [registry (merge (m/base-schemas) (m/type-schemas)
                          {::success [:map-of :keyword :string]
                           ::error [:string {:min 1}]})]
      (is (= {:definitions {"malli.swagger-test.error" {:minLength 1, :type "string"}
                            "malli.swagger-test.success" {:additionalProperties {:type "string"}
                                                          :type "object"}}
              :responses {200 {:description ""
                               :schema {:$ref "#/definitions/malli.swagger-test.success"}}
                          400 {:description ""
                               :schema {:$ref "#/definitions/malli.swagger-test.error"}}}}
             (swagger/swagger-spec {::swagger/responses
                                    {200 {:schema (m/schema ::success
                                                            {:registry registry})}
                                     400 {:schema (m/schema ::error
                                                            {:registry registry})}}})))))

  (testing "generates swagger for ::parameters and ::responses w/ basic schema + registry"
    (let [registry (merge (m/base-schemas) (m/type-schemas) (m/comparator-schemas)
                          {::req-body [:map-of :keyword :any]
                           ::query-b [:string {:min 10}]
                           ::query [:map [:a :int] [:b ::query-b]]
                           ::success-resp [:map [:it [:= "worked"]]]
                           ::error-resp [:string {:min 1}]})]
      (is (= {:definitions {"malli.swagger-test.error-resp" {:minLength 1, :type "string"}
                            "malli.swagger-test.req-body" {:additionalProperties {}, :type "object"}
                            "malli.swagger-test.success-resp" {:properties {:it {:const "worked"}}
                                                               :required [:it]
                                                               :type "object"}}
              :parameters [{:description ""
                            :in "body"
                            :name "body"
                            :required true
                            :schema {:$ref "#/definitions/malli.swagger-test.req-body"}}
                           {:description ""
                            :in "query"
                            :name :a
                            :required true
                            :type "integer"
                            :format "int64"}
                           {:description ""
                            :in "query"
                            :name :b
                            :required true
                            :type "string"
                            :minLength 10}]
              :responses {200 {:description ""
                               :schema {:$ref "#/definitions/malli.swagger-test.success-resp"}}
                          400 {:description ""
                               :schema {:$ref "#/definitions/malli.swagger-test.error-resp"}}}}
             (swagger/swagger-spec {::swagger/parameters
                                    {:body (m/schema ::req-body
                                                     {:registry registry})
                                     :query (m/schema ::query
                                                      {:registry registry})}
                                    ::swagger/responses
                                    {200 {:schema (m/schema ::success-resp
                                                            {:registry registry})}
                                     400 {:schema (m/schema ::error-resp
                                                            {:registry registry})}}})))))

  (testing "no schema in responses ignored"
    (is (= {:responses {200 {:description "" :schema {:type "string"}}
                        500 {:description "fail"}}}
           (swagger/swagger-spec {::swagger/responses
                                  {500 {:description "fail"}
                                   200 {:schema [:string]}}}))))

  (testing "generates swagger for ::parameters and ::responses w/ recursive schema + registry"
    (let [registry (merge (m/base-schemas) (m/type-schemas)
                          (m/comparator-schemas) (m/sequence-schemas)
                          {::a [:or
                                :string
                                [:vector [:ref ::b]]]
                           ::b [:or
                                :keyword
                                [:vector [:ref ::c]]]
                           ::c [:or
                                :symbol
                                [:vector [:ref ::a]]]
                           ::req-body [:map [:a ::a]]
                           ::success-resp [:map-of :keyword :string]
                           ::error-resp :string})]
      (testing "not an infinite schema"
        (is (not (m/validate (m/schema [:ref ::a] {:registry registry}) nil)))
        (is (not (m/validate (m/schema [:ref ::b] {:registry registry}) nil)))
        (is (not (m/validate (m/schema [:ref ::c] {:registry registry}) nil))))
      (is (= {:definitions {"malli.swagger-test.a" {:type "string"
                                                    :x-anyOf [{:type "string"}
                                                              {:type "array"
                                                               :items {:$ref "#/definitions/malli.swagger-test.b"}}]}
                            "malli.swagger-test.b" {:type "string"
                                                    :x-anyOf [{:type "string"}
                                                              {:type "array"
                                                               :items {:$ref "#/definitions/malli.swagger-test.c"}}]}
                            "malli.swagger-test.c" {:type "string"
                                                    :x-anyOf [{:type "string"}
                                                              {:type "array"
                                                               :items {:$ref "#/definitions/malli.swagger-test.a"}}]}
                            "malli.swagger-test.error-resp" {:type "string"}
                            "malli.swagger-test.req-body" {:properties {:a {:$ref "#/definitions/malli.swagger-test.a"}}
                                                           :required [:a]
                                                           :type "object"}
                            "malli.swagger-test.success-resp" {:additionalProperties {:type "string"}
                                                               :type "object"}}
              :parameters [{:description ""
                            :in "body"
                            :name "body"
                            :required true
                            :schema {:$ref "#/definitions/malli.swagger-test.req-body"}}]
              :responses {200 {:description ""
                               :schema {:$ref "#/definitions/malli.swagger-test.success-resp"}}
                          400 {:description ""
                               :schema {:$ref "#/definitions/malli.swagger-test.error-resp"}}}}
             (swagger/swagger-spec {::swagger/parameters
                                    {:body (m/schema ::req-body
                                                     {:registry registry})}
                                    ::swagger/responses
                                    {200 {:schema (m/schema ::success-resp
                                                            {:registry registry})}
                                     400 {:schema (m/schema ::error-resp
                                                            {:registry registry})}}})))))

  (testing "generates swagger for ::parameters and ::responses w/ var schema"
    (is (= {:definitions {"malli.swagger-test.Request" {:additionalProperties {}, :type "object"},
                          "malli.swagger-test.Success" {:properties {:it {:$ref "#/definitions/malli.swagger-test.SuccessWorked"}},
                                                        :required [:it]
                                                        :type "object"}
                          "malli.swagger-test.SuccessWorked" {:const "worked"}}
            :parameters [{:description ""
                          :in "body"
                          :name "body"
                          :required true
                          :schema {:$ref "#/definitions/malli.swagger-test.Request"}}]
            :responses {200 {:description ""
                             :schema {:$ref "#/definitions/malli.swagger-test.Success"}}}}
           (swagger/swagger-spec {::swagger/parameters {:body #'Request}
                                  ::swagger/responses {200 {:schema #'Success}}}))))
  (testing "::parameters :query w/ var schema"
    ;; NB! all refs get inlined!
    (is (= {:parameters [{:description ""
                          :in "query"
                          :name :a
                          :required true
                          :format "int64"
                          :type "integer"}
                         {:description ""
                          :in "query"
                          :name :b
                          :required true
                          :type "string"
                          :minLength 10}]}
           (swagger/swagger-spec {::swagger/parameters {:query #'Query}})))))

(deftest request-parameter-definition-regression-test
  ;; For issue #1002
  (testing "collects :definitions for all parameters"
    (let [registry (merge (m/base-schemas) (m/type-schemas) (m/comparator-schemas)
                          {::req-body [:map-of :keyword :any]})
          expected {:definitions {"malli.swagger-test.req-body" {:additionalProperties {}, :type "object"}}
                    :parameters [{:description ""
                                  :in "body"
                                  :name "body"
                                  :required true
                                  :schema {:$ref "#/definitions/malli.swagger-test.req-body"}}
                                 {:description ""
                                  :in "header"
                                  :name :h
                                  :required true
                                  :type "string"}
                                 {:description ""
                                  :in "query"
                                  :name :q
                                  :required true
                                  :type "string"}]}
          fix #(update % :parameters (partial sort-by :in))]
      (is (= expected
             (fix
              (swagger/swagger-spec {::swagger/parameters
                                     {:body (m/schema ::req-body {:registry registry})
                                      :header [:map [:h :string]]
                                      :query [:map [:q :string]]}}))))
      ;; bug #1002 was sensitive to the order of the ::swagger/parameters map
      (is (= expected
             (fix
              (swagger/swagger-spec {::swagger/parameters
                                     {:header [:map [:h :string]]
                                      :query [:map [:q :string]]
                                      :body (m/schema ::req-body {:registry registry})}})))))))
