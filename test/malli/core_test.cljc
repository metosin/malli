(ns malli.core-test
  (:require [clojure.string :as str]
            [clojure.test :refer [are deftest is testing]]
            [clojure.test.check.generators :as gen]
            [clojure.walk :as walk]
            [malli.core :as m]
            [malli.edn :as edn]
            [malli.generator :as mg]
            [malli.error :as me]
            [malli.impl.util :as miu]
            [malli.registry :as mr]
            [malli.transform :as mt]
            [malli.util :as mu]
            #?(:clj [malli.test-macros :refer [when-env]]))
  #?(:clj  (:import (clojure.lang IFn PersistentArrayMap PersistentHashMap))
     :cljs (:require-macros [malli.test-macros :refer [when-env]])))

(defn with-schema-forms [result]
  (some-> result
          (update :schema m/form)
          (update :errors (partial map (fn [error]
                                         (-> error
                                             (update :schema m/form)
                                             (update :type (fnil identity nil))
                                             (update :message (fnil identity nil))
                                             (dissoc :check)))))))

(defn as-data [x] (walk/prewalk (fn [x] (cond-> x (m/schema? x) (m/form))) x))

(defn results= [& results]
  (apply = (map with-schema-forms results)))

(defn schema= [& entries]
  (apply = (map (partial walk/postwalk (fn [x] (if (m/schema? x) (m/form x) x))) entries)))

(defn form= [& entries]
  (apply = (map m/form entries)))

(defn over-the-wire [?schema]
  (-> ?schema (edn/write-string) (edn/read-string)))

(deftest keyword->string
  (is (= "abba" (m/-keyword->string :abba)))
  (is (= "jabba/abba" (m/-keyword->string :jabba/abba)))
  (is (= "abba" (m/-keyword->string "abba"))))

(deftest parse-entries-test
  (let [entry-parser (m/-create-entry-parser
                      [[:x int?]
                       ::x
                       "x"
                       [::y {:optional true}]
                       [:y {:optional true, :title "boolean"} boolean?]]
                      {:naked-keys true}
                      {:registry (merge (m/default-schemas) {::x int?, "x" int?, ::y int?})})]
    (testing "forms"
      (is (= [[:x 'int?]
              ::x
              "x"
              [::y {:optional true}]
              [:y {:optional true, :title "boolean"} 'boolean?]]
             (m/-entry-forms entry-parser))))
    (testing "entries"
      (is (schema= [[:x [::m/val 'int?]]
                    [::x [::m/val ::x]]
                    ["x" [::m/val "x"]]
                    [::y [::m/val {:optional true} ::y]]
                    [:y [::m/val {:optional true :title "boolean"} 'boolean?]]]
                   (m/-entry-entries entry-parser))))
    (testing "children"
      (is (= [[:x nil 'int?]
              [::x nil ::x]
              ["x" nil "x"]
              [::y {:optional true} ::y]
              [:y {:optional true, :title "boolean"} 'boolean?]]
             (map #(update % 2 m/form) (m/-entry-children entry-parser))))))
  (testing "invalid entries fail"
    (is (thrown? #?(:clj Exception, :cljs js/Error)
                 (m/-create-entry-parser
                  [[:x]
                   [:y boolean?]] {:naked-keys true} nil)))
    (is (thrown? #?(:clj Exception, :cljs js/Error)
                 (m/-create-entry-parser
                  [[:x boolean?]
                   [:y]] {:naked-keys true} nil)))
    (is (thrown? #?(:clj Exception, :cljs js/Error)
                 (m/-create-entry-parser
                  [[:x boolean?]
                   [:y]
                   [:z boolean?]] {:naked-keys true} nil)))
    (is (thrown? #?(:clj Exception, :cljs js/Error)
                 (m/-create-entry-parser
                  [[:x boolean?]
                   [:y [:map
                        [:x]
                        [:y :int]]]] {:naked-keys true} nil))))
  (testing "duplicate keys"
    (is (thrown? #?(:clj Exception, :cljs js/Error)
                 (m/-create-entry-parser
                  [[:x int?]
                   [:x boolean?]] {:naked-keys true} nil))))
  (testing "naked keys fails when not supported"
    (is (thrown? #?(:clj Exception, :cljs js/Error)
                 (m/-create-entry-parser
                  [::x] nil nil)))))

;;
(deftest eval-test
  (testing "with defaults"
    (is (= 2 ((m/eval inc) 1)))
    (is (= 2 ((m/eval 'inc) 1)))
    (is (= 2 ((m/eval '#(inc %)) 1)))
    (is (= 2 ((m/eval '#(inc %1)) 1)))
    (is (= 2 ((m/eval '(fn [x] (inc x))) 1)))
    (is (= 2 ((m/eval "(fn [x] (inc x))") 1)))
    (is (= {:district 9} (m/eval "(m/properties [int? {:district 9}])")))
    (is (= :maybe (m/eval "(m/type [:maybe int?])")))
    (is (= ['int? 'string?] (map m/form (m/eval "(m/children [:or {:some \"props\"} int? string?])"))))
    (is (schema= [[:x [::m/val 'int?]] [:y [::m/val 'string?]]] (m/eval "(m/entries [:map [:x int?] [:y string?]])")))
    (is (schema= [[:x nil 'int?] [:y nil 'string?]] (m/eval "(m/children [:map [:x int?] [:y string?]])"))))
  #?(:bb nil
     :default
     (when-env
      "TEST_SCI"
      (testing "with options"
        (testing "disabling sci"
          (is (= 2 ((m/eval inc {::m/disable-sci true}) 1)))
          (is (thrown? #?(:clj Exception, :cljs js/Error) ((m/eval 'inc {::m/disable-sci true}) 1))))
        (testing "custom bindings"
          (let [f '(fn [schema] (m/form schema))]
            (is (thrown? #?(:clj Exception, :cljs js/Error) ((m/eval f) :string)))
            (is (= :string ((m/eval f {::m/sci-options {:namespaces {'malli.core {'form m/form}}}}) :string)))))))))

(deftest into-schema-test
  (is (form= [:map {:closed true} [:x int?]]
             (m/into-schema :map {:closed true} [[:x int?]]))))

(deftest schema-walker-test
  (testing "no-op schema-walker"
    (is (form= [:map {:closed true} [:x int?]]
               (m/walk [:map {:closed true} [:x int?]] (m/schema-walker identity))))
    (is (form= [:map {:registry {::age [:and int? [:> 18]]}} [:age ::age]]
               (m/walk [:map {:registry {::age [:and int? [:> 18]]}} [:age ::age]]
                       (m/schema-walker identity))))
    (testing "doesn't affect deref behaviour"
      (let [schema [:schema {:registry {"Foo" :int}} "Foo"]
            walked (m/walk [:schema {:registry {"Foo" :int}} "Foo"]
                           (m/schema-walker identity))]
        (is (form= schema walked))
        (is (form= (-> schema m/deref) (-> walked m/deref)))
        (is (form= (-> schema m/deref m/deref) (-> walked m/deref m/deref)))))))

(defrecord SomeRecord [])

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
      (is (false? (m/validate schema [1])))

      (is (nil? (m/explain schema 1)))
      (is (results= {:schema schema
                     :value "1"
                     :errors [{:path [], :in [], :schema schema, :value "1"}]}
                    {:schema schema
                     :value "1"
                     :errors [(miu/-error [] [] schema "1")]}
                    (m/explain schema "1")))

      (is (= 1 (m/parse schema 1)))
      (is (= ::m/invalid (m/parse schema "1")))
      (is (= 1 (m/unparse schema 1)))
      (is (= ::m/invalid (m/unparse schema "1")))

      (is (= 1 (m/decode schema "1" mt/string-transformer)))
      (is (= "1" (m/decode schema "1" mt/json-transformer)))

      (is (= "olipa_kerran_avaruus"
             (m/decode
              [string? {:decode/string '{:enter #(str "olipa_" %), :leave #(str % "_avaruus")}}]
              "kerran" mt/string-transformer)))

      (when-env
       "TEST_SCI"
       (testing "sci not available"
         (let [schema (m/schema
                       [string? {:decode/string '{:enter #(str "olipa_" %), :leave #(str % "_avaruus")}}]
                       {::m/disable-sci true})]

           (is (thrown-with-msg?
                #?(:clj Exception, :cljs js/Error)
                #":malli.core/sci-not-available"
                (m/decoder schema mt/string-transformer)))

           (is (thrown-with-msg?
                #?(:clj Exception, :cljs js/Error)
                #":malli.core/sci-not-available"
                (m/decoder
                 [string? {:decode/string '{:enter #(str "olipa_" %), :leave #(str % "_avaruus")}}]
                 {::m/disable-sci true} mt/string-transformer)))

           (testing "direct options win"
             (is (m/decoder schema {::m/disable-sci false} mt/string-transformer))))))

      (is (true? (m/validate (over-the-wire schema) 1)))

      (testing "ast"
        (is (= {:type 'int?} (m/ast schema)))
        (is (true? (m/validate (m/from-ast (m/ast schema)) 1))))

      (is (= 'int? (m/form schema)))))

  (testing "empty? does not throw in validation"
    (is (thrown? #?(:clj Exception, :cljs js/Error) (empty? 1)))
    (is (false? (m/validate empty? 1))))

  (testing "composite schemas"
    (let [schema (m/schema [:and int? [:or pos-int? neg-int?]])
          schema* (m/schema [:and int? [:orn [:pos pos-int?] [:neg neg-int?]]])]

      (doseq [schema [schema schema*]]
        (is (true? (m/validate schema 1)))
        (is (true? (m/validate schema -1)))
        (is (false? (m/validate schema 0)))
        (is (false? (m/validate schema "1")))
        (is (false? (m/validate schema [1]))))

      (is (nil? (m/explain schema 1)))
      (is (results= {:schema schema,
                     :value 0,
                     :errors [{:path [1 0], :in [], :schema pos-int?, :value 0}
                              {:path [1 1], :in [], :schema neg-int?, :value 0}]}
                    (m/explain schema 0)))

      (is (nil? (m/explain schema* 1)))
      (is (results= {:schema schema*,
                     :value 0,
                     :errors [{:path [1 :pos], :in [], :schema pos-int?, :value 0}
                              {:path [1 :neg], :in [], :schema neg-int?, :value 0}]}
                    (m/explain schema* 0)))

      (is (= 1 (m/parse schema 1)))
      (is (= ::m/invalid (m/parse schema 0)))
      (is (= 1 (m/unparse schema 1)))
      (is (= ::m/invalid (m/unparse schema 0)))

      (is (= (m/tag :pos 1) (m/parse schema* 1)))
      (is (= ::m/invalid (m/parse schema* 0)))
      (is (= 1 (m/unparse schema* (m/tag :pos 1))))
      (is (= ::m/invalid (m/unparse schema* [:pos 1])))
      (is (= ::m/invalid (m/unparse schema* (m/tag :pos 0))))

      (doseq [schema [schema schema*]]
        (testing (m/form schema)
          (is (= 1 (m/decode schema "1" mt/string-transformer)))
          (is (= "1" (m/decode schema "1" mt/json-transformer)))))

      (is (= "olipa_kerran_avaruus"
             (m/decode
              [:and {:decode/string '{:enter #(str "olipa_" %), :leave #(str % "_avaruus")}} string?]
              "kerran" mt/string-transformer)))
      (is (= "3_1_kerran_2_4"
             (m/decode
              [:and
               [:string {:decode/string '{:enter #(str "1_" %), :leave #(str % "_2")}}]
               [:string {:decode/string '{:enter #(str "3_" %), :leave #(str % "_4")}}]]
              "kerran" mt/string-transformer)))
      (is (= "1_kerran_2"
             (m/decode
              [:or
               [:string {:decode/string '{:enter #(str "1_" %), :leave #(str % "_2")}}]
               [:string {:decode/string '{:enter #(str "3_" %), :leave #(str % "_4")}}]]
              "kerran" mt/string-transformer)))
      (is (= "3_kerran_4"
             (m/decode
              [:or
               :map
               [:string {:decode/string '{:enter #(str "3_" %), :leave #(str % "_4")}}]]
              "kerran" mt/string-transformer)))

      (doseq [schema [schema schema*]]
        (is (true? (m/validate (over-the-wire schema) 1))))

      (testing "ast"
        (is (= {:type :and
                :children [{:type 'int?}
                           {:type :or
                            :children [{:type 'pos-int?}
                                       {:type 'neg-int?}]}]} (m/ast schema)))
        (is (= {:type :and,
                :children [{:type 'int?}
                           {:type :orn
                            :keys {:pos {:order 0
                                         :value {:type 'pos-int?}}
                                   :neg {:order 1
                                         :value {:type 'neg-int?}}}}]} (m/ast schema*)))
        (is (true? (m/validate (m/from-ast (m/ast schema)) -1)))
        (is (true? (m/validate (m/from-ast (m/ast schema)) 1))))

      (is (= [:and 'int? [:or 'pos-int? 'neg-int?]] (m/form schema)))
      (is (= [:and 'int? [:orn [:pos 'pos-int?] [:neg 'neg-int?]]] (m/form schema*))))

    (testing "transforming :or"
      (let [math (mt/transformer {:name :math})
            math-string [:string {:decode/math (partial str "math_")}]
            math-kw-string [:and math-string [:any {:decode/math keyword}]]
            bono-string [:string {:decode/math (partial str "such_")}]]

        (testing "first successful branch is selected"
          (is (= "math_1"
                 (m/decode math-string 1 math)
                 (m/decode [:and math-string] 1 math)
                 (m/decode [:or math-string] 1 math)
                 (m/decode [:or
                            math-kw-string
                            math-string
                            bono-string] 1 math)
                 (m/decode [:orn ["string" math-string]] 1 math)
                 (m/decode [:orn
                            ["kw-math" math-kw-string]
                            ["math" math-string]
                            ["bono" bono-string]] 1 math))))

        (testing "first branch value is selected as fallback, even if invalid"
          (is (= :math_1
                 (m/decode [:or
                            math-kw-string
                            :string] 1 math)
                 (m/decode [:orn
                            ["kw-math" math-kw-string]
                            ["string" :string]] 1 math))))

        (testing "first branch nil can be selected as a fallback"
          (is (= nil (m/decode
                      [:or
                       [:keyword {:decode/math (constantly nil)}]
                       :keyword]
                      "kikka"
                      (mt/transformer {:name :math}))))))

      (testing "top-level transformations are retained"
        (doseq [schema [[:or {:decode/string {:enter (fn [m] (update m :enter #(or % true)))
                                              :leave (fn [m] (update m :leave #(or % true)))}}
                         [:map
                          [:x keyword?]
                          [:enter boolean?]]
                         [:map
                          [:y keyword?]
                          [:enter boolean?]]]
                        [:orn {:decode/string {:enter (fn [m] (update m :enter #(or % true)))
                                               :leave (fn [m] (update m :leave #(or % true)))}}
                         [:äxy [:map
                                [:x keyword?]
                                [:enter boolean?]]]
                         [:yxy [:map
                                [:y keyword?]
                                [:enter boolean?]]]]]]
          (are [input result]
            (= (m/decode (mu/closed-schema schema) input (mt/string-transformer)) result)

            {:x "true"} {:x :true, :enter true, :leave true} ;; first
            {:x "true", :enter "invalid"} {:x :true, :enter "invalid", :leave true} ;; first (fallback)
            {:y "true"} {:y :true, :enter true, :leave true} ;; second
            {:y "true", :leave "invalid"} {:y "true", :enter true, :leave "invalid"} ;; no match
            {:x "true", :y "true"} {:x :true, :y "true", :enter true, :leave true})))) ;; first (fallback))

    (testing "explain with branches"
      (let [schema [:and pos-int? neg-int?]]
        (is (results= {:schema schema,
                       :value -1,
                       :errors [{:path [0], :in [], :schema pos-int?, :value -1}]}
                      (m/explain schema -1))))
      (let [schema [:and pos-int? neg-int?]]
        (is (results= {:schema schema,
                       :value 1,
                       :errors [{:path [1], :in [], :schema neg-int?, :value 1}]}
                      (m/explain schema 1))))))

  (testing ":not schema"
    (testing ":fn validation"
      (let [schema (m/schema [:not [:fn #(= % 1)]])]
        (is (true? (m/validate schema 2)))
        (is (nil? (m/explain schema 2)))
        (is (= 2 (m/parse schema 2)))
        (is (= "2" (m/decode schema "2" mt/string-transformer)))
        (is (= "2" (m/decode schema "2" mt/json-transformer)))

        (is (true? (m/validate schema "string")))
        (is (nil? (m/explain schema "string")))
        (is (= "string" (m/parse schema "string")))

        (is (true? (m/validate schema :keyword)))
        (is (nil? (m/explain schema :keyword)))
        (is (= :keyword (m/parse schema :keyword)))

        (is (false? (m/validate schema 1)))
        (is (results= {:schema schema
                       :value 1
                       :errors [{:path [0]
                                 :in []
                                 :schema schema
                                 :value 1}]}
                      (m/explain schema 1)))
        (is (= ::m/invalid (m/parse schema 1)))

        (is (m/walk schema (m/schema-walker identity)))))

    (testing "function validation"
      (let [schema1 (m/schema [:not pos?])
            schema2 (m/schema [:not empty?])]
        (is (true? (m/validate schema1 -1)))
        (is (nil? (m/explain schema1 -1)))
        (is (= -1 (m/parse schema1 -1)))
        (is (= "-1" (m/decode schema1 "-1" mt/string-transformer)))
        (is (= "-1" (m/decode schema1 "-1" mt/json-transformer)))

        (is (true? (m/validate schema1 0)))
        (is (nil? (m/explain schema1 0)))
        (is (= 0 (m/parse schema1 0)))

        (is (true? (m/validate schema2 "string")))
        (is (nil? (m/explain schema2 "string")))
        (is (= "string" (m/parse schema2 "string")))
        (is (= "string" (m/decode schema2 "string" mt/string-transformer)))
        (is (= "" (m/decode schema2 "" mt/string-transformer)))
        (is (= "string" (m/decode schema2 "string" mt/json-transformer)))
        (is (= "" (m/decode schema2 "" mt/json-transformer)))

        (is (false? (m/validate schema1 1)))
        (is (results= {:schema schema1
                       :value 1
                       :errors [{:path [0]
                                 :in []
                                 :schema schema1
                                 :value 1}]}
                      (m/explain schema1 1)))
        (is (= ::m/invalid (m/parse schema1 1)))

        (is (false? (m/validate schema2 "")))
        (is (results= {:schema schema2
                       :value ""
                       :errors [{:path [0]
                                 :in []
                                 :schema schema2
                                 :value ""}]}
                      (m/explain schema2 "")))
        (is (= ::m/invalid (m/parse schema2 "")))

        (is (m/walk schema1 (m/schema-walker identity)))
        (is (m/walk schema2 (m/schema-walker identity)))))

    (testing "as a part of a complex schema"
      (let [schema (m/schema [:map
                              [:a int?]
                              [:b [:not empty?]]
                              [:c [:map [:d [:not [:fn #(= "test" %)]]]]]
                              [:e [:not [:< 10]]]])]
        (is (m/validate schema {:a 1 :b "Test" :c {:d "Malli"} :e 10}))
        (is (results= {:errors [{:in [:b]
                                 :message nil
                                 :path [:b 0]
                                 :schema (mu/get-in schema [:b])
                                 :type nil
                                 :value ""}]
                       :schema schema
                       :value {:a 1 :b "" :c {:d "Malli"} :e 10}}
                      (m/explain schema {:a 1 :b "" :c {:d "Malli"} :e 10})))
        (is (results= {:errors [{:in [:c :d]
                                 :message nil
                                 :path [:c :d 0]
                                 :schema (mu/get-in schema [:c :d])
                                 :type nil
                                 :value "test"}]
                       :schema schema
                       :value {:a 1 :b "Test" :c {:d "test"} :e 10}}
                      (m/explain schema {:a 1 :b "Test" :c {:d "test"} :e 10})))
        (is (results= {:errors [{:in [:e]
                                 :message nil
                                 :path [:e 0]
                                 :schema (mu/get-in schema [:e])
                                 :type nil
                                 :value 9}]
                       :schema schema
                       :value {:a 1 :b "Test" :c {:d "Malli"} :e 9}}
                      (m/explain schema {:a 1 :b "Test" :c {:d "Malli"} :e 9}))))))

  (testing "comparator schemas"
    (let [schema (m/schema [:> 0])]

      (is (true? (m/validate schema 1)))
      (is (false? (m/validate schema 0)))
      (is (false? (m/validate schema "abba")))

      (is (nil? (m/explain schema 1)))
      (is (results= {:schema [:> 0], :value 0, :errors [{:path [], :in [], :schema [:> 0], :value 0}]}
                    (m/explain schema 0)))

      (is (= 1 (m/parse schema 1)))
      (is (= ::m/invalid (m/parse schema 0)))
      (is (= 1 (m/unparse schema 1)))
      (is (= ::m/invalid (m/unparse schema 0)))

      (is (= 1 (m/decode schema "1" mt/string-transformer)))
      (is (= "1" (m/decode schema "1" mt/json-transformer)))

      (is (= 4 (m/decode
                [:> {:decode/string '{:enter inc, :leave (partial * 2)}} 0]
                1 mt/string-transformer)))

      (is (true? (m/validate (over-the-wire schema) 1)))

      (testing "ast"
        (is (= {:type :>, :value 0}
               (m/ast schema)))
        (is (true? (m/validate (m/from-ast (m/ast schema)) 1))))

      (is (= [:> 0] (m/form schema)))))

  (testing "enum schemas"
    (let [schema (m/schema [:enum 1 2])]

      (is (true? (m/validate schema 1)))
      (is (false? (m/validate schema 0)))
      (is (false? (m/validate schema "abba")))

      (is (nil? (m/explain schema 1)))
      (is (results= {:schema [:enum 1 2], :value 0, :errors [{:path [], :in [], :schema [:enum 1 2], :value 0}]}
                    (m/explain [:enum 1 2] 0)))

      (is (= 1 (m/parse schema 1)))
      (is (= ::m/invalid (m/parse schema 0)))
      (is (= 1 (m/unparse schema 1)))
      (is (= ::m/invalid (m/unparse schema 0)))

      ;; TODO: infer type from :enum
      #_(is (= 1 (m/decode schema "1" mt/string-transformer)))
      #_(is (= "1" (m/decode schema "1" mt/json-transformer)))

      (testing "nil enums without properties require empty properties"
        (let [schema [:enum nil nil]]
          (testing (pr-str schema)
            (is (= nil (m/properties schema)))
            (is (= [nil] (m/children schema)))
            (is (= schema (m/form schema)))
            (is (= schema (-> schema m/form m/schema m/form))))))

      (testing "nil nums support properties"
        (let [schema [:enum {:foo :bar} nil]]
          (is (= {:foo :bar} (m/properties schema)))
          (is (= [nil] (m/children schema)))
          (is (= schema (m/form schema)))
          (is (= schema (-> schema m/form m/schema m/form)))))

      (testing "map enums without properties require empty properties"
        (doseq [schema [[:enum nil {:a 1} {:b 2}]
                        [:enum {} {:a 1} {:b 2}]]]
          (testing (pr-str schema)
            (is (= nil (m/properties schema)))
            (is (= [{:a 1} {:b 2}] (m/children schema)))
            (is (= [:enum nil {:a 1} {:b 2}] (m/form schema)))
            (is (= [:enum nil {:a 1} {:b 2}] (-> schema m/form m/schema m/form))))))

      (testing "map enums support properties"
        (let [schema [:enum {:foo :bar} {:a 1} {:b 2}]]
          (is (= {:foo :bar} (m/properties schema)))
          (is (= [{:a 1} {:b 2}] (m/children schema)))
          (is (= schema (m/form schema)))
          (is (= schema (-> schema m/form m/schema m/form)))))

      (is (true? (m/validate (over-the-wire schema) 1)))

      (testing "ast"
        (is (= {:type :enum, :values [1 2]}
               (m/ast schema)))
        (is (= {:type :enum :properties {:optional true} :values [1 2]}
               (m/ast (mu/update-properties schema assoc :optional true))))
        (is (true? (m/validate (m/from-ast (m/ast schema)) 1))))

      (is (= [:enum 1 2] (m/form schema)))))

  (testing "maybe schemas"
    (let [schema (m/schema [:maybe int?])]

      (is (true? (m/validate schema 1)))
      (is (true? (m/validate schema nil)))
      (is (false? (m/validate schema "abba")))

      (is (nil? (m/explain schema 1)))
      (is (results= {:schema [:maybe int?], :value "abba", :errors [{:path [0], :in [], :schema int?, :value "abba"}]}
                    (m/explain [:maybe int?] "abba")))

      (is (= 1 (m/parse schema 1)))
      (is (nil? (m/parse schema nil)))
      (is (= ::m/invalid (m/parse schema "abba")))
      (is (= 1 (m/unparse schema 1)))
      (is (nil? (m/unparse schema nil)))
      (is (= ::m/invalid (m/unparse schema "abba")))

      (is (= 1 (m/decode schema "1" mt/string-transformer)))
      (is (= "1" (m/decode schema "1" mt/json-transformer)))

      (is (= 4 (m/decode
                [:maybe {:decode/string '{:enter inc, :leave (partial * 2)}} int?]
                1 mt/string-transformer)))

      (is (true? (m/validate (over-the-wire schema) 1)))

      (testing "ast"
        (is (= {:type :maybe, :child {:type 'int?}}
               (m/ast schema)))
        (is (true? (m/validate (m/from-ast (m/ast schema)) 1)))
        (is (true? (m/validate (m/from-ast (m/ast schema)) nil))))

      (is (= [:maybe 'int?] (m/form schema)))))

  (testing "ref schemas"

    (let [schema [:ref {:registry {::referred [:map [:foo :int]]}} ::referred]]
      (is (nil? (m/explain schema {:foo 2})))
      (testing "explain path"
        (let [exp (m/explain schema {:foo "2"})]
          (is (results= {:value {:foo "2"}
                         :schema schema
                         :errors [{:in [:foo]
                                   :path [0 0 :foo]
                                   :schema :int
                                   :value "2"}]}
                        exp))
          (is (form= :int (mu/get-in schema (-> exp :errors first :path)))))))

    (testing "invalid refs fail"
      (is (thrown?
           #?(:clj Exception, :cljs js/Error)
           (m/schema [:ref int?]))))

    (testing "recursion"
      (let [ConsCell (m/schema [:schema {:registry {::cons [:maybe [:tuple int? [:ref ::cons]]]}} ::cons])]

        (is (true? (m/validate ConsCell [1 nil])))
        (is (true? (m/validate ConsCell [1 [2 nil]])))
        (is (false? (m/validate ConsCell [1 [2]])))

        (is (nil? (m/explain ConsCell [1 [2 nil]])))
        (is (results= {:schema ConsCell
                       :value [1 [2]]
                       :errors [{:in [1]
                                 :path [0 0 0 1 0 0 0]
                                 :schema (mu/get-in ConsCell [0 0 0])
                                 :type :malli.core/tuple-size
                                 :value [2]}]}
                      (m/explain ConsCell [1 [2]])))

        (is (= [1 nil] (m/parse ConsCell [1 nil])))
        (is (= [1 [2 nil]] (m/parse ConsCell [1 [2 nil]])))
        (is (= ::m/invalid (m/parse ConsCell [1 [2]])))
        (is (= [1 nil] (m/unparse ConsCell [1 nil])))
        (is (= [1 [2 nil]] (m/unparse ConsCell [1 [2 nil]])))
        (is (= ::m/invalid (m/unparse ConsCell [1 [2]])))

        (is (= [1 ["two" [3 nil]]] (m/decode ConsCell ["1" ["two" ["3" nil]]] mt/string-transformer)))
        (is (= ["1" ["two" ["3" nil]]] (m/decode ConsCell ["1" ["two" ["3" nil]]] mt/json-transformer)))
        (is (= [1 [2 [3 [4 ::end]]]]
               (m/decode
                [:schema {:registry {::cons [:maybe [:tuple int? [:ref {:decode/string (fnil identity ::end)} ::cons]]]}}
                 ::cons]
                [1 [2 [3 [4 nil]]]]
                mt/string-transformer)))

        (is (true? (m/validate (over-the-wire ConsCell) [1 [2 nil]])))

        (testing "ast"
          (is (= {:type :schema
                  :child {:type :malli.core/schema
                          :value ::cons}
                  :registry {::cons {:type :maybe
                                     :child {:type :tuple
                                             :children [{:type 'int?}
                                                        {:type :ref
                                                         :value ::cons}]}}}}
                 (m/ast ConsCell)))
          (is (true? (m/validate (m/from-ast (m/ast ConsCell)) [1 [2 nil]]))))

        (is (= [:schema {:registry {::cons [:maybe [:tuple 'int? [:ref ::cons]]]}}
                ::cons]
               (m/form ConsCell)))))

    (testing "mutual recursion"
      (let [registry {::ping [:maybe [:tuple [:= "ping"] [:ref ::pong]]]
                      ::pong [:maybe [:tuple [:= "pong"] [:ref ::ping]]]}]

        (is (true? (m/validate
                    ::ping
                    ["ping" ["pong" nil]]
                    {:registry (mr/composite-registry (m/default-schemas) registry)})))

        (is (true? (m/validate
                    [:schema {:registry registry}
                     ::ping]
                    ["ping" ["pong" nil]])))))

    (testing "fails with missing :ref"
      (is (thrown?
           #?(:clj Exception, :cljs js/Error)
           (m/validate
            [:schema
             {:registry {::ping [:maybe {:id ::pong} [:tuple [:= "ping"] [:ref ::invalid]]]
                         ::pong [:maybe {:id ::ping} [:tuple [:= "pong"] [:ref ::ping]]]}}
             ::ping]
            ["ping" ["ping" nil]])))))

  (testing "::m/schema ast"
    (is (= :int
           (-> [::m/schema :int]
               (m/schema)
               (m/ast)
               (m/from-ast)
               (m/form)))))

  (testing "schema"
    (is (form= :int
               (as-> [:schema :int] $
                 (m/explain $ "1")
                 (let [{:keys [schema] [{:keys [path]}] :errors} $]
                   (mu/get-in schema path))))))

  (testing "malli.core/schema"
    (is (= [::m/schema {:registry {::cons [:maybe [:tuple 'int? [:ref ::cons]]]}}
            ::cons]
           (m/form [::m/schema {:registry {::cons [:maybe [:tuple int? [:ref ::cons]]]}}
                    ::cons])))
    (is (= [:schema {:registry {::cons [:maybe [:tuple 'int? [:ref ::cons]]]}}
            ::cons]
           (m/form [:schema {:registry {::cons [:maybe [:tuple int? [:ref ::cons]]]}}
                    [::m/schema ::cons]])))

    (is (= "$kikka"
           (m/decode
            [:schema {:decode/custom (partial str "$")} string?] "kikka"
            (mt/transformer {:name :custom})))))

  (testing "schema schemas"
    (let [schema [:and
                  {:registry {::a ::b
                              ::b ::c
                              ::c [:schema pos-int?]}}
                  [:and ::a ::b ::c]]]

      (is (true? (m/validate schema 1)))
      (is (false? (m/validate schema -1)))

      (is (nil? (m/explain schema 1)))
      (is (results= {:schema schema
                     :value -1
                     :errors [{:path [0 0 0 0 0 0] :in [] :schema pos-int?, :value -1}
                              {:path [0 1 0 0 0], :in [], :schema pos-int?, :value -1}
                              {:path [0 2 0 0], :in [], :schema pos-int?, :value -1}]}

                    (m/explain schema -1)))

      (is (= 1 (m/parse schema 1)))
      (is (= ::m/invalid (m/parse schema -1)))
      (is (= 1 (m/unparse schema 1)))
      (is (= ::m/invalid (m/unparse schema -1)))

      (is (= 1 (m/decode schema "1" mt/string-transformer)))
      (is (= "1" (m/decode schema "1" mt/json-transformer)))

      (is (= "$kikka"
             (m/decode
              [:schema {:decode/custom (partial str "$")} string?] "kikka"
              (mt/transformer {:name :custom}))))

      (testing "deref"
        (is (mu/equals (m/schema int?) (m/deref int?)))
        (is (mu/equals (m/schema int?) (m/deref [:schema int?])))
        (is (mu/equals (m/schema [:schema [:schema int?]]) (m/deref [:schema [:schema [:schema int?]]]))))

      (testing "deref-all"
        (is (mu/equals (m/schema int?) (m/deref-all int?)))
        (is (mu/equals (m/schema int?) (m/deref-all [:schema int?])))
        (is (mu/equals (m/schema int?) (m/deref-all [:schema [:schema [:schema int?]]]))))

      (is (true? (m/validate (over-the-wire schema) 1)))

      (testing "ast"
        (is (= {:type :and,
                :children [{:type :and
                            :children [{:type ::m/schema, :value ::a}
                                       {:type ::m/schema, :value ::b}
                                       {:type ::m/schema, :value ::c}]}]
                :registry {::a {:type ::m/schema, :value ::b}
                           ::b {:type ::m/schema, :value ::c}
                           ::c {:type :schema, :child {:type 'pos-int?}}}}
               (m/ast schema)))
        (is (true? (m/validate (m/from-ast (m/ast schema)) 1))))

      (is (= [:and
              {:registry {::a ::b
                          ::b ::c
                          ::c [:schema 'pos-int?]}}
              [:and ::a ::b ::c]]
             (m/form schema)))))

  (testing "re schemas"
    (doseq [form [[:re "^[a-z]+\\.[a-z]+$"]
                  [:re #"^[a-z]+\.[a-z]+$"]
                  #"^[a-z]+\.[a-z]+$"]]
      (let [schema (m/schema form)
            re (if (sequential? form) (last form) form)]

        (is (true? (m/validate schema "a.b")))
        (is (false? (m/validate schema "abba")))
        (is (false? (m/validate schema ".b")))
        (is (false? (m/validate schema false)))

        (is (nil? (m/explain schema "a.b")))
        (is (results= {:schema schema, :value "abba", :errors [{:path [], :in [], :schema schema, :value "abba"}]}
                      (m/explain schema "abba")))

        (is (= "a.b" (m/parse schema "a.b")))
        (is (= ::m/invalid (m/parse schema "abba")))
        (is (= ::m/invalid (m/parse schema ".b")))
        (is (= ::m/invalid (m/parse schema false)))
        (is (= "a.b" (m/unparse schema "a.b")))
        (is (= ::m/invalid (m/unparse schema "abba")))
        (is (= ::m/invalid (m/unparse schema ".b")))
        (is (= ::m/invalid (m/unparse schema false)))

        (is (= 4 (m/decode
                  [:re {:decode/string '{:enter inc, :leave (partial * 2)}} ".*"]
                  1 mt/string-transformer)))

        (is (true? (m/validate (over-the-wire schema) "a.b")))

        (testing "ast"
          (is (= {:type :re, :value re}
                 (m/ast schema)))
          (is (true? (m/validate (m/from-ast (m/ast schema)) "a.b"))))

        (is (= form (m/form schema))))))

  (testing "ifn schemas"
    (let [schema (m/schema ifn?)]
      (is (true? (m/validate schema (fn []))))
      (is (true? (m/validate schema (constantly 1))))
      (is (true? (m/validate schema :keyword)))
      (is (true? (m/validate schema #?(:clj  (reify IFn
                                               (invoke [_] "Invoked!"))
                                       :cljs (reify IFn
                                               (-invoke [_] "Invoked!"))))))))

  (testing "fn schemas"
    (doseq [fn ['(fn [x] (and (int? x) (< 10 x 18)))
                "(fn [x] (and (int? x) (< 10 x 18)))"]]
      (let [schema (m/schema [:fn {:description "number between 10 and 18"} fn])]

        (is (true? (m/validate schema 12)))
        (is (false? (m/validate schema 1)))
        (is (false? (m/validate schema 20)))
        (is (false? (m/validate schema "invalid")))

        (is (nil? (m/explain schema 12)))
        (is (results= {:schema schema, :value "abba", :errors [{:path [], :in [], :schema schema, :value "abba"}]}
                      (m/explain schema "abba")))

        (is (= 12 (m/parse schema 12)))
        (is (= ::m/invalid (m/parse schema 1)))
        (is (= ::m/invalid (m/parse schema 20)))
        (is (= ::m/invalid (m/parse schema "invalid")))
        (is (= 12 (m/unparse schema 12)))
        (is (= ::m/invalid (m/unparse schema 1)))
        (is (= ::m/invalid (m/unparse schema 20)))
        (is (= ::m/invalid (m/unparse schema "invalid")))

        (is (= 4 (m/decode
                  [:fn {:decode/string '{:enter inc, :leave (partial * 2)}} 'int?]
                  1 mt/string-transformer)))

        (is (true? (m/validate (over-the-wire schema) 12)))

        (testing "ast"
          (is (= {:type :fn
                  :value fn
                  :properties {:description "number between 10 and 18"}}
                 (m/ast schema)))
          (is (true? (m/validate (m/from-ast (m/ast schema)) 12))))

        (is (= [:fn {:description "number between 10 and 18"} fn]
               (m/form schema)))))

    #?(:clj
       (testing "non-terminating functions DO NOT fail fast"
         (let [schema [:fn '(fn [x] (< x (apply max (range))))]]
           #?(:bb  nil ;; Graalvm doesn't support calling .stop on Threads since that is deprecated
              :clj (is (= ::miu/timeout (miu/-run (fn [] (m/validate schema 1)) 100))))
           #_(is (false? (m/validate schema 1)))
           #_(is (results= {:schema schema
                            :value 1
                            :errors [{:path []
                                      :in []
                                      :schema schema
                                      :value 1
                                      :type :sci.error/realized-beyond-max}]}
                           (m/explain schema 1)))))))

  (testing "map schemas"
    (let [schema (m/schema
                  [:map
                   [:x boolean?]
                   [:y {:optional true} int?]
                   [:z {:optional false} string?]])
          closed-schema (m/schema
                         [:map {:closed true}
                          [:x boolean?]
                          [:y {:optional true} int?]
                          [:z {:optional false} string?]])
          valid {:x true, :y 1, :z "kikka"}
          valid2 {:x false, :y 1, :z "kikka"}
          valid-with-extras {:x true, :y 1, :z "kikka", :extra "key"}
          invalid {:x true, :y "invalid", :z "kikka", :extra "ok"}]

      (is (true? (m/validate schema valid)))
      (is (true? (m/validate schema valid-with-extras)))
      (is (true? (m/validate schema valid2)))
      (is (false? (m/validate schema invalid)))
      (is (false? (m/validate schema "not-a-map")))

      (is (true? (m/validate closed-schema valid)))
      (is (false? (m/validate closed-schema valid-with-extras)))

      (is (nil? (m/explain schema valid)))
      (is (nil? (m/explain schema valid2)))

      (is (schema= [[:x nil 'boolean?]
                    [:y {:optional true} 'int?]
                    [:z {:optional false} 'string?]]
                   (m/children schema)))

      (is (true? (every? map-entry? (m/entries schema))))
      (is (= [:x :y :z] (map key (m/entries schema))))
      (is (= [:x :y :z] (m/explicit-keys schema)))

      (is (schema= [[:x [::m/val 'boolean?]]
                    [:y [::m/val {:optional true} 'int?]]
                    [:z [::m/val {:optional false} 'string?]]]
                   (m/entries schema)))

      (is (results= {:schema schema
                     :value {:y "invalid" :z "kikka"}
                     :errors
                     [{:path [:x], :in [:x], :schema schema, :value nil, :type ::m/missing-key}
                      {:path [:y], :in [:y], :schema int?, :value "invalid"}]}
                    (m/explain schema {:y "invalid" :z "kikka"})))

      (is (results= {:schema schema,
                     :value {:x "invalid"},
                     :errors [{:path [:x], :in [:x], :schema boolean?, :value "invalid"}
                              {:path [:z],
                               :in [:z],
                               :schema schema,
                               :value nil,
                               :type :malli.core/missing-key}]}
                    (m/explain schema {:x "invalid"})))

      (is (results= {:schema schema
                     :value "not-a-map"
                     :errors [{:path [], :in [], :schema schema, :value "not-a-map", :type ::m/invalid-type}]}
                    (m/explain schema "not-a-map")))

      (is (results= {:schema closed-schema
                     :value valid-with-extras
                     :errors [{:path [:extra],
                               :in [:extra],
                               :schema closed-schema,
                               :value "key",
                               :type :malli.core/extra-key,
                               :message nil}]}
                    (m/explain closed-schema valid-with-extras)))

      (is (= valid (m/parse schema valid)))
      (is (= valid-with-extras (m/parse schema valid-with-extras)))
      (is (= valid2 (m/parse schema valid2)))
      (is (= ::m/invalid (m/parse schema invalid)))
      (is (= ::m/invalid (m/parse schema "not-a-map")))
      (is (= valid (m/unparse schema valid)))
      (is (= valid-with-extras (m/unparse schema valid-with-extras)))
      (is (= valid2 (m/unparse schema valid2)))
      (is (= ::m/invalid (m/unparse schema invalid)))
      (is (= ::m/invalid (m/unparse schema "not-a-map")))

      (is (= valid (m/parse closed-schema valid)))
      (is (= ::m/invalid (m/parse closed-schema valid-with-extras)))
      (is (= valid (m/unparse closed-schema valid)))
      (is (= ::m/invalid (m/unparse closed-schema valid-with-extras)))

      (is (= {:x true} (m/decode schema {:x "true"} mt/string-transformer)))
      (is (= {:x true, :y 1} (m/decode schema {:x "true", :y "1"} mt/string-transformer)))
      (is (= {:x "true", :y "1"} (m/decode schema {:x "true", :y "1"} mt/json-transformer)))
      (is (= {:x true, :y 1} (m/decode schema {:x true, :y 1, :a 1} mt/strip-extra-keys-transformer)))
      (is (= {:x_key true, :y_key 2} (m/decode schema {:x true, :y 2}
                                               (mt/key-transformer
                                                {:decode #(-> % name (str "_key") keyword)}))))

      (testing "JSON transformer can decode map schema keys"
        (let [schema
              [:map
               [:a :uuid]
               [:b [:enum :x :y :z]]
               ["s" :boolean [:enum :f1 :f2]]]
              value
              {"a" "b699671c-d34d-b33f-1337-dbdbfd337e73"
               "b" "x"
               "s" "f1"}
              expected-decoded-value
              {:a #uuid "b699671c-d34d-b33f-1337-dbdbfd337e73"
               :b :x
               "s" :f1}
              decoded-value (m/decode schema value (mt/json-transformer {::mt/keywordize-map-keys true}))]
          (is (= expected-decoded-value decoded-value))
          (is (m/validate schema decoded-value))))

      (testing "JSON transformer maintains type of map"
        (let [schema [:map [:a :keyword] [:b :string]]
              value (sorted-map "a" "x" "b" "y")
              expected-decoded-value (sorted-map :a :x :b "y")
              decoded-value (m/decode schema value (mt/json-transformer {::mt/keywordize-map-keys true}))]
          (is (sorted? decoded-value))
          (is (= expected-decoded-value decoded-value))
          (is (m/validate schema decoded-value))))

      (is (= {:x 32}
             (m/decode
              [:map {:decode/string '{:enter #(update % :x inc), :leave #(update % :x (partial * 2))}}
               [:x {:decode/string '{:enter inc, :leave inc}}
                [int? {:decode/string '{:enter (partial + 2), :leave (partial * 3)}}]]]
              {:x 1}
              mt/string-transformer)))

      (is (true? (m/validate (over-the-wire schema) valid)))

      (testing "ast"
        (is (= {:type :map,
                :keys {:x {:order 0
                           :value {:type 'boolean?}},
                       :y {:order 1
                           :value {:type 'int?}
                           :properties {:optional true}},
                       :z {:order 2
                           :value {:type 'string?}
                           :properties {:optional false}}}}
               (m/ast schema)))
        (is (true? (m/validate (m/from-ast (m/ast schema)) valid))))

      (is (= [:map
              [:x 'boolean?]
              [:y {:optional true} 'int?]
              [:z {:optional false} 'string?]]
             (m/form schema))))

    (is (true? (m/validate [:map [:b boolean?]] {:b true})))
    (is (true? (m/validate [:map [:b boolean?]] {:b false})))
    (is (true? (m/validate [:map [:n nil?]] {:n nil}))))

  (testing "nil keys"
    (is (true? (m/validate
                [:map
                 ["status" [:enum "ok"]]
                 [1 any?]
                 [nil any?]
                 [::a string?]]
                {"status" "ok"
                 1 'number
                 nil :yay
                 ::a "properly awesome"}))))

  (testing "accumulating errors #84"
    (let [re #"b"
          schema [:map
                  [:a string?]
                  [:b [:fn 'pos?]]
                  [:c re]
                  [:d [:maybe int?]]]
          value {:b -1, :c "", :d "not"}]
      (results= {:schema schema,
                 :value value,
                 :errors [{:path [1 0], :in [:a], :schema schema, :type :malli.core/missing-key}
                          {:path [2 1], :in [:b], :schema [:fn 'pos?], :value -1}
                          {:path [3 1], :in [:c], :schema re, :value ""}
                          {:path [4 1], :in [:d], :schema [:maybe int?], :value "not"}]}
                (m/explain schema value))))

  (testing "multi-schemas"
    (let [schema [:multi {:dispatch :type
                          :decode/string '(fn [x] (update x :type keyword))}
                  [:sized [:map [:type keyword?] [:size int?]]]
                  [:human [:map [:type keyword?] [:name string?] [:address [:map [:country keyword?]]]]]]
          valid1 {:type :sized, :size 10}
          valid2 {:type :human :name "inkeri", :address {:country :PO}}
          valid3 (map->SomeRecord {:type :sized, :size 10})
          invalid1 {:type :sized, :size "size"}
          invalid2 {:type :human :namez "inkeri"}
          invalid3 {:type :worm}
          invalid4 []
          invalid5 "xxx"
          invalid6 (map->SomeRecord {:type :record})]

      (is (true? (m/validate schema valid1)))
      (is (true? (m/validate schema valid2)))
      (is (true? (m/validate schema valid3)))
      (is (false? (m/validate schema invalid1)))
      (is (false? (m/validate schema invalid2)))
      (is (false? (m/validate schema invalid3)))
      (is (false? (m/validate schema invalid4)))
      (is (false? (m/validate schema invalid5)))

      (is (nil? (m/explain schema valid1)))
      (is (nil? (m/explain schema valid2)))
      (is (nil? (m/explain schema valid3)))

      (is (results= {:schema schema,
                     :value {:type :sized, :size "size"},
                     :errors [{:path [:sized :size], :in [:size], :schema int?, :value "size"}]}
                    (m/explain schema invalid1)))

      (is (results= {:schema schema,
                     :value {:type :human, :namez "inkeri"},
                     :errors [{:path [:human :name]
                               :in [:name]
                               :schema [:map [:type keyword?] [:name string?] [:address [:map [:country keyword?]]]]
                               :value nil
                               :type :malli.core/missing-key}
                              {:path [:human :address]
                               :in [:address]
                               :schema [:map [:type keyword?] [:name string?] [:address [:map [:country keyword?]]]]
                               :value nil
                               :type :malli.core/missing-key}]}
                    (m/explain schema invalid2)))

      (is (results= {:schema schema,
                     :value {:type :worm}
                     :errors [{:path [:type]
                               :in [:type]
                               :schema schema
                               :value {:type :worm}
                               :type :malli.core/invalid-dispatch-value}]}
                    (m/explain schema invalid3)))

      (is (results= {:schema schema,
                     :value []
                     :errors [{:path []
                               :in []
                               :schema schema
                               :value []
                               :type :malli.core/invalid-dispatch-value}]}
                    (m/explain schema invalid4)))

      (is (results= {:schema schema,
                     :value "xxx"
                     :errors [{:path []
                               :in []
                               :schema schema
                               :value "xxx"
                               :type :malli.core/invalid-dispatch-value}]}
                    (m/explain schema invalid5)))

      (is (results= {:schema schema,
                     :value invalid6
                     :errors [{:path [:type]
                               :in [:type]
                               :schema schema
                               :value invalid6
                               :type :malli.core/invalid-dispatch-value}]}
                    (m/explain schema invalid6)))

      (is (= (m/tag :sized valid1) (m/parse schema valid1)))
      (is (= (m/tag :human valid2) (m/parse schema valid2)))
      (is (= (m/tag :sized valid3) (m/parse schema valid3)))
      (is (= ::m/invalid (m/parse schema invalid1)))
      (is (= ::m/invalid (m/parse schema invalid2)))
      (is (= ::m/invalid (m/parse schema invalid3)))
      (is (= ::m/invalid (m/parse schema invalid4)))
      (is (= ::m/invalid (m/parse schema invalid5)))
      (is (= ::m/invalid (m/parse schema invalid6)))
      (is (= valid1 (m/unparse schema (m/parse schema valid1))))
      (is (= valid1 (m/unparse schema (m/tag :sized valid1))))
      (is (= valid2 (m/unparse schema (m/parse schema valid2))))
      (is (= valid2 (m/unparse schema (m/tag :human valid2))))
      (is (= valid3 (m/unparse schema (m/parse schema valid3))))
      (is (= valid3 (m/unparse schema (m/tag :sized valid3))))
      (is (= ::m/invalid (m/unparse schema invalid1)))
      (is (= ::m/invalid (m/unparse schema invalid2)))
      (is (= ::m/invalid (m/unparse schema invalid3)))
      (is (= ::m/invalid (m/unparse schema invalid4)))
      (is (= ::m/invalid (m/unparse schema invalid5)))
      (is (= ::m/invalid (m/unparse schema invalid6)))

      (is (= {:type :sized, :size 10}
             (m/decode schema {:type "sized", :size "10"} mt/string-transformer)))
      (is (= {:type :human, :name "liisa", :address {:country :PO}}
             (m/decode schema {:type "human", :name "liisa", :address {:country "PO"}} mt/string-transformer)))
      (is (= {:type :sized, :size 10}
             (m/decode schema {:type :sized, :size 10, :nesting true} mt/strip-extra-keys-transformer)))

      (is (= {:type :math, :x 24}
             (m/decode
              [:multi {:dispatch :type
                       :decode/string '{:enter #(update % :x inc), :leave #(update % :x (partial * 2))}}
               [:math [:map [:type keyword?] [:x [int? {:decode/string '{:enter (partial + 2), :leave (partial * 3)}}]]]]]
              {:type :math, :x 1} mt/string-transformer)))

      (testing "in multi schema (two options)"
        (is (= {:category :book :title "FOUNTAINHEAD"}
               (m/decode
                [:multi
                 {:dispatch :category}
                 [:book
                  [:map
                   [:category [:= :book]]
                   [:title {:decode/string-upper clojure.string/upper-case} string?]]]
                 [:video
                  [:map
                   [:category [:= :video]]
                   [:name string?]]]]
                {:category :book :title "Fountainhead"}
                (mt/transformer {:name :string-upper})))))

      (is (true? (m/validate (over-the-wire schema) valid1)))

      (testing "ast"
        (is (= {:type :multi,
                :keys {:sized {:order 0,
                               :value {:type :map,
                                       :keys {:type {:order 0
                                                     :value {:type 'keyword?}}
                                              :size {:order 1
                                                     :value {:type 'int?}}}}},
                       :human {:order 1,
                               :value {:type :map,
                                       :keys {:type {:order 0
                                                     :value {:type 'keyword?}},
                                              :name {:order 1
                                                     :value {:type 'string?}}
                                              :address {:order 2
                                                        :value {:type :map
                                                                :keys {:country {:order 0
                                                                                 :value {:type 'keyword?}}}}}}}}}
                :properties {:dispatch :type, :decode/string '(fn [x] (update x :type keyword))}}
               (m/ast schema)))
        (is (true? (m/validate (m/from-ast (m/ast schema)) valid1))))

      (is (schema= [[:sized nil [:map [:type 'keyword?] [:size 'int?]]]
                    [:human nil [:map [:type 'keyword?] [:name 'string?] [:address [:map [:country 'keyword?]]]]]]
                   (m/children schema)))

      (is (= [:multi
              {:dispatch :type, :decode/string '(fn [x] (update x :type keyword))}
              [:sized [:map [:type 'keyword?] [:size 'int?]]]
              [:human [:map [:type 'keyword?] [:name 'string?] [:address [:map [:country 'keyword?]]]]]]
             (m/form schema))))

    (let [schema [:multi {:dispatch first}
                  [:human [:cat [:= :human]]]
                  [:bear [:cat [:= :bear] [:* :int]]]
                  [::m/default [:tuple :string :string]]]]

      (testing "validate"
        (is (m/validate schema [:human]))
        (is (m/validate schema [:bear 1 2 3]))
        (is (m/validate schema ["defaultit" "toimii"]))
        (is (not (m/validate schema [:so :invalid]))))

      (testing "explain"
        (is (not (m/explain schema [:human])))
        (is (not (m/explain schema [:bear 1 2 3])))
        (is (not (m/explain schema ["defaultit" "toimii"])))
        (is (results= {:schema schema,
                       :value [:so :invalid],
                       :errors [{:path [::m/default 0], :in [0], :schema :string, :value :so}
                                {:path [::m/default 1], :in [1], :schema :string, :value :invalid}]}
                      (m/explain schema [:so :invalid]))))

      (testing "default schema"
        (is (schema= [:tuple :string :string] (m/default-schema schema))))

      (testing "parser"
        (is (= (m/tag :human [:human]) (m/parse schema [:human])))
        (is (= (m/tag :bear [:bear [1 2 3]]) (m/parse schema [:bear 1 2 3])))
        (is (= (m/tag ::m/default ["defaultit" "toimii"]) (m/parse schema ["defaultit" "toimii"])))
        (is (= ::m/invalid (m/parse schema [:so :invalid]))))))

  (testing "map-of schema"

    (is (true? (m/validate [:map-of string? int?] {"age" 18})))
    (is (true? (m/validate [:map-of keyword? int?] {:age 18})))
    (is (false? (m/validate [:map-of string? int?] {:age "18"})))
    (is (false? (m/validate [:map-of string? int?] 1)))

    (testing "limits"
      (is (true? (m/validate [:map-of {:min 1} keyword? int?] {:age 18})))
      (is (false? (m/validate [:map-of {:min 2} keyword? int?] {:age 18})))
      (is (true? (m/validate [:map-of {:min 1 :max 3} keyword? int?] {:age 18})))
      (is (true? (m/validate [:map-of {:min 1 :max 3} keyword? int?] {:age 18 :-a-g-e 3})))
      (is (false? (m/validate [:map-of {:max 1} keyword? int?] {:age 18 :-a-g-e 3}))))

    (is (nil? (m/explain [:map-of string? int?] {"age" 18})))
    (is (some? (m/explain [:map-of string? int?] ::invalid)))
    (is (results= {:schema [:map-of string? int?],
                   :value {:age 18},
                   :errors [{:path [0],
                             :in [:age],
                             :schema string?,
                             :value :age}]}
                  (m/explain [:map-of string? int?] {:age 18})))
    (is (results= {:schema [:map-of string? int?],
                   :value {:age "18"},
                   :errors [{:path [0],
                             :in [:age],
                             :schema string?,
                             :value :age}
                            {:path [1],
                             :in [:age],
                             :schema int?,
                             :value "18"}]}
                  (m/explain [:map-of string? int?] {:age "18"})))

    (is (= {"age" 18} (m/parse [:map-of string? int?] {"age" 18})))
    (is (= {:age 18} (m/parse [:map-of keyword? int?] {:age 18})))
    (is (= ::m/invalid (m/parse [:map-of string? int?] {:age "18"})))
    (is (= ::m/invalid (m/parse [:map-of string? int?] 1)))
    (is (= {"age" 18} (m/unparse [:map-of string? int?] {"age" 18})))
    (is (= {:age 18} (m/unparse [:map-of keyword? int?] {:age 18})))
    (is (= ::m/invalid (m/unparse [:map-of string? int?] {:age "18"})))
    (is (= ::m/invalid (m/unparse [:map-of string? int?] 1)))

    (is (= {1 1} (m/decode [:map-of int? pos-int?] {"1" "1"} mt/string-transformer)))

    (is (= {:x 24}
           (m/decode
            [:map-of {:decode/string '{:enter #(update % :x inc), :leave #(update % :x (partial * 2))}}
             keyword? [int? {:decode/string '{:enter (partial + 2), :leave (partial * 3)}}]]
            {:x 1} mt/string-transformer)))

    (is (true? (m/validate (over-the-wire [:map-of string? int?]) {"age" 18})))

    (testing "ast"
      (is (= {:type :map-of,
              :key {:type 'int?}
              :value {:type 'pos-int?}}
             (m/ast [:map-of int? pos-int?])))
      (is (true? (m/validate (m/from-ast (m/ast [:map-of int? pos-int?])) {1 1}))))

    (testing "keyword keys are transformed via strings"
      (is (= {1 1} (m/decode [:map-of int? pos-int?] {:1 "1"} mt/string-transformer)))))

  (testing "sequence schemas"

    (testing "empty schemas fail"
      (doseq [element [:vector :sequential :set]]
        (is (thrown? #?(:clj Exception, :cljs js/Error) (m/schema [element])))))

    (testing "empty tuples are ok"
      (is (m/validate :tuple []))
      (is (nil? (m/explain :tuple [])))
      (is (m/explain :tuple [1]))
      (is (not (m/validate :tuple nil))))

    (testing "more than 1 elements fail on collections"
      (doseq [element [:vector :sequential :set]]
        (is (thrown? #?(:clj Exception, :cljs js/Error) (m/schema [element int? int?])))))

    (testing "validation + parse"
      (let [expectations {"vector" [[true [:vector int?] [1 2 3]]
                                    [false [:vector int?] [1 "2" 3]]
                                    [false [:vector int?] [1 2 "3"]]
                                    [false [:vector int?] [nil]]
                                    [false [:vector int?] "invalid"]

                                    [true [:vector {:min 3} int?] [1 2 3]]
                                    [false [:vector {:min 4} int?] [1 2 3]]

                                    [true [:vector {:max 3} int?] [1 2 3]]
                                    [false [:vector {:max 2} int?] [1 2 3]]

                                    [true [:vector {:min 1, :max 3} int?] [1 2 3]]
                                    [false [:vector {:min 4, :max 4} int?] [1 2 3]]

                                    [false [:vector int?] '(1 2 3)]
                                    [false [:vector int?] #{1 2 3}]]

                          "sequential" [[true [:sequential int?] '(1 2 3)]
                                        [true [:sequential int?] [1 2 3]]
                                        [true [:sequential int?] (range 10)]
                                        [false [:sequential int?] #{1 2 3}]
                                        [false [:sequential int?] nil]]

                          "set" [[true [:set int?] #{1 2 3}]
                                 [false [:set int?] #{1 "2" 3}]
                                 [false [:set int?] #{1 2 "3"}]
                                 [false [:set int?] #{nil}]
                                 [false [:set int?] "invalid"]

                                 [true [:set {:min 3} int?] #{1 2 3}]
                                 [false [:set {:min 4} int?] #{1 2 3}]

                                 [true [:set {:max 3} int?] #{1 2 3}]
                                 [false [:set {:max 2} int?] #{1 2 3}]

                                 [true [:set {:min 1, :max 3} int?] #{1 2 3}]
                                 [false [:set {:min 4, :max 4} int?] #{1 2 3}]

                                 [false [:set int?] '(1 2 3)]
                                 [false [:set int?] [1 2 3]]]

                          "tuple" [[true [:tuple int?] [1]]
                                   [true [:tuple int? string?] [1 "2"]]
                                   [false [:tuple int?] ["1"]]
                                   [false [:tuple int?] [nil]]
                                   [false [:tuple int?] "invalid"]

                                   ;; ignored
                                   [true [:tuple {:min 3} int?] [1]]
                                   [true [:tuple {:min 4} int?] [1]]

                                   ;; ignored
                                   [true [:tuple {:max 3} int?] [1]]
                                   [true [:tuple {:max 2} int?] [1]]

                                   ;; ignored
                                   [true [:tuple {:min 1, :max 3} int?] [1]]
                                   [true [:tuple {:min 4, :max 4} int?] [1]]

                                   [false [:tuple int?] '(1)]
                                   [false [:tuple int?] #{1}]]}]

        (doseq [[name data] expectations
                [expected schema value] data]
          (testing name
            (is (= expected (m/validate schema value)))
            (is (= expected (m/validate (over-the-wire schema) value)))
            (is (= (if expected value ::m/invalid) (m/parse schema value)))
            (is (= (if expected value ::m/invalid) (m/unparse schema value)))))))

    (testing "transform"
      (is (= {:x 1} (m/decode [:vector [:map [:x int?]]] {:x 1} (mt/transformer {:name "test"}))))
      (is (= [24 30 36 42]
             (m/decode
              [:sequential
               {:decode/string '{:enter (partial map inc), :leave (partial map (partial * 2))}}
               [int? {:decode/string '{:enter (partial + 2), :leave (partial * 3)}}]]
              [1 2 3 4] mt/string-transformer)))
      (is (= [24 48 8 10]
             (m/decode
              [:tuple
               {:decode/string '{:enter (partial mapv inc), :leave (partial mapv (partial * 2))}}
               [int? {:decode/string '{:enter (partial + 2), :leave (partial * 3)}}]
               [int? {:decode/string '{:enter (partial + 3), :leave (partial * 4)}}]]
              [1 2 3 4] mt/string-transformer)))

      (testing "changing type results in children not being called"
        (are [schema data]
          (= "age:31"
             (m/encode schema data
                       (let [should-not-be-called
                             (fn [_] (throw (ex-info "Was called" {:schema schema
                                                                   :data data})))]
                         (mt/transformer
                          {:name :test
                           :encoders {'int? should-not-be-called
                                      'keyword? should-not-be-called}}))))

          [:map {:encode/test (fn [{:keys [age]}]
                                (str "age:" age))}
           [:age int?]]
          {:age 31}
          [:map-of {:encode/test (fn [{:keys [age]}]
                                   (str "age:" age))}
           keyword? int?]
          {:age 31}
          [:tuple {:encode/test (fn [[_ age]]
                                  (str "age:" age))}
           keyword? int?]
          [:age 31]
          [:vector {:encode/test (fn [x]
                                   (str "age:" (:age (apply hash-map x))))}
           [:or keyword? int?]]
          [:age 31])))

    (testing "explain"
      (let [expectations {"vector" (let [schema [:vector {:min 2, :max 3} int?]]

                                     [[schema [1 2]
                                       nil]

                                      [schema 1
                                       {:schema schema
                                        :value 1
                                        :errors [{:path [], :in [], :type ::m/invalid-type, :schema schema, :value 1}]}]

                                      [schema [1]
                                       {:schema schema
                                        :value [1]
                                        :errors [{:path [], :in [], :type ::m/limits, :schema schema, :value [1]}]}]

                                      [schema [1 2 3 4]
                                       {:schema schema
                                        :value [1 2 3 4]
                                        :errors [{:path [], :in [], :type ::m/limits, :schema schema, :value [1 2 3 4]}]}]

                                      [schema [1 2 "3"]
                                       {:schema schema
                                        :value [1 2 "3"]
                                        :errors [{:path [0], :in [2], :schema int?, :value "3"}]}]])

                          "sequential" (let [schema [:sequential {:min 2, :max 3} int?]]

                                         [[schema '(1 2)
                                           nil]

                                          [schema 1
                                           {:schema schema
                                            :value 1
                                            :errors [{:path [], :in [], :type ::m/invalid-type, :schema schema, :value 1}]}]

                                          [schema '(1)
                                           {:schema schema
                                            :value '(1)
                                            :errors [{:path [], :in [], :type ::m/limits, :schema schema, :value '(1)}]}]

                                          [schema '(1 2 3 4)
                                           {:schema schema
                                            :value '(1 2 3 4)
                                            :errors [{:path [], :in [], :type ::m/limits, :schema schema, :value '(1 2 3 4)}]}]

                                          [schema '(1 2 "3")
                                           {:schema schema
                                            :value '(1 2 "3")
                                            :errors [{:path [0], :in [2], :schema int?, :value "3"}]}]])

                          "set" (let [schema [:set {:min 2, :max 3} int?]]

                                  [[schema #{1 2}
                                    nil]

                                   [schema 1
                                    {:schema schema
                                     :value 1
                                     :errors [{:path [], :in [], :type ::m/invalid-type, :schema schema, :value 1}]}]

                                   [schema #{1}
                                    {:schema schema
                                     :value #{1}
                                     :errors [{:path [], :in [], :type ::m/limits, :schema schema, :value #{1}}]}]

                                   [schema #{1 2 3 4}
                                    {:schema schema
                                     :value #{1 2 3 4}
                                     :errors [{:path [], :in [], :type ::m/limits, :schema schema, :value #{1 2 3 4}}]}]

                                   [schema #{1 2 "3"}
                                    {:schema schema
                                     :value #{1 2 "3"}
                                     :errors [{:path [0], :in ["3"], :schema int?, :value "3"}]}]])

                          "tuple" (let [schema [:tuple int? string?]]

                                    [[schema [1 "2"]
                                      nil]

                                     [schema 1
                                      {:schema schema
                                       :value 1
                                       :errors [{:path [], :in [], :type ::m/invalid-type, :schema schema, :value 1}]}]

                                     [schema [1]
                                      {:schema schema
                                       :value [1]
                                       :errors [{:path [], :in [], :type ::m/tuple-size, :schema schema, :value [1]}]}]

                                     [schema [1 2]
                                      {:schema schema
                                       :value [1 2]
                                       :errors [{:path [1], :in [1], :schema string?, :value 2}]}]])

                          "map+enum" (let [schema [:map
                                                   [:x [:enum "x"]]
                                                   [:y [:enum "y"]]]]

                                       [[schema {:x "x" :y "y"}
                                         nil]

                                        [schema {:x "non-x" :y "y"}
                                         {:schema schema
                                          :value {:x "non-x" :y "y"}
                                          :errors [{:path [:x], :in [:x], :schema [:enum "x"], :value "non-x"}]}]

                                        [schema {:x "x" :y "non-y"}
                                         {:schema schema
                                          :value {:x "x" :y "non-y"}
                                          :errors [{:path [:y], :in [:y], :schema [:enum "y"], :value "non-y"}]}]

                                        [schema {:x "non-x" :y "non-y"}
                                         {:schema schema
                                          :value {:x "non-x" :y "non-y"}
                                          :errors [{:path [:x], :in [:x], :schema [:enum "x"], :value "non-x"}
                                                   {:path [:y], :in [:y], :schema [:enum "y"], :value "non-y"}]}]])}
            expectations (assoc expectations "sequential" (concat (get expectations "list") (get expectations "vector")))]

        (doseq [[name data] expectations
                [schema value expected] data]
          (testing name
            (is (results= expected (m/explain schema value)))))))

    (testing "ast"
      (doseq [[name x] [[:vector [1 2 3]] [:sequential [1 2 3]] [:set #{1 2 3}]]]
        (is (= {:type name, :child {:type 'int?}}
               (m/ast [name int?])))
        (is (true? (m/validate (m/from-ast (m/ast [name int?])) x))))
      (is (= {:type :tuple, :children [{:type 'int?} {:type 'int?}]}
             (m/ast [:tuple int? int?])))
      (is (true? (m/validate (m/from-ast (m/ast [:tuple int? int?])) [1 2])))))

  (testing "seqex schemas"
    (doseq [typ [:cat :catn]]
      (testing typ
        (testing "empty"
          (let [s [typ]]
            (are [v v* errs]
              (let [es errs]
                (and (= (m/validate s v) (nil? es))
                     (results= (m/explain s v) (and es {:schema s, :value v, :errors es}))
                     (= (m/parse s v) (if (nil? es) (if (= typ :cat) v v*) ::m/invalid))
                     (or (some? es) (= (m/unparse s (if (= typ :cat) v v*)) v))))

              0 nil [{:path [], :in [], :schema s, :value 0, :type ::m/invalid-type}]
              "foo" nil [{:path [], :in [], :schema s, :value "foo", :type ::m/invalid-type}]
              nil nil [{:path [], :in [], :schema s, :value nil, :type ::m/invalid-type}]
              [] (m/tags {}) nil
              [0] nil [{:path [], :in [0], :schema s, :value 0, :type ::m/input-remaining}])))

        (testing "single"
          (let [s [typ (case typ :cat string? [:s string?])]]
            (are [v v* errs]
              (let [es errs]
                (and (= (m/validate s v) (nil? es))
                     (results= (m/explain s v) (and es {:schema s, :value v, :errors es}))
                     (= (m/parse s v) (if (nil? es) (if (= typ :cat) v v*) ::m/invalid))
                     (or (some? es) (= (m/unparse s (if (= typ :cat) v v*)) v))))

              0 nil [{:path [], :in [], :schema s, :value 0, :type ::m/invalid-type}]
              "foo" nil [{:path [], :in [], :schema s, :value "foo", :type ::m/invalid-type}]
              nil nil [{:path [], :in [], :schema s, :value nil, :type ::m/invalid-type}]
              [] nil [{:path [(case typ :catn :s 0)], :in [0], :schema string?, :value nil, :type ::m/end-of-input}]
              ["foo"] (m/tags {:s "foo"}) nil
              [0] nil [{:path [(case typ :catn :s 0)], :in [0], :schema string?, :value 0}]
              ["foo" "bar"] nil [{:path [], :in [1], :schema s, :value "bar", :type ::m/input-remaining}])))

        (testing "pair"
          (let [s [typ (case typ :cat string? [:s string?]) (case typ :cat int? [:n int?])]]
            (are [v v* errs]
              (let [es errs]
                (and (= (m/validate s v) (nil? es))
                     (results= (m/explain s v) (and es {:schema s, :value v, :errors es}))
                     (= (m/parse s v) (if (nil? es) (if (= typ :cat) v v*) ::m/invalid))
                     (or (some? es) (= (m/unparse s (if (= typ :cat) v v*)) v))))

              0 nil [{:path [], :in [], :schema s, :value 0, :type ::m/invalid-type}]
              "foo" nil [{:path [], :in [], :schema s, :value "foo", :type ::m/invalid-type}]
              nil nil [{:path [], :in [], :schema s, :value nil, :type ::m/invalid-type}]
              [] nil [{:path [(case typ :catn :s 0)], :in [0], :schema string?, :value nil, :type ::m/end-of-input}]
              ["foo"] nil [{:path [(case typ :catn :n 1)], :in [1], :schema int?, :value nil, :type ::m/end-of-input}]
              ["foo" 0] (m/tags {:s "foo", :n 0}) nil
              ["foo" "bar"] nil [{:path [(case typ :catn :n 1)], :in [1], :schema int?, :value "bar"}]
              [1 2] nil [{:path [(case typ :catn :s 0)], :in [0], :schema string?, :value 1}]
              ["foo" 0 1] nil [{:path [], :in [2], :schema s, :value 1, :type ::m/input-remaining}])))

        (testing "triplet"
          (let [s [typ (case typ :cat string? [:s string?]) (case typ :cat int? [:n int?])
                   (case typ :cat keyword? [:k keyword?])]]
            (are [v v* errs]
              (let [es errs]
                (and (= (m/validate s v) (nil? es))
                     (results= (m/explain s v) (and es {:schema s, :value v, :errors es}))
                     (= (m/parse s v) (if (nil? es) (if (= typ :cat) v v*) ::m/invalid))
                     (or (some? es) (= (m/unparse s (if (= typ :cat) v v*)) v))))

              0 nil [{:path [], :in [], :schema s, :value 0, :type ::m/invalid-type}]
              "foo" nil [{:path [], :in [], :schema s, :value "foo", :type ::m/invalid-type}]
              nil nil [{:path [], :in [], :schema s, :value nil, :type ::m/invalid-type}]
              [] nil [{:path [(case typ :catn :s 0)], :in [0], :schema string?, :value nil, :type ::m/end-of-input}]
              ["foo"] nil [{:path [(case typ :catn :n 1)], :in [1], :schema int?, :value nil, :type ::m/end-of-input}]
              ["foo" 0] nil [{:path [(case typ :catn :k 2)], :in [2], :schema keyword?, :value nil, :type ::m/end-of-input}]
              ["foo" 0 :bar] (m/tags {:s "foo", :n 0, :k :bar}) nil
              ["foo" 0 "bar"] nil [{:path [(case typ :catn :k 2)], :in [2], :schema keyword?, :value "bar"}]
              ["foo" 0 :bar 0] nil [{:path [], :in [3], :schema s, :value 0, :type ::m/input-remaining}])))

        (testing "* backtracks"
          (let [s [:cat [:* pos?] [:= 4]]
                s* [:catn [:pos [:* pos?]] [:four [:= 4]]]
                v [4 4 4 4]]
            (is (m/validate s v))

            (is (= [[4 4 4] 4] (m/parse s v)))
            (is (= (m/tags {:pos [4 4 4], :four 4}) (m/parse s* v)))
            (is (= v (m/unparse s [[4 4 4] 4])))
            (is (= v (m/unparse s* (m/tags {:pos [4 4 4], :four 4}))))))))

    (doseq [typ [:alt :altn]]
      (testing typ
        (testing "empty"
          (is (thrown? #?(:clj Exception, :cljs js/Error) (m/validator [typ]))))

        (testing "single"
          (let [s [typ (case typ :alt string? [:s string?])]]
            (are [v v*s errs]
              (let [[v* v**] v*s
                    es errs]
                (and (= (m/validate s v) (nil? es))
                     (results= (m/explain s v) (and es {:schema s, :value v, :errors es}))
                     (= (m/parse s v) (if (nil? es) (if (= typ :alt) v* v**) ::m/invalid))
                     (or (some? es) (= (m/unparse s (if (= typ :alt) v* v**)) v))))

              0 nil [{:path [], :in [], :schema s, :value 0, :type ::m/invalid-type}]
              "foo" nil [{:path [], :in [], :schema s, :value "foo", :type ::m/invalid-type}]
              nil nil [{:path [], :in [], :schema s, :value nil, :type ::m/invalid-type}]
              ["foo"] ["foo" (m/tag :s "foo")] nil
              [0] nil [{:path [(case typ :altn :s 0)], :in [0], :schema string?, :value 0}]
              ["foo" 0] nil [{:path [], :in [1], :schema s, :value 0, :type ::m/input-remaining}])))

        (testing "pair"
          (let [s [typ (case typ :alt string? [:s string?]) (case typ :alt int? [:n int?])]]
            (are [v v*s errs]
              (let [[v* v**] v*s
                    es errs]
                (and (= (m/validate s v) (nil? es))
                     (results= (m/explain s v) (and es {:schema s, :value v, :errors es}))
                     (= (m/parse s v) (if (nil? es) (if (= typ :alt) v* v**) ::m/invalid))
                     (or (some? es) (= (m/unparse s (if (= typ :alt) v* v**)) v))))

              0 nil [{:path [], :in [], :schema s, :value 0, :type ::m/invalid-type}]
              "foo" nil [{:path [], :in [], :schema s, :value "foo", :type ::m/invalid-type}]
              nil nil [{:path [], :in [], :schema s, :value nil, :type ::m/invalid-type}]
              ["foo"] ["foo" (m/tag :s "foo")] nil
              [0] [0 (m/tag :n 0)] nil
              ["foo" 0] nil [{:path [], :in [1], :schema s, :value 0, :type ::m/input-remaining}]
              [0 "foo"] nil [{:path [], :in [1], :schema s, :value "foo", :type ::m/input-remaining}])))

        (testing "triplet"
          (let [s [typ (case typ :alt string? [:s string?]) (case typ :alt int? [:n int?])
                   (case typ :alt keyword? [:k keyword?])]]
            (are [v v*s errs]
              (let [[v* v**] v*s
                    es errs]
                (and (= (m/validate s v) (nil? es))
                     (results= (m/explain s v) (and es {:schema s, :value v, :errors es}))
                     (= (m/parse s v) (if (nil? es) (if (= typ :alt) v* v**) ::m/invalid))
                     (or (some? es) (= (m/unparse s (if (= typ :alt) v* v**)) v))))

              0 nil [{:path [], :in [], :schema s, :value 0, :type ::m/invalid-type}]
              "foo" nil [{:path [], :in [], :schema s, :value "foo", :type ::m/invalid-type}]
              nil nil [{:path [], :in [], :schema s, :value nil, :type ::m/invalid-type}]
              ["foo"] ["foo" (m/tag :s "foo")] nil
              [0] [0 (m/tag :n 0)] nil
              [:foo] [:foo (m/tag :k :foo)] nil
              ["foo" 0] nil [{:path [], :in [1], :schema s, :value 0, :type ::m/input-remaining}]
              [0 "foo"] nil [{:path [], :in [1], :schema s, :value "foo", :type ::m/input-remaining}]
              [:foo 0] nil [{:path [], :in [1], :schema s, :value 0, :type ::m/input-remaining}])))))

    (testing "?"
      (is (thrown? #?(:clj Exception, :cljs js/Error) (m/validator [:?])))
      (is (thrown? #?(:clj Exception, :cljs js/Error) (m/validator [:? string? int?])))

      (let [s [:? string?]]
        (are [v v* errs]
          (let [es errs]
            (and (= (m/validate s v) (nil? es))
                 (results= (m/explain s v) (and es {:schema s, :value v, :errors es}))
                 (= (m/parse s v) (if (nil? es) v* ::m/invalid))
                 (or (some? es) (= (m/unparse s v*) v))))

          0 nil [{:path [], :in [], :schema s, :value 0, :type ::m/invalid-type}]
          "foo" nil [{:path [], :in [], :schema s, :value "foo", :type ::m/invalid-type}]
          nil nil [{:path [], :in [], :schema s, :value nil, :type ::m/invalid-type}]
          [] nil nil
          ["foo"] "foo" nil
          [0] nil [{:path [0], :in [0], :schema string?, :value 0}
                   {:path [], :in [0], :schema s, :value 0, :type ::m/input-remaining}]
          ["foo" 0] nil [{:path [], :in [1], :schema s, :value 0, :type ::m/input-remaining}]))

      (testing "pathological case (terminates)"
        (let [n 50
              s (into [:cat] (concat (repeat n [:? [:= :a]])
                                     (repeat n [:= :a])))
              v (repeat n :a)]
          (is (m/validate s v))
          (is (= (concat (repeat n nil) v) (m/parse s v)))
          (is (= v (m/unparse s (vec (concat (repeat n nil) v))))))))

    (testing "*"
      (is (thrown? #?(:clj Exception, :cljs js/Error) (m/validator [:*])))
      (is (thrown? #?(:clj Exception, :cljs js/Error) (m/validator [:* string? int?])))

      (let [s [:* string?]]
        (are [v errs]
          (let [es errs]
            (and (= (m/validate s v) (nil? es))
                 (results= (m/explain s v) (and es {:schema s, :value v, :errors es}))
                 (= (m/parse s v) (if (nil? es) v ::m/invalid))
                 (or (some? es) (= (m/unparse s v) v))))

          0 [{:path [], :in [], :schema s, :value 0, :type ::m/invalid-type}]
          "foo" [{:path [], :in [], :schema s, :value "foo", :type ::m/invalid-type}]
          nil [{:path [], :in [], :schema s, :value nil, :type ::m/invalid-type}]
          [] nil
          ["foo"] nil
          [0] [{:path [0], :in [0], :schema string?, :value 0}
               {:path [], :in [0], :schema s, :value 0, :type ::m/input-remaining}]
          ["foo" 0] [{:path [0], :in [1], :schema string?, :value 0}
                     {:path [], :in [1], :schema s, :value 0, :type ::m/input-remaining}]
          ["foo" "bar"] nil)))

    (testing "+"
      (is (thrown? #?(:clj Exception, :cljs js/Error) (m/validator [:+])))
      (is (thrown? #?(:clj Exception, :cljs js/Error) (m/validator [:+ string? int?])))

      (testing "parse + unparse"
        (is (= [2 3] (->> (m/parse [:+ :int] [2 3]) (m/unparse [:+ :int])))))

      (let [s [:+ string?]]
        (are [v errs]
          (let [es errs]
            (and (= (m/validate s v) (nil? es))
                 (results= (m/explain s v) (and es {:schema s, :value v, :errors es}))
                 (= (m/parse s v) (if (nil? es) v ::m/invalid))
                 (or (some? es) (= (m/unparse s v) v))))

          0 [{:path [], :in [], :schema s, :value 0, :type ::m/invalid-type}]
          "foo" [{:path [], :in [], :schema s, :value "foo", :type ::m/invalid-type}]
          nil [{:path [], :in [], :schema s, :value nil, :type ::m/invalid-type}]
          [] [{:path [0], :in [0], :schema string?, :value nil, :type ::m/end-of-input}]
          ["foo"] nil
          [0] [{:path [0], :in [0], :schema string?, :value 0}]
          ["foo" 0] [{:path [0], :in [1], :schema string?, :value 0}
                     {:path [], :in [1], :schema s, :value 0, :type ::m/input-remaining}]
          ["foo" "bar"] nil)))

    (testing "repeat"
      (is (thrown? #?(:clj Exception, :cljs js/Error) (m/validator [:repeat {:min 1, :max 3}])))
      (is (thrown? #?(:clj Exception, :cljs js/Error) (m/validator [:repeat {:min 1, :max 3} string? int?])))

      (is (not (m/validate [:* [:repeat :int]] [[1 2 3] [4 5]])))
      (is (m/explain [:* [:repeat :int]] [[1 2 3] [4 5]]))
      (is (m/validate [:* [:repeat :int]] [1 2 3 4 5]))
      (is (nil? (m/explain [:* [:repeat :int]] [1 2 3 4 5])))
      (is (m/validate [:repeat :int] [1 2 3 4 5]))
      (is (nil? (m/explain [:repeat :int] [1 2 3 4 5])))
      (is (m/validate [:repeat [:repeat :int]] []))
      (is (m/validate [:repeat [:+ :int]] []))
      (is (nil? (m/explain [:repeat [:+ :int]] [])))
      (is (m/validate [:repeat :int] []))
      (is (nil? (m/explain [:repeat :int] [])))
      (is (m/validate [:repeat [:* :int]] []))
      (is (nil? (m/explain [:repeat [:* :int]] [])))
      (is (m/validate [:repeat {:min 0 :max 0} [:* :int]] []))
      (is (nil? (m/explain [:repeat {:min 0 :max 0} [:* :int]] [])))
      (is (m/validate [:repeat {:min 0 :max 1} [:* :int]] []))
      (is (nil? (m/explain [:repeat {:min 0 :max 1} [:* :int]] [])))
      (is (m/validate [:repeat {:min 1 :max 1} [:* :int]] []))
      (is (nil? (m/explain [:repeat {:min 1 :max 1} [:* :int]] [])))
      (is (m/validate [:repeat {:min 1} [:* :int]] []))
      (is (not (m/explain [:repeat {:min 1} [:* :int]] [])))
      (is (not (m/validate [:repeat {:min 1} :int] [])))
      (is (m/explain [:repeat {:min 1} :int] []))
      (is (nil? (m/explain [:repeat [:+ :int]] [])))
      (is (m/validate [:repeat {:min 1} [:repeat :int]] [1 2 3 4 5]))
      (is (nil? (m/explain [:repeat {:min 1} [:repeat :int]] [1 2 3 4 5])))
      (is (not (m/validate [:repeat {:min 1} [:repeat :int]] [1 2 3 4 5 nil])))
      (is (m/explain [:repeat {:min 1} [:repeat :int]] [1 2 3 4 5 nil]))
      (is (m/validate [:repeat {:min 1} [:repeat {:min 1} :int]] [1 2 3 4 5]))
      (is (m/validate [:repeat [:repeat {:min 1} :int]] [1 2 3 4 5]))
      (is (m/validate [:repeat {:min 0} [:repeat :int]] [1 2 3 4 5]))
      (is (m/validate [:repeat [:repeat :int]] [1 2 3 4 5]))
      (is (nil? (m/explain [:repeat [:repeat :int]] [1 2 3 4])))
      (is (m/explain [:repeat :int] [:a :b :c :d]))
      (is (m/explain [:repeat [:repeat :int]] [:a :b :c :d]))
      (is (not (m/validate [:repeat [:repeat :int]] [:a :b :c :d])))
      (is (nil? (m/explain [:repeat [:repeat [:repeat :int]]] [1 2 3 4])))
      (is (not (m/validate [:repeat [:repeat :int]] [1 2 3 4 5 nil])))
      (is (m/explain [:repeat [:repeat :int]] [1 2 3 4 5 nil]))
      (is (nil? (m/explain [:repeat :int] [1 2 3 4 5])))
      (is (m/validate [:repeat :int] [1 2 3 4 5]))
      (is (m/explain [:repeat :int] [1 2 3 4 nil]))
      (is (not (m/validate [:repeat [:repeat :int]] [[1 2 3] [4 5]])))
      (is (m/explain [:* [:repeat :int]] [[1 2 3] [4 5]]))
      (is (not (m/validate [:* [:repeat :int]] [[1 2 3] [4 5]])))
      (is (m/explain [:repeat [:* :int]] [[1 2 3] [4 5]]))
      (is (m/validate [:repeat [:sequential :int]] [[1 2 3] [4 5]]))
      (is (nil? (m/explain [:repeat [:sequential :int]] [[1 2 3] [4 5]])))
      (is (not (m/validate [:repeat [:repeat :int]] [[1 2 3] [4 5]])))
      (is (m/explain [:repeat [:repeat :int]] [[1 2 3] [4 5]]))
      (is (not (m/validate [:repeat :int] [1 nil])))
      (is (not (m/validate [:repeat [:repeat :int]] [1 nil])))
      (is (m/explain [:repeat [:repeat :int]] [1 nil]))
      (is (m/explain [:repeat {:min 1, :max 3} string?] ["foo" 0]))
      (let [s [:repeat {:min 1, :max 3} string?]]
        (are [?v ?errs]
          (let [es# ?errs]
            (and (is (= (m/validate s ?v) (nil? es#)))
                 (is (results= (m/explain s ?v) (and es# {:schema s, :value ?v, :errors es#})))
                 (is (= (m/parse s ?v) (if (nil? es#) ?v ::m/invalid)))
                 (is (or (some? es#) (= (m/unparse s ?v) ?v)))
                 true))

          0 [{:path [], :in [], :schema s, :value 0, :type ::m/invalid-type}]
          "foo" [{:path [], :in [], :schema s, :value "foo", :type ::m/invalid-type}]
          nil [{:path [], :in [], :schema s, :value nil, :type ::m/invalid-type}]
          [] [{:path [0], :in [0], :schema string?, :value nil, :type ::m/end-of-input}]
          ["foo"] nil
          [0] [{:path [0], :in [0], :schema string?, :value 0}]
          ["foo" 0] [{:path [0], :in [1], :schema string?, :value 0}
                     {:path [], :in [1], :schema s, :value 0, :type ::m/input-remaining}]
          ["foo" 0 1] [{:path [0], :in [1], :schema string?, :value 0}
                       {:path [], :in [1], :schema [:repeat {:min 1, :max 3} string?], :value 0, :type :malli.core/input-remaining}]
          ["foo" "bar"] nil
          ["foo" "bar" 0] [{:path [0], :in [2], :schema string?, :value 0}
                           {:path [], :in [2], :schema s, :value 0, :type ::m/input-remaining}]
          ["foo" "bar" "baz"] nil
          ["foo" 0 "bar" 1] [{:path [0], :in [1], :schema string?, :value 0}
                             {:path [], :in [1], :schema [:repeat {:min 1, :max 3} string?], :value 0, :type :malli.core/input-remaining}]
          ["foo" "bar" "baz" "quux"] [{:path [], :in [3], :schema s, :value "quux", :type ::m/input-remaining}])))

    (testing ":schema wrap"
      (is (thrown? #?(:clj Exception, :cljs js/Error) (m/validator [:schema])))
      (is (thrown? #?(:clj Exception, :cljs js/Error) (m/validator [:schema [:* string?] [:* int?]])))

      (let [s [:* [:schema [:* string?]]]]
        (are [v errs]
          (let [es errs]
            (and (= (m/validate s v) (nil? es))
                 (results= (m/explain s v) (and es {:schema s, :value v, :errors es}))
                 (= (m/parse s v) (if (nil? es) v ::m/invalid))
                 (or (some? es) (= (m/unparse s v) v))))

          0 [{:path [], :in [], :schema s, :value 0, :type ::m/invalid-type}]
          "foo" [{:path [], :in [], :schema s, :value "foo", :type ::m/invalid-type}]
          [] nil
          ["foo"] [{:path [0], :in [0], :schema [:* string?], :value "foo", :type ::m/invalid-type}
                   {:path [], :in [0], :schema s, :value "foo", :type ::m/input-remaining}]
          [[]] nil
          [["foo"]] nil
          [[0]] [{:path [0 0], :in [0 0], :schema string?, :value 0}
                 {:path [0], :in [0 0], :schema [:* string?], :value 0, :type ::m/input-remaining}
                 {:path [], :in [0], :schema s, :value [0], :type ::m/input-remaining}])))

    (testing "RefSchemas"
      (is (m/validate
           [:schema {:registry {"ints" [:+ int?]
                                "bools" [:+ boolean?]}}
            [:* [:cat "ints" "bools"]]]
           [1 true 2 2 false]))
      (is (thrown-with-msg? #?(:clj Exception, :cljs js/Error) #":malli.core/potentially-recursive-seqex"
                            (m/validator
                             [:schema {:registry {::ints [:cat int? [:ref ::ints]]}}
                              ::ints])))
      (is (m/validate
           [:schema {:registry {::ints [:* [:or int? [:ref ::ints]]]}}
            ::ints]
           [[1 2 3]]))
      ;; A bit undesirable, but intentional:
      (is (thrown-with-msg? #?(:clj Exception, :cljs js/Error) #":malli.core/potentially-recursive-seqex"
                            (m/validator
                             [:schema {:registry {::boll [:cat boolean?]}}
                              [:* [:ref ::boll]]]))))))

(deftest path-with-properties-test
  (let [?path #(-> % :errors first :path)]

    (is (= [0] (?path (m/explain [:and int?] "2"))))
    (is (= [0] (?path (m/explain [:and {:name "int?"} int?] "2"))))

    (is (= [0] (?path (m/explain [:vector int?] ["2"]))))
    (is (= [0] (?path (m/explain [:vector {:name "int?"} [int?]] ["2"]))))

    (is (= [0] (?path (m/explain [:tuple int?] ["2"]))))
    (is (= [0] (?path (m/explain [:tuple {:name "int?"} [int?]] ["2"]))))

    (is (= [:x] (?path (m/explain [:map [:x int?]] {:x "1"}))))
    (is (= [:x] (?path (m/explain [:map {:name int?} [:x int?]] {:x "1"}))))
    (is (= [:x] (?path (m/explain [:map {:name int?} [:x {:optional false} int?]] {:x "1"}))))))

(deftest properties-test
  (testing "properties can be set and retrieved"
    (let [properties {:title "kikka"}]
      (is (= properties
             (m/properties [:and properties int?])
             (m/properties [int? properties]))))
    (is (= nil
           (m/properties [:and {} int?])
           (m/properties [:and nil int?])
           (m/properties [:and int?])
           (m/properties [:enum {} 1 2 3])
           (m/properties [:enum nil 1 2 3])
           (m/properties [:enum 1 2 3])))))

(deftest children-test
  (testing "children can be set and retrieved"
    (let [schema1 (m/schema int?)
          schema2 (m/schema pos-int?)]
      (is (= [schema1 schema2]
             (m/children [:and {:a 1} schema1 schema2])
             (m/children [:and {} schema1 schema2])
             (m/children [:and schema1 schema2]))))))

(deftest options-test
  (testing "options can be set and retrieved"
    (let [opts {:tyyris "tyllero"
                :registry (m/default-schemas)}
          schema (m/into-schema 'int? {} nil opts)]
      (is (= opts
             (m/options schema))))))

(deftest round-trip-test
  (testing "schemas can be roundtripped"
    (let [schema (m/schema
                  [:map
                   [:x boolean?]
                   [:y {:optional true} int?]
                   [:z string?]])
          schema' (over-the-wire schema)
          valid {:x true, :y 1, :z "kikka"}]
      (is (= true
             (m/validate schema valid)
             (m/validate schema' valid)))
      (is (form= schema schema')))))

(deftest custom-registry-test
  (let [registry (merge
                  (m/comparator-schemas)
                  (m/base-schemas)
                  {"int" (m/-simple-schema {:type "int", :pred int?})
                   "string" (m/-simple-schema {:type "string", :pred string?})})]
    (is (true? (m/validate [:or "int" "string"] 123 {:registry registry})))
    (is (false? (m/validate [:or "int" "string"] 'kikka {:registry registry})))))

(deftest encode-decode-test
  (testing "works with custom registry"
    (let [opts {:registry (merge (m/default-schemas) {:test keyword?})}
          encoded (m/encode :test :foo opts mt/string-transformer)
          decoded (m/decode :test encoded opts mt/string-transformer)]
      (is (= "foo" encoded))
      (is (= :foo decoded)))))

(def sequential (m/-collection-schema {:type `sequential, :pred sequential?}))

(deftest custom-into-schema-test
  (doseq [value [[1 2 3] '(1 2 3)]]
    (is (= true (m/validate [sequential int?] value)))))

(deftest walker-in-test
  (is (form= [:map {:path []}
              [:id [string? {:path [:id]}]]
              [:tags [:set {:path [:tags]} [keyword? {:path [:tags ::m/in]}]]]
              [:address
               [:maybe {:path [:address]}
                [:vector {:path [:address 0]}
                 [:map {:path [:address 0 :malli.core/in]}
                  [:street [string? {:path [:address 0 ::m/in :street]}]]
                  [:lonlat
                   [:tuple {:path [:address 0 ::m/in :lonlat]}
                    [double? {:path [:address 0 ::m/in :lonlat 0]}]
                    [double? {:path [:address 0 ::m/in :lonlat 1]}]]]]]]]]

             (m/walk
              [:map
               [:id string?]
               [:tags [:set keyword?]]
               [:address
                [:maybe
                 [:vector
                  [:map
                   [:street string?]
                   [:lonlat [:tuple double? double?]]]]]]]
              (fn [schema path children options]
                (m/into-schema
                 (m/type schema)
                 (assoc (m/properties schema) :path path)
                 children
                 options))))))

(deftest custom-registry-qualified-keyword-in-map-test
  (let [schema [:map {:registry {:user/id int?
                                 "user/location" [:tuple :int :int]
                                 'user/country string?}}
                :user/id
                ["user/location"]
                [:name string?]
                ['user/country {:optional true}]]]

    (testing "Example with references + optional, regular key"
      (is (m/validate schema {:user/id 123 "user/location" [1 1] 'user/country "Finland" :name "Malli"})))

    (testing "Optional qualified keyword is optional"
      (is (m/validate schema {:user/id 123 "user/location" [1 1] :name "Malli"}))))

  (testing "invalid entry"
    (is (thrown-with-msg?
         #?(:clj Exception, :cljs js/Error) #":malli.core/invalid-entry"
         (m/schema [:map {:registry {:kikka :int}} :int])))))

(deftest simple-schemas
  (testing "simple schemas"
    (doseq [[type {:keys [schema validate explain decode encode ast form]}]
            {:any {:schema :any
                   :validate {:success [nil 1 "kikka"]}
                   :ast {:type :any}
                   :form :any}
             :some {:schema :some
                    :validate {:success [1 "kikka"]
                               :failure [nil]}
                    :ast {:type :some}
                    :form :some}
             :nil {:schema :nil
                   :validate {:success [nil], :failure [1 "kikka"]}
                   :ast {:type :nil}
                   :form :nil}
             :string {:schema [:string {:min 1, :max 4}]
                      :validate {:success ["abba" "a"]
                                 :failure [nil "invalid" "" 1]}
                      :explain [["abba"]
                                [false {:schema [:string {:min 1, :max 4}]
                                        :value false
                                        :errors [{:path []
                                                  :in []
                                                  :schema [:string {:min 1, :max 4}]
                                                  :value false}]}]]
                      :decode [["1" "1" mt/string-transformer]
                               ["1" "1" mt/json-transformer]
                               ["--" "<-->" mt/string-transformer [:string {:decode/string {:enter #(str "<" %), :leave #(str % ">")}}]]]
                      :encode [["1" "1" mt/string-transformer]
                               ["1" "1" mt/json-transformer]
                               ["--" "<-->" mt/string-transformer [:string {:encode/string {:enter #(str "<" %), :leave #(str % ">")}}]]]
                      :ast {:type :string, :properties {:min 1, :max 4}}
                      :form [:string {:min 1, :max 4}]}
             :int {:schema [:int {:min 1, :max 4}]
                   :validate {:success [1 4]
                              :failure [nil "invalid" 5]}
                   :explain [[1]
                             [false {:schema [:int {:min 1, :max 4}]
                                     :value false
                                     :errors [{:path []
                                               :in []
                                               :schema [:int {:min 1, :max 4}]
                                               :value false}]}]]
                   :decode [["1" 1 mt/string-transformer]
                            ["1" "1" mt/json-transformer]
                            [1 3 mt/string-transformer [:int {:decode/string {:enter inc, :leave inc}}]]]
                   :encode [[1 "1" mt/string-transformer]
                            [1 1 mt/json-transformer]
                            [1 3 mt/string-transformer [:int {:encode/string {:enter inc, :leave inc}}]]]
                   :ast {:type :int, :properties {:min 1, :max 4}}
                   :form [:int {:min 1, :max 4}]}
             :float {:schema [:float {:min 1.0, :max 4.0}]
                     :validate {:success [1.0 2.2 4.0]
                                :failure [nil "invalid" 0.5]}
                     :explain [[1.0]
                               [false {:schema [:float {:min 1.0, :max 4.0}]
                                       :value false
                                       :errors [{:path []
                                                 :in []
                                                 :schema [:float {:min 1.0, :max 4.0}]
                                                 :value false}]}]]
                     :decode [["1.1" (float 1.1) mt/string-transformer]
                              ["1.1" "1.1" mt/json-transformer]
                              [(float 1.2) (float 3.2) mt/string-transformer [:float {:decode/string {:enter inc, :leave inc}}]]]
                     :encode [[(float 1.1) "1.1" mt/string-transformer]
                              [(float 1.1) (float 1.1) mt/json-transformer]
                              [(float 1.2) (float 3.2) mt/string-transformer [:float {:encode/string {:enter inc, :leave inc}}]]]
                     :ast {:type :float, :properties {:min 1.0, :max 4.0}}
                     :form [:float {:min 1.0, :max 4.0}]}
             :double {:schema [:double {:min 1.0, :max 4.0}]
                      :validate {:success [1.0 2.2 4.0]
                                 :failure [nil "invalid" 0.5]}
                      :explain [[1.0]
                                [false {:schema [:double {:min 1.0, :max 4.0}]
                                        :value false
                                        :errors [{:path []
                                                  :in []
                                                  :schema [:double {:min 1.0, :max 4.0}]
                                                  :value false}]}]]
                      :decode [["1.1" 1.1 mt/string-transformer]
                               ["1.1" "1.1" mt/json-transformer]
                               [1.1 3.1 mt/string-transformer [:double {:decode/string {:enter inc, :leave inc}}]]]
                      :encode [[1.1 "1.1" mt/string-transformer]
                               [1.1 1.1 mt/json-transformer]
                               [1.1 3.1 mt/string-transformer [:double {:encode/string {:enter inc, :leave inc}}]]]
                      :ast {:type :double, :properties {:min 1.0, :max 4.0}}
                      :form [:double {:min 1.0, :max 4.0}]}
             :keyword {:schema :keyword
                       :validate {:success [:abba :user/abba]
                                  :failure [nil "invalid"]}
                       :explain [[:abba]
                                 [false {:schema :keyword
                                         :value false
                                         :errors [{:path []
                                                   :in []
                                                   :schema :keyword
                                                   :value false}]}]]
                       :decode [["abba" :abba mt/string-transformer]
                                ["user/abba" :user/abba mt/string-transformer]
                                ["abba" :abba mt/json-transformer]
                                ["user/abba" :user/abba mt/json-transformer]
                                ["abba" :user/abba mt/string-transformer [:keyword {:decode/string {:enter (partial str "user/"), :leave keyword}}]]]
                       :encode [[:abba "abba" mt/string-transformer]
                                [:user/abba "user/abba" mt/string-transformer]
                                [:abba "abba" mt/json-transformer]
                                [:user/abba "user/abba" mt/json-transformer]
                                [:user/abba "abba" mt/string-transformer [:keyword {:encode/string {:enter name, :leave str}}]]]
                       :ast {:type :keyword}
                       :form :keyword}
             :qualified-keyword {:schema [:qualified-keyword {:namespace :user}]
                                 :validate {:success [:user/abba]
                                            :failure [:abba :zzz/abba nil "invalid"]}
                                 :explain [[:user/abba]
                                           [false {:schema [:qualified-keyword {:namespace :user}]
                                                   :value false
                                                   :errors [{:path []
                                                             :in []
                                                             :schema [:qualified-keyword {:namespace :user}]
                                                             :value false}]}]]
                                 :decode [["abba" :abba mt/string-transformer]
                                          ["user/abba" :user/abba mt/string-transformer]
                                          ["abba" :abba mt/json-transformer]
                                          ["user/abba" :user/abba mt/json-transformer]
                                          ["abba" :user/abba mt/string-transformer [:qualified-keyword {:decode/string {:enter (partial str "user/"), :leave keyword}}]]]
                                 :encode [[:abba "abba" mt/string-transformer]
                                          [:user/abba "user/abba" mt/string-transformer]
                                          [:abba "abba" mt/json-transformer]
                                          [:user/abba "user/abba" mt/json-transformer]
                                          [:user/abba "abba" mt/string-transformer [:qualified-keyword {:encode/string {:enter name, :leave str}}]]]
                                 :ast {:type :qualified-keyword, :properties {:namespace :user}}
                                 :form [:qualified-keyword {:namespace :user}]}
             :symbol {:schema :symbol
                      :validate {:success ['abba 'user/abba]
                                 :failure [nil "invalid"]}
                      :explain [['abba]
                                [false {:schema :symbol
                                        :value false
                                        :errors [{:path []
                                                  :in []
                                                  :schema :symbol
                                                  :value false}]}]]
                      :decode [["abba" 'abba mt/string-transformer]
                               ["user/abba" 'user/abba mt/string-transformer]
                               ["abba" 'abba mt/json-transformer]
                               ["user/abba" 'user/abba mt/json-transformer]
                               ["abba" 'user/abba mt/string-transformer [:symbol {:decode/string {:enter (partial str "user/"), :leave symbol}}]]]
                      :encode [['abba "abba" mt/string-transformer]
                               ['user/abba "user/abba" mt/string-transformer]
                               ['abba "abba" mt/json-transformer]
                               ['user/abba "user/abba" mt/json-transformer]
                               ['user/abba "abba" mt/string-transformer [:symbol {:encode/string {:enter name, :leave str}}]]]
                      :ast {:type :symbol}
                      :form :symbol}
             :qualified-symbol {:schema :qualified-symbol
                                :validate {:success ['user/abba]
                                           :failure ['abba nil "invalid"]}
                                :explain [['user/abba]
                                          [false {:schema :qualified-symbol
                                                  :value false
                                                  :errors [{:path []
                                                            :in []
                                                            :schema :qualified-symbol
                                                            :value false}]}]]
                                :decode [["abba" 'abba mt/string-transformer]
                                         ["user/abba" 'user/abba mt/string-transformer]
                                         ["abba" 'abba mt/json-transformer]
                                         ["user/abba" 'user/abba mt/json-transformer]
                                         ["abba" 'user/abba mt/string-transformer [:qualified-symbol {:decode/string {:enter (partial str "user/"), :leave symbol}}]]]
                                :encode [['abba "abba" mt/string-transformer]
                                         ['user/abba "user/abba" mt/string-transformer]
                                         ['abba "abba" mt/json-transformer]
                                         ['user/abba "user/abba" mt/json-transformer]
                                         ['user/abba "abba" mt/string-transformer [:qualified-symbol {:encode/string {:enter name, :leave str}}]]]
                                :ast {:type :qualified-symbol}
                                :form :qualified-symbol}
             :uuid {:schema :uuid
                    :validate {:success [#uuid"72b9bf3d-398c-472f-9360-c1a997c22240"]
                               :failure ["72b9bf3d-398c-472f-9360-c1a997c22240" nil 123]}
                    :explain [[#uuid"72b9bf3d-398c-472f-9360-c1a997c22240"]
                              [false {:schema :uuid
                                      :value false
                                      :errors [{:path []
                                                :in []
                                                :schema :uuid
                                                :value false}]}]]
                    :decode [["72b9bf3d-398c-472f-9360-c1a997c22240" #uuid"72b9bf3d-398c-472f-9360-c1a997c22240" mt/string-transformer]
                             ["abba" "abba" mt/string-transformer]
                             ["abba" "abba" mt/json-transformer]
                             [123 123 mt/json-transformer]
                             ["9360-c1a997c22240" #uuid"72b9bf3d-398c-472f-9360-c1a997c22240" mt/string-transformer [:uuid {:decode/string {:enter (partial str "72b9bf3d-398c-472f-"), :leave mt/-string->uuid}}]]]
                    :encode [[#uuid"72b9bf3d-398c-472f-9360-c1a997c22240" "72b9bf3d-398c-472f-9360-c1a997c22240" mt/string-transformer]
                             ["abba" "abba" mt/string-transformer]
                             ["abba" "abba" mt/json-transformer]
                             [123 "123" mt/json-transformer]
                             [#uuid"72b9bf3d-398c-472f-9360-c1a997c22240" "72b9bf3d-398c-472f-9360-c1a997c22240" mt/string-transformer [:uuid {:decode/string {:enter (partial str "72b9bf3d-398c-472f-"), :leave mt/-string->uuid}}]]]
                    :ast {:type :uuid}
                    :form :uuid}}]

      (testing (str "simple-schema: " type)

        (testing "successful validation"
          (doseq [x (:success validate)]
            (is (true? (m/validate schema x)))))

        (testing "failing validation"
          (doseq [x (:failure validate)]
            (is (false? (m/validate schema x)))))

        (testing "explain"
          (doseq [[value expected] explain]
            (is (results= expected (m/explain schema value)))))

        (testing "decoding"
          (doseq [[value expected transformer ?schema] decode]
            (is (= expected (m/decode (or ?schema schema) value transformer)))))

        (testing "encoding"
          (doseq [[value expected transformer ?schema] encode]
            (is (= expected (m/encode (or ?schema schema) value transformer)))))

        (testing "over-the-wire"
          (doseq [x (:success validate)]
            (is (true? (m/validate (over-the-wire schema) x)))))

        (testing "form"
          (is (= form (m/form schema))))

        (testing "ast"
          (is (= ast (m/ast schema))))))))

(def generate-over6 (gen/large-integer* {:min 7}))

(def Over6
  (m/-simple-schema
   {:type :user/over6
    :pred #(and (int? %) (> % 6))
    :type-properties {:error/message "should be over 6"
                      :decode/string mt/-string->long
                      :json-schema/type "integer"
                      :json-schema/format "int64"
                      :json-schema/minimum 6
                      :gen/gen generate-over6}}))

(deftest custom-simple-type-test

  (testing "can be walked"
    (is (m/walk Over6 (m/schema-walker identity))))

  (testing "with static type-properties"
    (let [over6 (m/schema [Over6 {:json-schema/example 42}])]
      (testing "form"
        (is (= [:user/over6 {:json-schema/example 42}] (m/form over6))))
      (testing "validation"
        (is (false? (m/validate over6 6)))
        (is (true? (m/validate over6 7))))
      (testing "properties"
        (is (= {:error/message "should be over 6"
                :decode/string mt/-string->long
                :json-schema/type "integer"
                :json-schema/format "int64"
                :json-schema/minimum 6
                :gen/gen generate-over6}
               (m/type-properties over6)))
        (is (= {:json-schema/example 42}
               (m/properties over6))))))

  (testing "with instance-based type-properties"
    (let [Over (m/-simple-schema
                {:type :user/over
                 :compile (fn [{:keys [value]} _ _]
                            (assert (int? value))
                            {:pred #(and (int? %) (> % value))
                             :type-properties {:error/message (str "should be over " value)
                                               :decode/string mt/-string->long
                                               :json-schema/type "integer"
                                               :json-schema/format "int64"
                                               :json-schema/minimum value}})})]

      (testing "over6"
        (let [schema [Over {:value 6}]]
          (testing "type"
            (is (= :user/over (m/-type Over) (m/type schema))))
          (testing "form"
            (is (= [:user/over {:value 6}] (m/form schema))))
          (testing "validation"
            (is (false? (m/validate schema 6)))
            (is (true? (m/validate schema 7))))
          (testing "properties"
            (is (= {:error/message "should be over 6"
                    :decode/string mt/-string->long
                    :json-schema/type "integer"
                    :json-schema/format "int64"
                    :json-schema/minimum 6}
                   (m/type-properties schema)))
            (is (= {:value 6}
                   (m/properties schema))))))

      (testing "over42"
        (let [schema [Over {:value 42}]]
          (testing "form"
            (is (= [:user/over {:value 42}] (m/form schema))))
          (testing "validation"
            (is (false? (m/validate schema 42)))
            (is (true? (m/validate schema 43))))
          (testing "properties"
            (is (= {:error/message "should be over 42"
                    :decode/string mt/-string->long
                    :json-schema/type "integer"
                    :json-schema/format "int64"
                    :json-schema/minimum 42}
                   (m/type-properties schema)))
            (is (= {:value 42}
                   (m/properties schema)))))))))

(deftest parent-test
  (testing "registered schemas"
    (is (= (mr/-schema m/default-registry :string) (m/parent [:string {:min 0}]))))
  (testing "non-registered schemas"
    (is (= Over6 (m/parent [Over6 {:json-schema/example 42}])))))

(deftest -regex-min-max-size-test
  (are [s min-max]
    (= min-max ((juxt :min :max) (m/-regex-min-max (m/schema s) false)))

    int? [1 1]
    [:cat] [0 0]
    [:cat int?] [1 1]
    [:cat int? [:cat]] [1 1]
    [:cat int? [:cat string? int?]] [3 3]
    [:schema [:cat int? [:cat string? int?]]] [3 3]
    [::m/schema [:cat int? [:cat string? int?]]] [3 3]
    [:cat int? [:schema [:cat string? int?]]] [2 2]
    [:cat int? [::m/schema [:cat string? int?]]] [3 3]
    [:cat int? [:schema [:catn [:s string?] [:i int?]]]] [2 2]
    [:catn] [0 0]
    [:catn [:n int?]] [1 1]
    [:catn [:n int?] [:named [:cat]]] [1 1]
    [:catn [:n int?] [:named [:cat string? int?]]] [3 3]
    [:alt int?] [1 1]
    [:alt int? [:cat]] [0 1]
    [:altn [:n int?]] [1 1]
    [:altn [:n int?] [:empty [:cat]]] [0 1]
    [:* int?] [0 nil]
    [:? int?] [0 1]
    [:? [:cat int? int?]] [0 2]
    [:? [:schema [:cat int? int?]]] [0 1]
    [:? [::m/schema [:cat int? int?]]] [0 2]
    [:+ [:cat string? int?]] [2 nil]
    [:+ [:schema [:cat string? int?]]] [1 nil]
    [:+ [::m/schema [:cat string? int?]]] [2 nil]
    [:+ [:? int?]] [0 nil]
    [:repeat {:min 5, :max 15} [:cat string? int?]] [10 30]
    [:repeat {:min 5, :max 15} [:* int?]] [0 nil]
    [:cat {:registry {:named [:cat string? int?]}} :named] [2 2]
    [:cat {:registry {:named [:cat string? int?]}} [:repeat {:min 5 :max 15} :named]] [10 30]
    [:cat {:registry {:named [:cat string? int?]}} [:repeat {:min 5 :max 15} [:schema :named]]] [5 15])

  (is (thrown-with-msg? #?(:clj Exception, :cljs js/Error) #":malli.core/potentially-recursive-seqex"
                        (m/-regex-min-max
                         (m/schema [:cat {:registry {::ints [:cat int? [:ref ::ints]]}}
                                    ::ints])
                         false))))

(defn single-arity
  ([x] x)
  ([_ _] (m/-fail! ::arity-error)))

(defn validate-times
  "Validate value n times while validation returns `true`,
  and return the final result."
  [n schema v]
  {:pre [(pos? n)]}
  (let [res (m/validate schema v)]
    (if (or (= 1 n) (not (true? res)))
      res
      (recur (dec n) schema v))))

(defn explain-times
  "Explain value n times while explain returns `nil`,
  and return the final result."
  [n schema v]
  {:pre [(pos? n)]}
  (let [res (m/explain schema v)]
    (if (or (= 1 n) (not (nil? res)))
      res
      (recur (dec n) schema v))))

(def function-schema-validation-times
  "Number of times to test a successful generative test involving :=>."
  1000)

(defn fn-schema-info [?schema]
  (some-> (m/schema ?schema)
          (m/-function-info)
          (update :input m/form)
          (update :output m/form)
          (update :guard #(some-> % m/form))))

;; js allows invalid arity
(deftest function-schema-test

  (is (= {:min 2
          :max 2
          :arity 2
          :input [:cat :int :int]
          :output :int
          :guard nil}
         (fn-schema-info [:=> [:cat :int :int] :int])
         (fn-schema-info [:-> :int :int :int])))

  (testing ":=>"
    (let [?schema [:=> [:cat int? int?] int?]
          valid-f (fn [x y] (unchecked-subtract x y))
          schema1 (m/schema ?schema)
          schema2 (m/schema ?schema {::m/function-checker mg/function-checker})]

      (testing "by default, all ifn? are valid"
        (is (true? (m/validate schema1 identity)))
        (is (true? (m/validate schema1 #{}))))

      (testing "using generative testing"
        (is (false? (m/validate schema2 single-arity)))
        #?(:clj (is (false? (m/validate schema2 (fn [x] x)))))
        #?(:clj (is (false? (m/validate schema2 #{}))))
        (is (true? (validate-times function-schema-validation-times schema2 valid-f)))
        (is (false? (m/validate schema2 (fn [x y] (str x y)))))

        (is (nil? (explain-times function-schema-validation-times schema2 valid-f)))

        (testing "exception in execution causes single error to root schema path"
          (is (results= {:schema ?schema
                         :value single-arity
                         :errors [{:path []
                                   :in []
                                   :schema [:=> [:cat int? int?] int?]
                                   :value single-arity}]}
                        (m/explain schema2 single-arity))))

        (testing "error in output adds error to child in path 1"
          (let [f (fn [x y] (str x y))]
            (is (results= {:schema ?schema
                           :value f
                           :errors [{:path []
                                     :in []
                                     :schema [:=> [:cat int? int?] int?]
                                     :value f}
                                    {:path [1]
                                     :in []
                                     :schema int?
                                     :value "00"}]}
                          (m/explain schema2 f)))))

        (is (= single-arity (m/decode schema2 single-arity mt/string-transformer)))

        (is (true? (validate-times function-schema-validation-times (over-the-wire schema1) valid-f)))

        (is (= {:type :=>
                :input {:type :cat
                        :children [{:type 'int?} {:type 'int?}]}
                :output {:type 'int?}}
               (m/ast schema1))))))

  (testing ":->"
    (let [?schema [:-> int? int? int?]
          valid-f (fn [x y] (unchecked-subtract x y))
          schema1 (m/schema ?schema)
          schema2 (m/schema ?schema {::m/function-checker mg/function-checker})]

      (testing "by default, all ifn? are valid"
        (is (true? (m/validate schema1 identity)))
        (is (true? (m/validate schema1 #{}))))

      (testing "using generative testing"
        (is (false? (m/validate schema2 single-arity)))
        #?(:clj (is (false? (m/validate schema2 (fn [x] x)))))
        #?(:clj (is (false? (m/validate schema2 #{}))))
        (is (true? (validate-times function-schema-validation-times schema2 valid-f)))
        (is (false? (m/validate schema2 (fn [x y] (str x y)))))

        (is (nil? (explain-times function-schema-validation-times schema2 valid-f)))

        (testing "exception in execution causes single error to root schema path"
          (is (results= {:schema ?schema
                         :value single-arity
                         :errors [{:path [::m/in]
                                   :in []
                                   :schema [:=> [:cat int? int?] int?]
                                   :value single-arity}]}
                        (m/explain schema2 single-arity))))

        (testing "error in output adds error to child in path 1"
          (let [f (fn [x y] (str x y))]
            (is (results= {:schema ?schema
                           :value f
                           :errors [{:path [::m/in]
                                     :in []
                                     :schema [:=> [:cat int? int?] int?]
                                     :value f}
                                    {:path [::m/in 1]
                                     :in []
                                     :schema int?
                                     :value "00"}]}
                          (m/explain schema2 f)))))

        (is (= single-arity (m/decode schema2 single-arity mt/string-transformer)))

        (is (true? (validate-times function-schema-validation-times (over-the-wire schema1) valid-f)))

        (is (= {:type :->
                :children [{:type 'int?} {:type 'int?} {:type 'int?}]}
               (m/ast schema1))))))

  (testing ":function"
    (is (= nil
           (fn-schema-info (m/schema [:function [:-> :int :int :int]]))))
    (is (= [[:-> :int :int :int]
            [:=> [:cat :int] :int]]
           (map m/form (m/-function-schema-arities (m/schema [:function
                                                              [:-> :int :int :int]
                                                              [:=> [:cat :int] :int]])))))
    (doseq [s [[:function :cat]]]
      (is (thrown-with-msg?
           #?(:clj Exception, :cljs js/Error)
           #":malli.core/non-function-childs"
           (m/schema s))))

    (testing "varargs with identical min arity get +1 arity"
      (is (m/schema
           [:function
            [:=> :cat nil?]
            [:=> [:cat [:? nil?]] nil?]])))

    (testing "invalid arities"

      (doseq [s [[:function
                  [:=> :cat nil?]
                  [:=> :cat nil?]]
                 [:function
                  [:-> nil?]
                  [:=> :cat nil?]]]]
        (is (thrown-with-msg?
             #?(:clj Exception, :cljs js/Error)
             #":malli.core/duplicate-arities"
             (m/schema s))))

      (doseq [s [[:function
                  [:=> :cat nil?]
                  [:=> [:cat [:? nil?]] nil?]
                  [:=> [:cat [:? nil?] [:? nil?]] nil?]]]]
        (is (thrown-with-msg?
             #?(:clj Exception, :cljs js/Error)
             #":malli.core/multiple-varargs"
             (m/schema s)))))

    (doseq [?schema [[:function
                      [:=> [:cat int?] int?]
                      [:=> [:cat int? int?] int?]]]]
      (let [valid-f (fn ([x] x) ([x y] (unchecked-subtract x y)))
            invalid-f (fn ([x] x) ([x y] (str x y)))

            schema1 (m/schema ?schema)
            schema2 (m/schema ?schema {::m/function-checker mg/function-checker})]

        (testing "by default, all ifn? are valid"
          (is (true? (m/validate schema1 identity)))
          (is (true? (m/validate schema1 #{}))))

        (testing "using generative testing"
          #?(:clj (is (false? (m/validate schema2 identity))))
          (is (false? (m/validate schema2 #{})))

          (is (false? (m/validate schema2 single-arity)))
          #?(:clj (is (false? (m/validate schema2 (fn [x] x)))))
          #?(:clj (is (false? (m/validate schema2 #{}))))
          (is (true? (validate-times function-schema-validation-times schema2 valid-f)))
          (is (false? (m/validate schema2 (fn [x y] (str x y)))))

          (is (nil? (explain-times function-schema-validation-times schema2 valid-f)))

          (is (results= {:schema schema2
                         :value invalid-f
                         :errors [{:path []
                                   :in []
                                   :schema [:function
                                            [:=> [:cat int?] int?]
                                            [:=> [:cat int? int?] int?]]
                                   :value invalid-f}]}
                        (m/explain schema2 invalid-f))))

        (testing "guards"
          (let [guard (fn [[[x y] z]] (= (str x y) z))
                schema1 (m/schema
                         [:=> [:cat :int :int] :string [:fn guard]]
                         {::m/function-checker mg/function-checker})
                schema2 (m/schema
                         [:-> {:guard guard} :int :int :string]
                         {::m/function-checker mg/function-checker})
                valid (fn [x y] (str x y))
                invalid (fn [x y] (str x "-" y))]

            (is (= {:type :=>,
                    :input {:type :cat
                            :children [{:type :int} {:type :int}]},
                    :output {:type :string},
                    :guard {:type :fn
                            :value guard}}
                   (m/ast schema1)))

            (is (= {:type :->
                    :children [{:type :int} {:type :int} {:type :string}]
                    :properties {:guard guard}}
                   (m/ast schema2)))

            (is (= nil (m/explain schema1 valid)))
            (is (= nil (m/explain schema2 valid)))

            (testing "error in guard adds error on path 2"
              (is (results= {:schema schema1,
                             :value invalid
                             :errors [{:path [],
                                       :in [],
                                       :schema schema1
                                       :value invalid}
                                      {:path [2]
                                       :in []
                                       :schema [:fn guard]
                                       :value ['(0 0) "0-0"]}]}
                            (m/explain schema1 invalid)))
              (is (results= {:schema schema2,
                             :value invalid
                             :errors [{:path [::m/in],
                                       :in [],
                                       :schema schema1
                                       :value invalid}
                                      {:path [::m/in 2]
                                       :in []
                                       :schema [:fn guard]
                                       :value ['(0 0) "0-0"]}]}
                            (m/explain schema2 invalid))))

            (testing "instrument"
              (doseq [schema [[:=> [:cat :any] :any [:fn (fn [[[arg] ret]] (not= arg ret))]]
                              [:-> {:guard (fn [[[arg] ret]] (not= arg ret))} :any :any]]]
                (let [fn (m/-instrument {:schema schema} str)]

                  (is (= "2" (fn 2)))

                  (is (thrown-with-msg?
                       #?(:clj Exception, :cljs js/Error)
                       #":malli.core/invalid-guard"
                       (fn "0"))))))))

        (testing "non-accumulating errors"
          (let [schema (m/schema
                        [:tuple :int [:function [:=> [:cat :int] :int]]]
                        {::m/function-checker malli.generator/function-checker})
                f (fn [_] 1)]
            (is (results= {:schema schema,
                           :value ["1" f],
                           :errors [{:path [0], :in [0], :schema :int, :value "1"}]}
                          (m/explain schema ["1" f])))))

        (is (= valid-f (m/decode schema1 valid-f mt/string-transformer)))

        (is (true? (m/validate (over-the-wire schema1) valid-f)))))))

(deftest test-415
  (testing "multi default is not transformed"
    (let [transformer (mt/key-transformer
                       {:encode (comp keyword str/upper-case name)})
          schema [:multi {:dispatch :foo-bar}
                  [:bar [:map [:foo-bar keyword?]]]
                  [::m/default [:map [:foo-bar keyword?]]]]]

      (is (= {:FOO-BAR "bar"} (m/encode schema {:foo-bar "bar"} transformer)))
      (is (= {:FOO-BAR "baz"} (m/encode schema {:foo-bar "baz"} transformer))))))

(deftest custom-collection-test
  (let [List (m/-collection-schema
              {:type :list
               :compile (fn [properties [child] _options]
                          {:pred list?
                           :empty '()
                           :type-properties {:error/message "should be a list"
                                             :gen/schema [:vector properties child]
                                             :gen/fmap #(or (list* %) '())}})})]
    (is (m/validate [List :int] '(1 2)))
    (is (not (m/validate [List :int] [1 2])))
    (is (= :list (m/-type List)))
    (is (= :list (m/type [List :int])))))

(defn function-schema-registry-test-fn [])

(deftest function-schema-registry-test
  (swap! @#'m/-function-schemas* update :clj dissoc 'malli.core-test)
  (let [prior-function-schemas (m/function-schemas #?(:cljs :cljs :clj :clj))
        _ (m/=> function-schema-registry-test-fn [:=> :cat :nil])
        new-function-schemas (m/function-schemas #?(:cljs :cljs :clj :clj))
        this-ns-schemas (get new-function-schemas 'malli.core-test)
        fn-schema (get this-ns-schemas 'function-schema-registry-test-fn)]
    (is (= (inc (count prior-function-schemas)) (count new-function-schemas)))
    (is (map? this-ns-schemas))
    (is (map? fn-schema))))

(deftest -instrument-test
  (let [int<=6 [:int {:max 6}]]

    (testing "single-arity, with defaults"
      (let [pow2 (m/-instrument {:schema [:=> [:cat :int] int<=6]} (fn [x] (* x x)))]
        (is (= 4 (pow2 2)))
        (is (thrown-with-msg?
             #?(:clj Exception, :cljs js/Error)
             #":malli.core/invalid-input"
             (pow2 "2")))
        (is (thrown-with-msg?
             #?(:clj Exception, :cljs js/Error)
             #":malli.core/invalid-output"
             (pow2 4)))
        (is (thrown-with-msg?
             #?(:clj Exception, :cljs js/Error)
             #":malli.core/invalid-arity"
             (pow2 4 2)))))

    (testing "multi-arity, with options"
      (let [report* (atom [])
            <-report #(let [report @report*] (reset! report* []) report)
            pow2 (m/-instrument
                  {:schema [:function
                            [:=> [:cat :int] int<=6]
                            [:=> [:cat :int :int] int<=6]]
                   :scope #{:input :output}
                   :report (fn [error _] (swap! report* conj error))}
                  (fn
                    ([x] (* x x))
                    ([x y] (* x y))))]
        (is (= 4 (pow2 2)))

        (is (= 16 (pow2 4)))
        (is (= [::m/invalid-output] (<-report)))

        (is (= 0.5 (pow2 5 0.1)))
        (is (= [::m/invalid-input ::m/invalid-output] (<-report)))))

    (testing "multi-arity, with sequence schemas"
      (let [report* (atom [])
            <-report #(let [report @report*] (reset! report* []) report)
            fun (m/-instrument
                 {:schema [:=> [:cat :int [:schema [:cat :keyword :int]] [:? [:cat :int :int]]] int<=6]
                  :scope #{:input :output}
                  :report (fn [error _] (swap! report* conj error))}
                 (fn [x & _] x))]
        (is (= 1 (fun 1 [:x 3])))
        (is (= [] (<-report)))
        (is (= 1 (fun 1 [:x 3] 4 5)))
        (is (= [] (<-report)))
        (is (= 16 (fun 16 [:x 3])))
        (is (= [::m/invalid-output] (<-report)))
        (testing "between min and max arity but invalid"
          (is (= 16 (fun 16 [:x 3] 4)))
          (is (= [::m/invalid-input ::m/invalid-output] (<-report))))
        (testing "over max arity"
          (is (= 1 (fun 1 [:x 3] 4 5 6)))
          (is (= [::m/invalid-arity ::m/invalid-input] (<-report))))
        (testing "under min arity"
          (is (= 1 (fun 1)))
          (is (= [::m/invalid-arity ::m/invalid-input] (<-report))))
        (testing "invalid sub-sequence"
          (is (= 1 (fun 1 [2 3] 4 5)))
          (is (= [::m/invalid-input] (<-report)))
          (is (= 1 (fun 1 [:x 2 3] 4 5)))
          (is (= [::m/invalid-input] (<-report))))))

    (testing "generated function"
      (let [pow2 (m/-instrument
                  {:schema [:function
                            [:=> [:cat :int] int<=6]
                            [:=> [:cat :int :int] int<=6]]
                   :gen mg/generate})]
        (is (m/validate int<=6 (pow2 100)))
        (is (m/validate int<=6 (pow2 100 100)))
        (is (thrown-with-msg?
             #?(:clj Exception, :cljs js/Error)
             #":malli.core/invalid-arity"
             (pow2 100 100 100)))))))

(deftest -safe-pred-test
  (is (true? ((m/-safe-pred (constantly "true")) ::any)))
  (is (false? ((m/-safe-pred (constantly nil)) ::any)))
  (is (true? (m/validate [:fn (constantly "true")] ::any)))
  (is (false? (m/validate [:fn (constantly nil)] ::any))))

(deftest validate-limits
  (testing "Upper and lower bound"
    (let [f (m/-validate-limits 3 7)]
      (is (false? (f (range 2))))
      (is (true? (f (range 3))))
      (is (true? (f (range 4))))
      (is (true? (f (range 7))))
      (is (false? (f (range 8))))))
  (testing "Upper bound, no lower bound"
    (let [f (m/-validate-limits nil 7)]
      (is (true? (f (range 2))))
      (is (true? (f (range 3))))
      (is (true? (f (range 4))))
      (is (true? (f (range 7))))
      (is (false? (f (range 8))))))
  (testing "Lower bound, no upper bounds"
    (let [f (m/-validate-limits 3 nil)]
      (is (false? (f (range 2))))
      (is (true? (f (range 3))))
      (is (true? (f (range 4))))
      (is (true? (f (range 7))))
      (is (true? (f (range 8))))))
  (testing "No bounds"
    (let [f (m/-validate-limits nil nil)]
      (is (true? (f (range 2))))
      (is (true? (f (range 3))))
      (is (true? (f (range 4))))
      (is (true? (f (range 7))))
      (is (true? (f (range 8)))))))

(deftest ast-test
  (doseq [{:keys [name hiccup ast]}
          [{:name "recursion"
            :hiccup [:ref {:registry {"ConsCell" [:maybe [:tuple :int [:ref "ConsCell"]]]}}
                     "ConsCell"]
            :ast {:type :ref
                  :value "ConsCell"
                  :registry {"ConsCell" {:type :maybe
                                         :child {:type :tuple
                                                 :children [{:type :int}
                                                            {:type :ref
                                                             :value "ConsCell"}]}}}}}
           {:name "hiccup"
            :hiccup [:schema
                     {:registry {"hiccup" [:orn
                                           [:node
                                            [:catn
                                             [:name 'keyword?]
                                             [:props [:? [:map-of 'keyword? 'any?]]]
                                             [:children [:* [:schema [:ref "hiccup"]]]]]]
                                           [:primitive
                                            [:orn
                                             [:nil 'nil?]
                                             [:boolean 'boolean?]
                                             [:number 'number?]
                                             [:text 'string?]]]]}}
                     "hiccup"]
            :ast {:type :schema
                  :child {:type ::m/schema
                          :value "hiccup"}
                  :registry {"hiccup" {:type :orn
                                       :keys {:node {:order 0
                                                     :value {:type :catn
                                                             :keys {:name {:order 0
                                                                           :value {:type 'keyword?}}
                                                                    :props {:order 1
                                                                            :value {:type :?
                                                                                    :children [{:type :map-of
                                                                                                :key {:type 'keyword?}
                                                                                                :value {:type 'any?}}]}}
                                                                    :children {:order 2
                                                                               :value {:type :*
                                                                                       :children [{:type :schema
                                                                                                   :child {:type :ref
                                                                                                           :value "hiccup"}}]}}}}}
                                              :primitive {:order 1
                                                          :value {:type :orn
                                                                  :keys {:nil {:order 0
                                                                               :value {:type 'nil?}}
                                                                         :boolean {:order 1
                                                                                   :value {:type 'boolean?}}
                                                                         :number {:order 2
                                                                                  :value {:type 'number?}}
                                                                         :text {:order 3
                                                                                :value {:type 'string?}}}}}}}}}}]]
    (testing "ast for"
      (testing (pr-str name)
        (testing "ast"
          (is (= ast (m/ast hiccup))))
        (testing "form"
          (is (= hiccup (m/form (m/from-ast ast)))))))))

(deftest -vmap-test
  (is (= [] (m/-vmap str nil)))
  (is (= [] (m/-vmap str [])))
  (is (= ["1"] (m/-vmap str [1])))
  (is (= ["1"] (m/-vmap str '(1))))
  (is (= ["1"] (m/-vmap str (subvec [1 2] 0 1))))
  (is (= ["1"] (m/-vmap str (lazy-seq [1]))))
  (is (= ["1" "2"] (m/-vmap str [1 2]))))

(deftest issue-626-test
  (testing "m/from-ast does not work with symbols or unamespaced keywords"
    (let [BiggerThan3 (m/schema
                       (m/-simple-schema
                        {:type :my/bigger-than-3
                         :pred #(> % 3)}))
          BiggerThan4 (m/schema
                       (m/-simple-schema
                        {:type :my-bigger-than-4
                         :pred #(> % 4)}))
          BiggerThan5 (m/schema
                       (m/-simple-schema
                        {:type 'my/bigger-than-5
                         :pred #(> % 5)}))
          options {:registry (malli.registry/composite-registry
                              (m/-registry)
                              {:my/bigger-than-3 BiggerThan3
                               :my-bigger-than-4 BiggerThan4
                               'my/bigger-than-5 BiggerThan5})}
          via-ast #(-> (m/ast % options) (m/from-ast options))]

      (is (m/schema? (via-ast :my/bigger-than-3)))
      (is (m/schema? (via-ast :my-bigger-than-4)))
      (is (m/schema? (via-ast 'my/bigger-than-5))))))

(deftest cat-catn-unparse-test
  (is (= ["1" 2 "3"] (m/unparse [:cat string? int? string?] ["1" 2 "3"])))
  (is (= ::m/invalid (m/unparse [:cat string? int? string?] [1 2 3])))
  (is (= ["1" 2 "3"] (m/unparse [:catn [:a string?] [:b int?] [:c string?]] (m/tags {:a "1" :b 2 :c "3"}))))
  (is (= ::m/invalid (m/unparse [:catn [:a string?] [:b int?] [:c string?]] (m/tags {:a 1 :b 2 :c 3})))))

(deftest unparse-confusion-test
  ;; parse-unparse should roundtrip even for weird situations where
  ;; the schema tries to match on the result of unparse. See #1150 #1153.
  (let [s [:or
           [:tuple :string :keyword]
           [:orn ["any" :keyword]]]]
    (is (= :k (m/unparse s (m/parse s :k)))))
  (let [s [:or
           [:map [:key :string] [:value :keyword]]
           [:orn ["any" :keyword]]]]
    (is (= :k (m/unparse s (m/parse s :k)))))
  (let [s [:or
           [:map [:s :string]]
           [:catn [:s :string]]]]
    (is (= ["k"] (m/unparse s (m/parse s ["k"])))))
  (let [s [:or
           [:map [:values [:map [:s :string]]]]
           [:catn [:s :string]]]]
    (is (= ["k"] (m/unparse s (m/parse s ["k"]))))))

(deftest repeat-unparse-test
  (is (m/validate [:repeat {:min 1 :max 2} [:cat :int :int]] [1 2 3 4]))
  (is (= [[1 2] [3 4]] (m/parse [:repeat {:min 1 :max 2} [:cat :int :int]] [1 2 3 4])))
  (is (= [1 2 3 4] (m/unparse [:repeat {:min 1 :max 2} [:cat :int :int]] [[1 2] [3 4]])))
  (is (= ::m/invalid (m/unparse [:repeat {:min 1 :max 1} [:cat :int :int]] [[1 2] [3 4]])))
  (is (= ::m/invalid (m/unparse [:repeat {:max 1} [:cat :int :int]] [[1 2] [3 4]])))
  (is (= ::m/invalid (m/unparse [:repeat {:min 3} [:cat :int :int]] [[1 2] [3 4]])))
  (is (= ::m/invalid (m/unparse [:repeat {:min 3} [:cat :int :int]] [[1 2] [3 4]])))
  (is (= [[1 2 3 4]] (m/parse [:repeat [:* :int]] [1 2 3 4])))
  (is (= [1 2 3 4] (m/unparse [:repeat [:* :int]] [[1 2 3 4]])))
  (is (= [[1 2 3 4]] (m/parse [:* [:repeat :int]] [1 2 3 4])))
  (is (= [1 2 3 4] (m/unparse [:* [:repeat :int]] [[1 2 3 4]])))
  (is (= [[1 2 3 4]] (m/parse [:repeat [:repeat :int]] [1 2 3 4])))
  (is (= [1 2 3 4] (m/unparse [:repeat [:repeat :int]] [[1 2 3 4]])))
  (is (= ::m/invalid (m/parse [:repeat [:repeat :int]] [1 nil 3 4])))
  (is (= ::m/invalid (m/unparse [:repeat [:repeat :int]] [[1 nil 3 4]]))))

(deftest issue-451-test
  (testing "registry -in schema vector syntax"
    (let [one-level-schema [:map {:registry {:my/string-like :string}}
                            [:entry [:my/string-like {:some "prop"}]]]]

      (is (true? (m/validate one-level-schema {:entry "a"})))))

  (testing "testcase from #451"
    (let [opts {:registry {:string (m/-string-schema)
                           ::derived-str :string
                           ::derived-str2 ::derived-str
                           ::derived-str3 [::derived-str {:error/message "bar"}]
                           ::derived-str4 [::derived-str2 {:error/message "baz"}]}}]

      (is (m/schema ::derived-str2 opts))
      (is (m/schema ::derived-str3 opts))
      (is (m/schema ::derived-str4 opts))

      (is (= "baz"
             (->
              (m/schema ::derived-str4 opts)
              (m/deref)
              (m/properties)
              :error/message))))))

(deftest -map-schema-test
  (let [test-data (array-map :id (random-uuid)
                             :name "baz"
                             :code (rand-int 100)
                             :organization (hash-map :id (random-uuid)
                                                     :code (rand-int 100)))]
    (testing "returns map schema with map? as default :pred fn"
      (let [DefaultMapSchema (m/-map-schema {:naked-keys true})
            data-schema (m/schema [DefaultMapSchema
                                   [:id uuid?]
                                   [:name string?]
                                   [:code {:optional true} int?]
                                   [:organization [DefaultMapSchema
                                                   [:id uuid?]
                                                   [:name {:optional true} string?]
                                                   [:code int?]]]])]
        (is (m/validate data-schema test-data))))

    (testing "returns map schema with custom :pred fn as provided in opts"
      (let [persistent-array-map? #(instance? PersistentArrayMap %)
            ArrayMapSchema (m/-map-schema {:pred persistent-array-map?})
            persistent-hash-map? #(instance? PersistentHashMap %)
            HashMapSchema (m/-map-schema {:pred persistent-hash-map?})
            data-schema (m/schema [ArrayMapSchema
                                   [:id uuid?]
                                   [:name string?]
                                   [:code {:optional true} int?]
                                   [:organization [HashMapSchema
                                                   [:id uuid?]
                                                   [:name {:optional true} string?]
                                                   [:code int?]]]])]
        (is (m/validate data-schema test-data))))
    (testing "returns map schema with type-properties as provided in opts"
      (let [error-message "error message coming from type-properties"
            type-properties {:error/message error-message}
            MapSchemaWithTypeProperties (m/-map-schema {:type-properties type-properties})
            data-schema (m/schema [MapSchemaWithTypeProperties])]
        (is (= type-properties (m/type-properties data-schema)))
        (is (= [error-message] (me/humanize (m/explain data-schema []))))))))

(deftest -map-of-schema-test
  (testing "returns map-of schema with type-properties as provided in opts"
    (let [error-message "custom-error-message"
          type-properties {:error/message error-message}
          MapOfSchemaWithTypeProperties (m/-map-of-schema {:type-properties type-properties})
          data-schema (m/schema [MapOfSchemaWithTypeProperties
                                 [:string]
                                 [:double]])]
      (is (= type-properties (m/type-properties data-schema)))
      (is (= [error-message] (me/humanize (m/explain data-schema [])))))))

(deftest -tuple-schema-test
  (testing "returns tuple schema with type-properties as provided in opts"
    (let [error-message "custom-error-message"
          type-properties {:error/message error-message}
          TupleSchemaWithTypeProperties (m/-tuple-schema {:type-properties type-properties})
          data-schema (m/schema [TupleSchemaWithTypeProperties])]
      (is (= type-properties (m/type-properties data-schema)))
      (is (= [error-message] (me/humanize (m/explain data-schema {})))))))

(deftest coerce-test
  (let [schema [:map [:x :keyword]]]
    (testing "without transformer, just validates"
      (testing "success"
        (is (= {:x :kikka} (m/coerce schema {:x :kikka})))
        (is (= {:x :kikka} (m/coerce schema {:x :kikka} nil)))
        (is (= {:x :kikka} (m/coerce schema {:x :kikka} nil nil))))
      (testing "fails"
        (is (thrown? #?(:clj Exception, :cljs js/Error) (m/coerce schema {:x "kikka"})))
        (is (thrown? #?(:clj Exception, :cljs js/Error) (m/coerce schema {:x "kikka"} nil)))
        (is (thrown? #?(:clj Exception, :cljs js/Error) (m/coerce schema {:x "kikka"} nil nil)))))
    (testing "with transformer"
      (testing "success"
        (is (= {:x :kikka} (m/coerce schema {:x "kikka"} mt/string-transformer)))
        (is (= {:x :kikka} (m/coerce schema {:x "kikka"} mt/string-transformer nil))))
      (testing "fails"
        (is (thrown? #?(:clj Exception, :cljs js/Error) (m/coerce schema {:x 123} mt/string-transformer)))
        (is (thrown? #?(:clj Exception, :cljs js/Error) (m/coerce schema {:x 123} mt/string-transformer nil)))))
    (testing "cps"
      (let [result (atom nil)
            respond #(swap! result assoc :respond %)
            raise #(swap! result assoc :raise %)
            <result (fn [] (let [res @result] (reset! result nil) res))]
        (testing "success"
          (m/coerce schema {:x :kikka} nil respond raise)
          (is (= {:respond {:x :kikka}} (<result))))
        (testing "failure"
          (m/coerce schema {:x "kikka"} nil respond raise)
          (is (= {:raise {:explain {:errors [{:in [:x]
                                              :path [:x]
                                              :schema :keyword
                                              :value "kikka"}]
                                    :schema schema
                                    :value {:x "kikka"}}
                          :schema schema
                          :value {:x "kikka"}}} (as-data (<result)))))))))

(deftest extra-entries-in-map-test
  (let [schema [:map
                [:x :boolean]
                [:y {:optional true} :int]
                [::m/default [:map-of :int :int]]]
        valid {:x true, :y 1}
        valid2 {:x true, :y 1, 123 123, 456 456}
        invalid {:x true, :y 1, 42 42, 123 "123", "456" 456}]

    (testing "validation"
      (is (true? (m/validate schema valid)))
      (is (true? (m/validate schema valid2)))
      (is (false? (m/validate schema invalid)))
      (is (false? (m/validate schema "not-a-map"))))

    (testing "explain"
      (is (nil? (m/explain schema valid)))
      (is (nil? (m/explain schema valid2)))
      (is (results= {:schema schema,
                     :value {:y "invalid", "123" "123"},
                     :errors [{:path [:x],
                               :in [:x],
                               :schema schema,
                               :value nil,
                               :type :malli.core/missing-key}
                              {:path [:y], :in [:y], :schema :int, :value "invalid"}
                              {:path [::m/default 0], :in ["123"], :schema :int, :value "123"}
                              {:path [::m/default 1], :in ["123"], :schema :int, :value "123"}]}
                    (m/explain schema {:y "invalid", "123" "123"}))))

    (testing "children"
      (is (schema= [[:x nil :boolean]
                    [:y {:optional true} :int]
                    [::m/default nil [:map-of :int :int]]]
                   (m/children schema))))

    (testing "entries"
      (is (true? (every? map-entry? (m/entries schema))))
      (is (= [:x :y ::m/default] (map key (m/entries schema))))
      (is (= [:x :y] (m/explicit-keys schema)))
      (is (schema= [[:x [::m/val :boolean]]
                    [:y [::m/val {:optional true} :int]]
                    [::m/default [::m/val [:map-of :int :int]]]]
                   (m/entries schema))))

    (testing "default-schema"
      (is (schema= [:map-of :int :int] (m/default-schema schema))))

    (testing "parsing and unparsing"
      (let [schema [:map {:registry {'int [:orn [::int :int]]
                                     'str [:orn [::str :string]]}}
                    [:id 'int]
                    ["name" 'str]
                    [::m/default [:map-of 'str 'str]]]
            valid {:id 1, "name" "tommi", "kikka" "kukka", "abba" "jabba"}]
        (is (= {:id (m/tag ::int 1)
                "name" (m/tag ::str "tommi")
                (m/tag ::str "kikka") (m/tag ::str "kukka")
                (m/tag ::str "abba") (m/tag ::str "jabba")}
               (m/parse schema valid)))
        (is (= valid (->> valid (m/parse schema) (m/unparse schema))))
        (is (= ::m/invalid (m/parse schema {"kukka" 42})))))

    (testing "stripping keys"
      (is (= {:x true, :y 1, 42 42}
             (m/decode schema invalid (mt/strip-extra-keys-transformer)))))

    (testing "::m/default transforming doesn't effect defined keys"
      (is (= {:id 12
              :name :kikka
              "age" 13
              "ABBA" "jabba"
              "KIKKA" "kukka"}
             (m/decode
              [:map
               [:id :int]
               [:name :keyword]
               ["age" :int]
               [::m/default [:map-of [:string {:decode/test str/upper-case}] :string]]]
              {:id 12, :name :kikka, "age" "13", "abba" "jabba", "kikka" "kukka"}
              (mt/transformer
               (mt/string-transformer)
               (mt/transformer {:name :test}))))))

    (testing "nil-punning tranformers"
      (is (= identity
             (m/decoder
              [:map
               [:id :int]
               [:name :keyword]
               ["age" :int]
               [::m/default [:map-of :keyword :keyword]]]
              (mt/transformer)))))

    (testing "over the wire"
      (is (true? (m/validate (over-the-wire schema) valid))))

    (testing "ast"
      (is (= {:type :map,
              :keys {:x {:order 0
                         :value {:type :boolean}},
                     :y {:order 1
                         :value {:type :int}
                         :properties {:optional true}},
                     :malli.core/default {:order 2
                                          :value {:type :map-of
                                                  :key {:type :int}
                                                  :value {:type :int}}}}}

             (m/ast schema)))
      (is (true? (m/validate (m/from-ast (m/ast schema)) valid))))

    (testing "form"
      (is (= [:map
              [:x :boolean]
              [:y {:optional true} :int]
              [::m/default [:map-of :int :int]]]
             (m/form schema))))))

(deftest -comp-test
  (let [prefixer (fn [prefix]
                   (fn [s] (str prefix s)))]
    (testing "the order of function applications"
      (is (= "abcxxx"
             ((m/-comp (prefixer "a")
                       (prefixer "b")
                       (prefixer "c")) "xxx")))
      (is (= "abcdefghixxx"
             ((m/-comp (prefixer "a")
                       (prefixer "b")
                       (prefixer "c")
                       (prefixer "d")
                       (prefixer "e")
                       (prefixer "f")
                       (prefixer "g")
                       (prefixer "h")
                       (prefixer "i")) "xxx"))))))

(deftest issue-925-test
  (testing "order is retained with catn parse+unparse"
    (let [schema [:catn
                  [:a :int]
                  [:b :int]
                  [:c :int]
                  [:d :int]
                  [:e :int]
                  [:f :int]
                  [:g :int]
                  [:h :int]
                  [:i :int]]
          input [1 2 3 4 5 6 7 8 9]]
      (is (= input (->> input (m/parse schema) (m/unparse schema)))))))

(deftest issue-937-test
  (testing ":altn can handle just one child entry when nested"
    (let [schema [:* [:altn [:a [:= :a]]]]
          value [:a]]
      (is (= true (m/validate schema value)))
      (is (= nil (m/explain schema value)))
      (is (= [(m/tag :a :a)] (m/parse schema value)))
      (is (= value (m/unparse schema (m/parse schema value))))
      (is (= value (m/decode schema value nil))))))

(def UserId :string)

(def User
  [:map
   [:id #'UserId]
   [:friends {:optional true} [:set [:ref #'User]]]])

(deftest var-registry-test
  (let [schema (m/schema User)]

    (testing "getting schema over Var works"
      (is (= UserId (mr/-schema (mr/var-registry) #'UserId)))
      (is (= User (mr/-schema (mr/var-registry) #'User))))

    (testing "we do not list all Var schemas (yet)"
      (is (= nil (mr/-schemas (mr/var-registry)))))

    (testing "it works"
      (is (= User (m/form schema)))
      (is (every? (m/validator schema) (mg/sample schema {:seed 100}))))

    (testing "explain path"
      (let [exp (m/explain schema {:id 1})]
        (is (results= {:value {:id 1}
                       :schema User
                       :errors [{:in [:id]
                                 :path [:id 0]
                                 :schema :string
                                 :value 1}]}
                      exp))
        (is (form= :string (mu/get-in schema (-> exp :errors first :path)))))
      (let [explicit-ref [:ref #'UserId]
            exp (m/explain explicit-ref 1)]
        (is (results= {:value 1
                       :schema explicit-ref
                       :errors [{:in []
                                 :path [0 0]
                                 :schema :string
                                 :value 1}]}
                      exp))
        (is (form= :string (mu/get-in explicit-ref (-> exp :errors first :path))))))))

#?(:clj
   (deftest roundrobin-var-references
     (let [schema (m/schema User)]

       (testing "default options with var-registry fails"
         (is (thrown? Exception
                      (as-> schema $
                        (edn/write-string $)
                        (edn/read-string $)
                        (every? (m/validator $) (mg/sample schema))))))

       (testing "with custom edamame options with var-registry succeeds"
         (is (as-> schema $
               (edn/write-string $)
               (edn/read-string $ {::edn/edamame-options {:fn true,
                                                          :regex true,
                                                          :var resolve}})
               (every? (m/validator $) (mg/sample schema))))))))

(deftest deref-recursive-test
  (let [schema [:schema {:registry {::user-id :uuid
                                    ::address [:map
                                               [:street :string]
                                               [:lonlat {:optional true} [:tuple :double :double]]]
                                    ::user [:map
                                            [:id ::user-id]
                                            [:name :string]
                                            [:friends {:optional true} [:set [:ref ::user]]]
                                            [:address ::address]]}}
                ::user]]
    (is (= [:map
            [:id :uuid]
            [:name :string]
            [:friends {:optional true} [:set [:ref ::user]]]
            [:address [:map
                       [:street :string]
                       [:lonlat {:optional true} [:tuple :double :double]]]]]
           (m/form (m/deref-recursive schema))
           (m/form (m/deref-recursive schema {::m/ref-key nil}))))
    (is (= [:map {:id ::user}
            [:id [:uuid {:id ::user-id}]]
            [:name :string]
            [:friends {:optional true} [:set [:ref ::user]]]
            [:address [:map {:id ::address}
                       [:street :string]
                       [:lonlat {:optional true} [:tuple :double :double]]]]]
           (m/form (m/deref-recursive schema {::m/ref-key :id}))))
    (testing "util schemas"
      (let [registry (merge (m/default-schemas) (mu/schemas))]
        (is (= [:map [:x :int] [:y :int]]
               (m/form (m/deref-recursive
                        [:merge
                         [:map [:x :int]]
                         [:map [:y :int]]]
                        {:registry registry}))))
        (is (= [:map {:id ::xymap}
                [::x [:int {:id ::x}]]
                [::y [:int {:id ::y}]]]
               (m/form (m/deref-recursive
                        [:schema {:registry {::x :int
                                             ::y :int
                                             ::xmap [:map ::x]
                                             ::ymap [:map ::y]
                                             ::xymap [:merge ::xmap ::ymap]}}
                         ::xymap]
                        {:registry registry, ::m/ref-key :id}))))))))

(deftest seqable-schema-test
  (is (m/validate [:seqable :int] nil))
  (is (m/validate [:seqable :int] #{1 2 3}))
  (is (m/validate [:seqable :int] [1 2 3]))
  (is (m/validate [:seqable :int] (sorted-set 1 2 3)))
  (is (m/validate [:seqable :int] #{1 2 3}))
  (is (m/validate [:seqable :int] (range 1000)))
  (is (not (m/validate [:seqable :int] (conj (vec (range 1000)) nil))))
  (is (not (m/validate [:seqable :int] (concat (range 1000) [nil]))))
  (is (not (m/validate [:seqable :int] (eduction (concat (range 1000) [nil])))))
  (is (not (m/validate [:seqable {:min 1000} :int] (eduction (concat (range 1000) [nil])))))
  (is (not (m/validate [:seqable {:min 1000} :int] (concat (range 1000) [nil]))))
  (is (nil? (m/explain [:seqable :int] #{1 2 3})))
  (is (not (m/validate [:seqable :int] #{1 nil 3})))
  (is (= #{["should be an integer"]}
         (me/humanize (m/explain [:seqable :int] #{1 nil 3}))))
  (let [original (interleave (range 10) (cycle [true false]))
        parsed (m/parse [:seqable [:orn [:l :int] [:r :boolean]]] original)
        unparsed (m/unparse [:seqable [:orn [:l :int] [:r :boolean]]] parsed)]
    (is (= original unparsed))
    (is (= [(m/tag :l 0) (m/tag :r true) (m/tag :l 1) (m/tag :r false) (m/tag :l 2) (m/tag :r true) (m/tag :l 3) (m/tag :r false) (m/tag :l 4) (m/tag :r true) (m/tag :l 5)
            (m/tag :r false) (m/tag :l 6) (m/tag :r true) (m/tag :l 7) (m/tag :r false) (m/tag :l 8) (m/tag :r true) (m/tag :l 9) (m/tag :r false)]
           parsed)))
  (let [original (sorted-set 1 2 3)
        parsed (m/parse [:seqable [:orn [:a :int]]] original)
        unparsed (m/unparse [:seqable [:orn [:a :int]]] parsed)]
    (is (= unparsed [1 2 3]))
    (is (= parsed [(m/tag :a 1) (m/tag :a 2) (m/tag :a 3)]))))

(deftest every-schema-test
  (is (m/validate [:every :int] nil))
  (is (m/validate [:every :int] #{1 2 3}))
  (is (m/validate [:every :int] [1 2 3]))
  (is (m/validate [:every :int] (sorted-set 1 2 3)))
  (is (not (m/validate [:every :int] (conj (vec (range 1000)) nil))))
  (is (nil? (m/explain [:every :int] #{1 2 3})))
  (is (not (m/validate [:every :int] #{1 nil 3})))
  (is (m/validate [:every :int] (concat (range 1000) [nil])))
  (is (m/validate [:every :int] (eduction (concat (range 1000) [nil]))))
  ;; counted/indexed colls have everything validated
  (is (not (m/validate [:every :int] (vec (concat (range 1000) [nil])))))
  (is (m/validate [:every :int] (concat (range 1000) [nil])))
  (is (m/validate [:every :int] (eduction (concat (range 1000) [nil]))))
  (is (m/validate [:every {:min 1000} :int] (concat (range 1000) [nil])))
  (is (m/validate [:every :int] (concat (range 1000) [nil]) {::m/coll-check-limit 1000}))
  (is (m/validate [:every {:min 1000} :int] (eduction (concat (range 1000) [nil]))))
  ;; counted/indexed colls have everything validated
  (is (not (m/validate [:every {:min 1000} :int] (vec (concat (range 1000) [nil])))))
  (is (not (m/validate [:every {:min 1001} :int] (concat (range 1000) [nil]))))
  (is (not (m/validate [:every {:min 1001} :int] (eduction (concat (range 1000) [nil])))))
  (is (m/validate [:every {:max 1000} :int] (range 1000)))
  (is (not (m/validate [:every {:max 1000} :int] (range 1001))))
  (is (not (m/validate [:every {:max 1001} :int] (concat (range 1000) [nil]))))
  (is (not (m/validate [:every {:max 1001} :int] (eduction (concat (range 1000) [nil])))))
  (is (= #{["should be an integer"]}
         (me/humanize (m/explain [:every :int] #{1 nil 3}))))
  (is (nil? (m/explain [:every :int] (concat (range 1000) [nil]))))
  (is (nil? (m/explain [:every :int] (eduction (concat (range 1000) [nil])))))
  (is (= (concat (repeat 1000 nil) [["should be an integer"]])
         (me/humanize (m/explain [:every {:min 1001} :int] (concat (range 1000) [nil])))))
  (is (= (concat (repeat 1000 nil) [["should be an integer"]])
         (me/humanize (m/explain [:every {:min 1001} :int] (eduction (concat (range 1000) [nil]))))))
  (is (= (concat (repeat 1000 nil) [["should be an integer"]])
         (me/humanize (m/explain [:every {:max 1001} :int] (concat (range 1000) [nil])))))
  (is (= (concat (repeat 1000 nil) [["should be an integer"]])
         (me/humanize (m/explain [:every {:max 1001} :int] (eduction (concat (range 1000) [nil]))))))
  (doseq [parse [#'m/parse #'m/unparse]]
    (testing parse
      (let [good-sequence (interleave (range 10) (cycle [true false]))]
        (is (identical? good-sequence
                        (parse [:every [:orn [:l :int] [:r :boolean]]]
                               good-sequence))))
      (is (= ::m/invalid
             (parse [:every [:orn [:l :int] [:r :boolean]]]
                    (interleave (range 10) (cycle [true false nil])))))
      (doseq [coerce [#'identity #?(:clj #'eduction
                                    ;;TODO :cljs ?
                                    )]]
        (testing coerce
          (let [bad-but-too-big (coerce
                                 (concat (interleave (range 1000) (cycle [true false]))
                                         [nil]))
                bad-indexed-seq (vec bad-but-too-big)]
            (is (identical? bad-but-too-big
                            (parse [:every [:orn [:l :int] [:r :boolean]]]
                                   bad-but-too-big)))
            (is (= ::m/invalid
                   (parse [:every [:orn [:l :int] [:r :boolean]]]
                          bad-indexed-seq)))))))))

(deftest proxy-schema-explain-path
  (let [y-schema [:int {:doc "int"}]
        schema (m/schema [(mu/-select-keys)
                          [:map
                           [:x :int]
                           [:y y-schema]
                           [:z :int]]
                          [:x :y]])
        explain (m/explain schema {:x 1, :y "2"})]
    (is (results= {:schema schema
                   :value {:x 1, :y "2"}
                   :errors [{:path [::m/in :y], :in [:y], :schema y-schema, :value "2"}]}
                  explain))
    (is (form= y-schema (mu/get-in schema (-> explain :errors first :path))))))

(deftest catch-infinitely-expanding-schema
  (is (thrown-with-msg?
        #?(:clj Exception, :cljs js/Error)
        #?(:clj #":malli\.core/infinitely-expanding-schema"
           :cljs #":malli\.core/invalid-schema")
        (m/schema [(m/schema :any)]))))

(deftest eduction-test
  (is (m/validate [:sequential {:min 0} :int] (eduction identity (range 10))))
  (is (m/validate [:sequential {:max 0} :int] (eduction)))
  (is (not (m/validate [:sequential {:max 0} :int] (eduction [1]))))
  (is (not (m/validate [:sequential {:min 11} :int] (eduction identity (range 10)))))
  (is (not (m/validate [:seqable {:min 11} :int] (eduction identity (range 10)))))
  (is (nil? (m/explain [:sequential {:min 9} :int] (eduction identity (range 10))))))

(deftest pr-str-test
  (testing "print IntoSchema"
    (is (= "#IntoSchema {:type :and}"
           (pr-str (m/-and-schema)))))
  (testing "print Schema"
    (is (= "[:map [:x :int]]"
           (pr-str (m/schema [:map [:x :int]]))))))
