(ns malli.core-test
  (:require [clojure.test :refer [deftest testing is are]]
            [malli.core :as m]
            [malli.edn :as me]
            [malli.transform :as mt]
            [malli.util :as mu]
            [malli.registry :as mr]
            [malli.impl.util :as miu]
            [clojure.walk :as walk]
            [malli.generator :as mg]
            [clojure.test.check.generators :as gen]))

(defn with-schema-forms [result]
  (some-> result
          (update :schema m/form)
          (update :errors (partial map (fn [error]
                                         (-> error
                                             (update :schema m/form)
                                             (update :type (fnil identity nil))
                                             (update :message (fnil identity nil))
                                             (miu/map->SchemaError)))))))

(defn results= [& results]
  (apply = (map with-schema-forms results)))

(defn schema= [& entries]
  (apply = (map (partial walk/postwalk (fn [x] (if (m/schema? x) (m/form x) x))) entries)))

(defn form= [& entries]
  (apply = (map m/form entries)))

(defn over-the-wire [?schema]
  (-> ?schema (me/write-string) (me/read-string)))

(deftest keyword->string
  (is (= "abba" (m/-keyword->string :abba)))
  (is (= "jabba/abba" (m/-keyword->string :jabba/abba)))
  (is (= "abba" (m/-keyword->string "abba"))))

(deftest parse-entries-test
  (let [{:keys [children entries forms]} (m/-parse-entries
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
             forms)))
    (testing "entries"
      (is (schema= [[:x [::m/val 'int?]]
                    [::x [::m/val ::x]]
                    ["x" [::m/val "x"]]
                    [::y [::m/val {:optional true} ::y]]
                    [:y [::m/val {:optional true :title "boolean"} 'boolean?]]]
                   entries)))
    (testing "children"
      (is (= [[:x nil 'int?]
              [::x nil ::x]
              ["x" nil "x"]
              [::y {:optional true} ::y]
              [:y {:optional true, :title "boolean"} 'boolean?]]
             (map #(update % 2 m/form) children)))))
  (testing "duplicate keys"
    (is (thrown? #?(:clj Exception, :cljs js/Error)
                 (m/-parse-entries
                   [[:x int?]
                    [:x boolean?]] {:naked-keys true} nil))))
  (testing "naked keys fails when not supported"
    (is (thrown? #?(:clj Exception, :cljs js/Error)
                 (m/-parse-entries
                   [::x] nil nil)))))

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
  (testing "with options"
    (testing "disabling sci"
      (is (= 2 ((m/eval inc {::m/disable-sci true}) 1)))
      (is (thrown? #?(:clj Exception, :cljs js/Error) ((m/eval 'inc {::m/disable-sci true}) 1))))
    (testing "custom bindings"
      (let [f '(fn [schema] (m/form schema))]
        (is (thrown? #?(:clj Exception, :cljs js/Error) ((m/eval f) :string)))
        (is (= :string ((m/eval f {::m/sci-options {:bindings {'m/form m/form}}}) :string)))))))

(deftest into-schema-test
  (is (form= [:map {:closed true} [:x int?]]
             (m/into-schema :map {:closed true} [[:x int?]]))))

(deftest schema-walker-test
  (is (form= [:map {:closed true} [:x int?]]
             (m/walk [:map {:closed true} [:x int?]] (m/schema-walker identity))))
  (is (form= [:map {:registry {::age [:and int? [:> 18]]}} [:age ::age]]
             (m/walk [:map {:registry {::age [:and int? [:> 18]]}} [:age ::age]]
                     (m/schema-walker identity)))))

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
                     :errors [(m/-error [] [] schema "1")]}
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
            (is (m/decoder schema {::m/disable-sci false} mt/string-transformer)))))

      (is (true? (m/validate (over-the-wire schema) 1)))

      (is (= {:type 'int?}
             (mu/to-map-syntax schema)))

      (is (= 'int? (m/form schema)))))

  (testing "composite schemas"
    (let [schema (m/schema [:and int? [:or pos-int? neg-int?]])
          schema* (m/schema [:and int? [:or* [:pos pos-int?] [:neg neg-int?]]])]

      (doseq [schema [schema schema*]]
        (is (true? (m/validate schema 1)))
        (is (true? (m/validate schema -1)))
        (is (false? (m/validate schema 0)))
        (is (false? (m/validate schema "1")))
        (is (false? (m/validate schema [1]))))

      (is (= pos-int? (m/validator [:and pos-int? pos-int? pos-int?])))
      (is (= pos-int? (m/validator [:or pos-int? pos-int? pos-int?])))
      (is (= pos-int? (m/validator [:or* [:a pos-int?] [:b pos-int?] [:c pos-int?]])))

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

      (is (= (miu/-tagged :pos 1) (m/parse schema* 1)))
      (is (= ::m/invalid (m/parse schema* 0)))
      (is (= 1 (m/unparse schema* (miu/-tagged :pos 1))))
      (is (= ::m/invalid (m/unparse schema* (miu/-tagged :pos 0))))

      (doseq [schema [schema schema*]]
        (is (= 1 (m/decode schema "1" mt/string-transformer)))
        (is (= "1" (m/decode schema "1" mt/json-transformer))))

      (is (= "olipa_kerran_avaruus"
             (m/decode
               [:and {:decode/string '{:enter #(str "olipa_" %), :leave #(str % "_avaruus")}} string?]
               "kerran" mt/string-transformer)))

      (doseq [schema [schema schema*]]
        (is (true? (m/validate (over-the-wire schema) 1))))

      (is (= {:type :and
              :children [{:type 'int?}
                         {:type :or
                          :children [{:type 'pos-int?}
                                     {:type 'neg-int?}]}]}
             (mu/to-map-syntax schema)))
      (is (= {:type :and
              :children [{:type 'int?}
                         {:type :or*
                          :children [[:pos nil {:type 'pos-int?}]
                                     [:neg nil {:type 'neg-int?}]]}]}
             (mu/to-map-syntax schema*)))

      (is (= [:and 'int? [:or 'pos-int? 'neg-int?]] (m/form schema)))
      (is (= [:and 'int? [:or* [:pos 'pos-int?] [:neg 'neg-int?]]] (m/form schema*))))

    (testing "transforming :or"
      (testing "first valid transformed branch is used"
        (doseq [schema [[:or
                         [:map [:x keyword?]]
                         int?
                         [:map [:y keyword?]]
                         keyword?]
                        [:or*
                         [:äxy [:map [:x keyword?]]]
                         [:n int?]
                         [:yxy [:map [:y keyword?]]]
                         [:kw keyword?]]]]
          (are [input result]
            (is (= (m/decode schema input mt/string-transformer)
                   result))

            {:x "true", :y "true"} {:x :true, :y "true"}
            {:x false, :y "true"} {:x false, :y :true}
            {:x false, :y false} {:x false, :y false}
            1 1
            "kikka" :kikka)))

      (testing "top-level transformations are retained"
        (doseq [schema [[:or {:decode/string {:enter (fn [m] (update m :enter #(or % true)))
                                              :leave (fn [m] (update m :leave #(or % true)))}}
                         [:map
                          [:x keyword?]
                          [:enter boolean?]]
                         [:map
                          [:y keyword?]
                          [:enter boolean?]]]
                        [:or* {:decode/string {:enter (fn [m] (update m :enter #(or % true)))
                                               :leave (fn [m] (update m :leave #(or % true)))}}
                         [:äxy [:map
                                [:x keyword?]
                                [:enter boolean?]]]
                         [:yxy [:map
                                [:y keyword?]
                                [:enter boolean?]]]]]]
          (are [input result]
            (is (= (m/decode (mu/closed-schema schema) input mt/string-transformer)
                   result))

            {:x "true"} {:x :true, :enter true, :leave true}
            {:x "true", :enter "invalid"} {:x "true", :enter "invalid", :leave true}

            {:y "true"} {:y :true, :enter true, :leave true}
            {:y "true", :leave "invalid"} {:y "true", :enter true, :leave "invalid"}

            {:x "true", :y "true"} {:x "true", :y "true", :enter true, :leave true}))))

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

      (is (= {:type :>, :children [0]}
             (mu/to-map-syntax schema)))

      (is (= [:> 0] (m/form schema)))))

  (testing "enum schemas"
    (let [schema (m/schema [:enum 1 2])]

      (is (true? (m/validate schema 1)))
      (is (false? (m/validate schema 0)))
      (is (false? (m/validate schema "abba")))

      (is (nil? (m/explain schema 1)))
      (is (results= {:schema [:enum 1 2], :value 0, :errors [{:path [0], :in [], :schema [:enum 1 2], :value 0}]}
                    (m/explain [:enum 1 2] 0)))

      (is (= 1 (m/parse schema 1)))
      (is (= ::m/invalid (m/parse schema 0)))
      (is (= 1 (m/unparse schema 1)))
      (is (= ::m/invalid (m/unparse schema 0)))

      ;; TODO: infer type from :enum
      #_(is (= 1 (m/decode schema "1" mt/string-transformer)))
      #_(is (= "1" (m/decode schema "1" mt/json-transformer)))

      (testing "map enums require nil properties"
        (let [schema [:enum nil {:a 1} {:b 2}]]
          (is (= nil (m/properties schema)))
          (is (= [{:a 1} {:b 2}] (m/children schema)))))

      (is (true? (m/validate (over-the-wire schema) 1)))

      (is (= {:type :enum, :children [1 2]}
             (mu/to-map-syntax schema)))

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

      (is (= {:type :maybe, :children [{:type 'int?}]}
             (mu/to-map-syntax schema)))

      (is (= [:maybe 'int?] (m/form schema)))))

  (testing "ref schemas"

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
                                 :path [0 1 0 0]
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

        (is (= {:type :schema
                :properties {:registry {::cons [:maybe [:tuple 'int? [:ref ::cons]]]}}
                :children [{:type :malli.core/schema, :children [::cons]}]}
               (mu/to-map-syntax ConsCell)))

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
                     :errors [{:path [0 0] :in [] :schema pos-int?, :value -1}
                              {:path [0 1], :in [], :schema pos-int?, :value -1}
                              {:path [0 2], :in [], :schema pos-int?, :value -1}]}

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

      (is (= {:type :and
              :children [{:type :and
                          :children [{:type ::m/schema
                                      :children [::a]}
                                     {:type ::m/schema
                                      :children [::b]}
                                     {:type ::m/schema
                                      :children [::c]}]}]
              :properties {:registry {::a ::b
                                      ::b ::c
                                      ::c [:schema 'pos-int?]}}}
             (mu/to-map-syntax schema)))

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

        (is (= {:type :re, :children [re]}
               (mu/to-map-syntax schema)))

        (is (= form (m/form schema))))))

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

        (is (= {:type :fn
                :children [fn]
                :properties {:description "number between 10 and 18"}}
               (mu/to-map-syntax schema)))

        (is (= [:fn {:description "number between 10 and 18"} fn]
               (m/form schema)))))

    (testing "non-terminating functions fail fast"
      (let [schema [:fn '(fn [x] (< x (apply max (range))))]]
        (is (false? (m/validate schema 1)))
        (is (results= {:schema schema
                       :value 1
                       :errors [{:path []
                                 :in []
                                 :schema schema
                                 :value 1
                                 :type :sci.error/realized-beyond-max}]}
                      (m/explain schema 1))))))

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
          valid-with-extras {:x true, :y 1, :z "kikka", :extra "key"}
          valid2 {:x false, :y 1, :z "kikka"}
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

      (is (schema= [[:x [::m/val 'boolean?]]
                    [:y [::m/val {:optional true} 'int?]]
                    [:z [::m/val {:optional false} 'string?]]]
                   (m/entries schema)))

      (is (results= {:schema schema
                     :value {:y "invalid" :z "kikka"}
                     :errors
                     [{:path [:x], :in [:x], :schema schema, :type ::m/missing-key}
                      {:path [:y], :in [:y], :schema int?, :value "invalid"}]}
                    (m/explain schema {:y "invalid" :z "kikka"})))

      (is (results= {:schema schema,
                     :value {:x "invalid"},
                     :errors [{:path [:x], :in [:x], :schema boolean?, :value "invalid"}
                              {:path [:z],
                               :in [:z],
                               :schema schema,
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
                               :value nil,
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

      (is (= {:x 32}
             (m/decode
               [:map {:decode/string '{:enter #(update % :x inc), :leave #(update % :x (partial * 2))}}
                [:x {:decode/string '{:enter inc, :leave inc}}
                 [int? {:decode/string '{:enter (partial + 2), :leave (partial * 3)}}]]]
               {:x 1}
               mt/string-transformer)))

      (is (true? (m/validate (over-the-wire schema) valid)))

      (is (= {:type :map
              :children [[:x nil {:type 'boolean?}]
                         [:y {:optional true} {:type 'int?}]
                         [:z {:optional false} {:type 'string?}]]}
             (mu/to-map-syntax schema)))

      (is (= [:map
              [:x 'boolean?]
              [:y {:optional true} 'int?]
              [:z {:optional false} 'string?]]
             (m/form schema))))

    (is (true? (m/validate [:map [:b boolean?]] {:b true})))
    (is (true? (m/validate [:map [:b boolean?]] {:b false})))
    (is (true? (m/validate [:map [:n nil?]] {:n nil}))))

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
          invalid1 {:type :sized, :size "size"}
          invalid2 {:type :human :namez "inkeri"}
          invalid3 {:type :worm}]

      (is (true? (m/validate schema valid1)))
      (is (true? (m/validate schema valid2)))
      (is (false? (m/validate schema invalid1)))
      (is (false? (m/validate schema invalid2)))
      (is (false? (m/validate schema invalid3)))
      (is (false? (m/validate schema "not-a-map")))

      (is (nil? (m/explain schema valid1)))
      (is (nil? (m/explain schema valid2)))

      (is (results= {:schema schema,
                     :value {:type :sized, :size "size"},
                     :errors [{:path [:sized :size], :in [:size], :schema int?, :value "size"}]}
                    (m/explain schema invalid1)))

      (is (results= {:schema schema,
                     :value {:type :human, :namez "inkeri"},
                     :errors [{:path [:human :name]
                               :in [:name]
                               :schema [:map [:type keyword?] [:name string?] [:address [:map [:country keyword?]]]]
                               :type :malli.core/missing-key}
                              {:path [:human :address]
                               :in [:address]
                               :schema [:map [:type keyword?] [:name string?] [:address [:map [:country keyword?]]]]
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

      (is (= valid1 (m/parse schema valid1)))
      (is (= valid2 (m/parse schema valid2)))
      (is (= ::m/invalid (m/parse schema invalid1)))
      (is (= ::m/invalid (m/parse schema invalid2)))
      (is (= ::m/invalid (m/parse schema invalid3)))
      (is (= ::m/invalid (m/parse schema "not-a-map")))
      (is (= valid1 (m/unparse schema valid1)))
      (is (= valid2 (m/unparse schema valid2)))
      (is (= ::m/invalid (m/unparse schema invalid1)))
      (is (= ::m/invalid (m/unparse schema invalid2)))
      (is (= ::m/invalid (m/unparse schema invalid3)))
      (is (= ::m/invalid (m/unparse schema "not-a-map")))

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

      (is (true? (m/validate (over-the-wire schema) valid1)))

      (is (= {:type :multi
              :properties {:dispatch :type, :decode/string '(fn [x] (update x :type keyword))}
              :children [[:sized nil {:type :map
                                      :children [[:type nil {:type 'keyword?}]
                                                 [:size nil {:type 'int?}]]}]
                         [:human nil {:type :map
                                      :children [[:type nil {:type 'keyword?}]
                                                 [:name nil {:type 'string?}]
                                                 [:address nil {:type :map
                                                                :children [[:country nil {:type 'keyword?}]]}]]}]]}
             (mu/to-map-syntax schema)))

      (is (schema= [[:sized nil [:map [:type 'keyword?] [:size 'int?]]]
                    [:human nil [:map [:type 'keyword?] [:name 'string?] [:address [:map [:country 'keyword?]]]]]]
                   (m/children schema)))

      (is (= [:multi
              {:dispatch :type, :decode/string '(fn [x] (update x :type keyword))}
              [:sized [:map [:type 'keyword?] [:size 'int?]]]
              [:human [:map [:type 'keyword?] [:name 'string?] [:address [:map [:country 'keyword?]]]]]]
             (m/form schema)))))

  (testing "map-of schema"

    (is (true? (m/validate [:map-of string? int?] {"age" 18})))
    (is (true? (m/validate [:map-of keyword? int?] {:age 18})))
    (is (false? (m/validate [:map-of string? int?] {:age "18"})))
    (is (false? (m/validate [:map-of string? int?] 1)))

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

    (is (= {:type :map-of, :children [{:type 'int?} {:type 'pos-int?}]}
           (mu/to-map-syntax [:map-of int? pos-int?])))

    (testing "keyword keys are transformed via strings"
      (is (= {1 1} (m/decode [:map-of int? pos-int?] {:1 "1"} mt/string-transformer)))))

  (testing "sequence schemas"

    (testing "empty schemas fail"
      (doseq [element [:vector :sequential :set]]
        (is (thrown? #?(:clj Exception, :cljs js/Error) (m/schema [element])))))

    (testing "empty tuples are ok"
      (is (m/validate :tuple []))
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
          (is (= "age:31"
                 (m/encode schema data
                           (let [should-not-be-called
                                 (fn [_] (throw (ex-info "Was called" {:schema schema
                                                                       :data data})))]
                             (mt/transformer
                               {:name :test
                                :encoders {'int? should-not-be-called
                                           'keyword? should-not-be-called}})))))
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
                                          :errors [{:path [:x 0], :in [:x], :schema [:enum "x"], :value "non-x"}]}]

                                        [schema {:x "x" :y "non-y"}
                                         {:schema schema
                                          :value {:x "x" :y "non-y"}
                                          :errors [{:path [:y 0], :in [:y], :schema [:enum "y"], :value "non-y"}]}]

                                        [schema {:x "non-x" :y "non-y"}
                                         {:schema schema
                                          :value {:x "non-x" :y "non-y"}
                                          :errors [{:path [:x 0], :in [:x], :schema [:enum "x"], :value "non-x"}
                                                   {:path [:y 0], :in [:y], :schema [:enum "y"], :value "non-y"}]}]])}
            expectations (assoc expectations "sequential" (concat (get expectations "list") (get expectations "vector")))]

        (doseq [[name data] expectations
                [schema value expected] data]
          (testing name
            (is (results= expected (m/explain schema value)))))))

    (testing "visit"
      (doseq [name [:vector :sequential :set]]
        (is (= {:type name, :children [{:type 'int?}]}
               (mu/to-map-syntax [name int?]))))
      (is (= {:type :tuple, :children [{:type 'int?} {:type 'int?}]}
             (mu/to-map-syntax [:tuple int? int?])))))

  (testing "seqex schemas"
    (doseq [typ [:cat :cat*]]
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
              [] {} nil
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
              [] nil [{:path [(case typ :cat* :s 0)], :in [0], :schema string?, :value nil, :type ::m/end-of-input}]
              ["foo"] {:s "foo"} nil
              [0] nil [{:path [(case typ :cat* :s 0)], :in [0], :schema string?, :value 0}]
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
              [] nil [{:path [(case typ :cat* :s 0)], :in [0], :schema string?, :value nil, :type ::m/end-of-input}]
              ["foo"] nil [{:path [(case typ :cat* :n 1)], :in [1], :schema int?, :value nil, :type ::m/end-of-input}]
              ["foo" 0] {:s "foo", :n 0} nil
              ["foo" "bar"] nil [{:path [(case typ :cat* :n 1)], :in [1], :schema int?, :value "bar"}]
              [1 2] nil [{:path [(case typ :cat* :s 0)], :in [0], :schema string?, :value 1}]
              ["foo" 0 1] nil [{:path [], :in [2], :schema s, :value 1, :type ::m/input-remaining}])))

        (testing "triplet"
          (let [s [typ (case typ :cat string? [:s string?]) (case typ :cat int? [:n int?])
                   (case typ :cat keyword? [:k keyword?])]]
            (are [v v* errs]
              (let [es errs]
                (and (= (m/validate s v) (nil? es))
                     (results= (m/explain s v) (and es {:schema s, :value v, :errors es}))
                     (= (m/parse s v) (if (nil? es) (if (= typ :cat) v v*) ::m/invalid))
                     (or (some? es) (= (m/unparse s (if (= typ :cat) v v*) s) v))))

              0 nil [{:path [], :in [], :schema s, :value 0, :type ::m/invalid-type}]
              "foo" nil [{:path [], :in [], :schema s, :value "foo", :type ::m/invalid-type}]
              nil nil [{:path [], :in [], :schema s, :value nil, :type ::m/invalid-type}]
              [] nil [{:path [(case typ :cat* :s 0)], :in [0], :schema string?, :value nil, :type ::m/end-of-input}]
              ["foo"] nil [{:path [(case typ :cat* :n 1)], :in [1], :schema int?, :value nil, :type ::m/end-of-input}]
              ["foo" 0] nil [{:path [(case typ :cat* :k 2)], :in [2], :schema keyword?, :value nil, :type ::m/end-of-input}]
              ["foo" 0 :bar] {:s "foo", :n 0, :k :bar} nil
              ["foo" 0 "bar"] nil [{:path [(case typ :cat* :k 2)], :in [2], :schema keyword?, :value "bar"}]
              ["foo" 0 :bar 0] nil [{:path [], :in [3], :schema s, :value 0, :type ::m/input-remaining}])))

        (testing "* backtracks"
          (let [s [:cat [:* pos?] [:= 4]]
                s* [:cat* [:pos [:* pos?]] [:four [:= 4]]]
                v [4 4 4 4]]
            (is (m/validate s v))

            (is (= [[4 4 4] 4] (m/parse s v)))
            (is (= {:pos [4 4 4], :four 4} (m/parse s* v)))
            (is (= v (m/unparse s [[4 4 4] 4])))
            (is (= v (m/unparse s* {:pos [4 4 4], :four 4})))))))

    (doseq [typ [:alt :alt*]]
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
              ["foo"] ["foo" (miu/-tagged :s "foo")] nil
              [0] nil [{:path [(case typ :alt* :s 0)], :in [0], :schema string?, :value 0}]
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
              ["foo"] ["foo" (miu/-tagged :s "foo")] nil
              [0] [0 (miu/-tagged :n 0)] nil
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
              ["foo"] ["foo" (miu/-tagged :s "foo")] nil
              [0] [0 (miu/-tagged :n 0)] nil
              [:foo] [:foo (miu/-tagged :k :foo)] nil
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
          (is (= v (m/unparse s (concat (repeat n nil) v)))))))

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

      (let [s [:repeat {:min 1, :max 3} string?]]
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
          ["foo" "bar"] nil
          ["foo" "bar" 0] [{:path [0], :in [2], :schema string?, :value 0}
                           {:path [], :in [2], :schema s, :value 0, :type ::m/input-remaining}]
          ["foo" "bar" "baz"] nil
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
  (let [schema [:map {:registry {::id int?
                                 ::country string?}}
                ::id
                [:name string?]
                [::country {:optional true}]]]

    (testing "Example with qualified keyword + optional, regular key"
      (is (m/validate schema {::id 123 ::country "Finland" :name "Malli"})))

    (testing "Optional qualified keyword is optional"
      (is (m/validate schema {::id 123 :name "Malli"})))))

(deftest simple-schemas
  (testing "simple schemas"
    (doseq [[type {:keys [schema validate explain decode encode map-syntax form]}]
            {:string {:schema [:string {:min 1, :max 4}]
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
                      :map-syntax {:type :string, :properties {:min 1, :max 4}}
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
                   :map-syntax {:type :int, :properties {:min 1, :max 4}}
                   :form [:int {:min 1, :max 4}]}
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
                      :map-syntax {:type :double, :properties {:min 1.0, :max 4.0}}
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
                       :map-syntax {:type :keyword}
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
                                 :map-syntax {:type :qualified-keyword, :properties {:namespace :user}}
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
                      :map-syntax {:type :symbol}
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
                                :map-syntax {:type :qualified-symbol}
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
                    :map-syntax {:type :uuid}
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

        (testing "map-syntax"
          (is (= map-syntax (mu/to-map-syntax schema))))))))

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
                 (fn [{:keys [value]} _]
                   (assert (int? value))
                   {:type :user/over
                    :pred #(and (int? %) (> % value))
                    :type-properties {:error/message (str "should be over " value)
                                      :decode/string mt/-string->long
                                      :json-schema/type "integer"
                                      :json-schema/format "int64"
                                      :json-schema/minimum value}}))]

      (testing "over6"
        (let [schema [Over {:value 6}]]
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

(defn single-arity
  ([x] x)
  ([_ _] (m/-fail! ::arity-error)))

(deftest function-schema-test
  (let [f-ok (fn [x y] (+ x y))
        => [:=> [:tuple int? int?] int?]]

    (testing "by default, all ifn? are valid"
      (is (true? (m/validate => identity)))
      (is (true? (m/validate => #{}))))

    (testing "using generative testing"
      (is (false? (m/validate => single-arity {::m/=>validator mg/=>validator})))
      ;; js allows invalid arity
      #?(:clj (is (false? (m/validate => (fn [x] x) {::m/=>validator mg/=>validator}))))
      #?(:clj (is (false? (m/validate => #{} {::m/=>validator mg/=>validator}))))
      (is (true? (m/validate => f-ok {::m/=>validator mg/=>validator})))
      (is (false? (m/validate => (fn [x y] (str x y)) {::m/=>validator mg/=>validator}))))

    (is (nil? (m/explain => (fn [x y] (+ x y)) {::m/=>validator mg/=>validator})))
    (is (results= {:schema [:=> [:tuple int? int?] int?]
                   :value single-arity
                   :errors [{:path []
                             :in []
                             :schema [:=> [:tuple int? int?] int?]
                             :value single-arity}]}
                  (m/explain => single-arity {::m/=>validator mg/=>validator})))

    (is (= single-arity (m/decode => single-arity mt/string-transformer)))

    (is (true? (m/validate (over-the-wire =>) f-ok)))

    (is (= {:type :=>, :children [{:type :tuple, :children [{:type 'int?} {:type 'int?}]} {:type 'int?}]}
           (mu/to-map-syntax =>)))))
