(ns malli.core-test
  (:require [clojure.test :refer [deftest testing is are]]
            [malli.core :as m]
            [malli.edn :as me]
            [malli.transform :as mt]
            [malli.util :as mu]
            [malli.registry :as mr]))

(defn with-schema-forms [result]
  (some-> result
          (update :schema m/form)
          (update :errors (partial map (fn [error]
                                         (-> error
                                             (update :schema m/form)
                                             (update :type (fnil identity nil))
                                             (update :message (fnil identity nil))
                                             (m/map->SchemaError)))))))

(defn results= [& results]
  (apply = (map with-schema-forms results)))

(defn entries= [& entries]
  (apply = (map (partial map #(update % 2 m/form)) entries)))

(defn form= [& entries]
  (apply = (map m/form entries)))

(defn over-the-wire [?schema]
  (-> ?schema (me/write-string) (me/read-string)))

(deftest keyword->string
  (is (= "abba" (m/keyword->string :abba)))
  (is (= "jabba/abba" (m/keyword->string :jabba/abba)))
  (is (= "abba" (m/keyword->string "abba"))))

(deftest parse-entry-syntax-test
  (let [{:keys [children entries forms]} (m/-parse-entry-syntax
                                           [[:x int?]
                                            [:y {:optional true, :title "boolean"} boolean?]] nil)]
    (testing "forms"
      (is (= [[:x 'int?]
              [:y {:optional true, :title "boolean"} 'boolean?]]
             forms)))
    (testing "entries"
      (is (entries= [[:x nil int?]
                     [:y {:optional true, :title "boolean"} boolean?]]
                    entries)))
    (testing "children"
      (is (= [2 3]
             (map count children)))))
  (is (thrown? #?(:clj Exception, :cljs js/Error)
               (m/-parse-entry-syntax
                 [[:x int?]
                  [:x boolean?]] nil))))

(deftest eval-test
  (is (= 2 ((m/eval inc) 1)))
  (is (= 2 ((m/eval 'inc) 1)))
  (is (= 2 ((m/eval '#(inc %)) 1)))
  (is (= 2 ((m/eval '#(inc %1)) 1)))
  (is (= 2 ((m/eval '(fn [x] (inc x))) 1)))
  (is (= 2 ((m/eval "(fn [x] (inc x))") 1)))
  (is (= {:district 9} (m/eval "(m/properties [int? {:district 9}])")))
  (is (= :maybe (m/eval "(m/type [:maybe int?])")))
  (is (= ['int? 'string?] (map m/form (m/eval "(m/children [:or {:some \"props\"} int? string?])"))))
  (is (entries= [[:x nil 'int?] [:y nil 'string?]] (m/eval "(m/map-entries [:map [:x int?] [:y string?]])"))))

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
                     :errors [(m/error [] [] schema "1")]}
                    (m/explain schema "1")))

      (is (= 1 (m/decode schema "1" mt/string-transformer)))
      (is (= "1" (m/decode schema "1" mt/json-transformer)))

      (is (= "olipa_kerran_avaruus"
             (m/decode
               [string? {:decode/string '{:enter #(str "olipa_" %), :leave #(str % "_avaruus")}}]
               "kerran" mt/string-transformer)))

      (is (true? (m/validate (over-the-wire schema) 1)))

      (is (= {:type 'int?}
             (m/walk schema m/map-syntax-walker)))

      (is (= 'int? (m/form schema)))))

  (testing "composite schemas"
    (let [schema (m/schema [:and int? [:or pos-int? neg-int?]])]

      (is (true? (m/validate schema 1)))
      (is (true? (m/validate schema -1)))
      (is (false? (m/validate schema 0)))
      (is (false? (m/validate schema "1")))
      (is (false? (m/validate schema [1])))

      (is (= pos-int? (m/validator [:and pos-int? pos-int? pos-int?])))
      (is (= pos-int? (m/validator [:or pos-int? pos-int? pos-int?])))

      (is (nil? (m/explain schema 1)))
      (is (results= {:schema schema,
                     :value 0,
                     :errors [{:path [2 1], :in [], :schema pos-int?, :value 0}
                              {:path [2 2], :in [], :schema neg-int?, :value 0}]}
                    (m/explain schema 0)))

      (is (= 1 (m/decode schema "1" mt/string-transformer)))
      (is (= "1" (m/decode schema "1" mt/json-transformer)))

      (is (= "olipa_kerran_avaruus"
             (m/decode
               [:and {:decode/string '{:enter #(str "olipa_" %), :leave #(str % "_avaruus")}} string?]
               "kerran" mt/string-transformer)))

      (is (true? (m/validate (over-the-wire schema) 1)))

      (is (= {:type :and
              :children [{:type 'int?}
                         {:type :or
                          :children [{:type 'pos-int?}
                                     {:type 'neg-int?}]}]}
             (m/walk schema m/map-syntax-walker)))

      (is (= [:and 'int? [:or 'pos-int? 'neg-int?]] (m/form schema))))

    (testing "transforming :or"
      (testing "first valid transformed branch is used"
        (are [input result]
          (is (= (m/decode
                   [:or
                    [:map [:x keyword?]]
                    int?
                    [:map [:y keyword?]]
                    keyword?]
                   input
                   mt/string-transformer)
                 result))

          {:x "true", :y "true"} {:x :true, :y "true"}
          {:x false, :y "true"} {:x false, :y :true}
          {:x false, :y false} {:x false, :y false}
          1 1
          "kikka" :kikka))

      (testing "top-level transformations are retained"
        (are [input result]
          (is (= (m/decode
                   (mu/closed-schema
                     [:or {:decode/string {:enter (fn [m] (update m :enter #(or % true)))
                                           :leave (fn [m] (update m :leave #(or % true)))}}
                      [:map
                       [:x keyword?]
                       [:enter boolean?]]
                      [:map
                       [:y keyword?]
                       [:enter boolean?]]])
                   input
                   mt/string-transformer)
                 result))

          {:x "true"} {:x :true, :enter true, :leave true}
          {:x "true", :enter "invalid"} {:x "true", :enter "invalid", :leave true}

          {:y "true"} {:y :true, :enter true, :leave true}
          {:y "true", :leave "invalid"} {:y "true", :enter true, :leave "invalid"}

          {:x "true", :y "true"} {:x "true", :y "true", :enter true, :leave true})))

    (testing "explain with branches"
      (let [schema [:and pos-int? neg-int?]]
        (is (results= {:schema schema,
                       :value -1,
                       :errors [{:path [1], :in [], :schema pos-int?, :value -1}]}
                      (m/explain schema -1))))
      (let [schema [:and pos-int? neg-int?]]
        (is (results= {:schema schema,
                       :value 1,
                       :errors [{:path [2], :in [], :schema neg-int?, :value 1}]}
                      (m/explain schema 1))))))

  (testing "comparator schemas"
    (let [schema (m/schema [:> 0])]

      (is (true? (m/validate schema 1)))
      (is (false? (m/validate schema 0)))
      (is (false? (m/validate schema "abba")))

      (is (nil? (m/explain schema 1)))
      (is (results= {:schema [:> 0], :value 0, :errors [{:path [], :in [], :schema [:> 0], :value 0}]}
                    (m/explain schema 0)))

      (is (= 1 (m/decode schema "1" mt/string-transformer)))
      (is (= "1" (m/decode schema "1" mt/json-transformer)))

      (is (= 4 (m/decode
                 [:> {:decode/string '{:enter inc, :leave (partial * 2)}} 0]
                 1 mt/string-transformer)))

      (is (true? (m/validate (over-the-wire schema) 1)))

      (is (= {:type :>, :children [0]}
             (m/walk schema m/map-syntax-walker)))

      (is (= [:> 0] (m/form schema)))))

  (testing "enum schemas"
    (let [schema (m/schema [:enum 1 2])]

      (is (true? (m/validate schema 1)))
      (is (false? (m/validate schema 0)))
      (is (false? (m/validate schema "abba")))

      (is (nil? (m/explain schema 1)))
      (is (results= {:schema [:enum 1 2], :value 0, :errors [{:path [0], :in [], :schema [:enum 1 2], :value 0}]}
                    (m/explain [:enum 1 2] 0)))

      ;; TODO: infer type from :enum
      #_(is (= 1 (m/decode schema "1" mt/string-transformer)))
      #_(is (= "1" (m/decode schema "1" mt/json-transformer)))

      (testing "map enums require nil properties"
        (let [schema [:enum nil {:a 1} {:b 2}]]
          (is (= nil (m/properties schema)))
          (is (= [{:a 1} {:b 2}] (m/children schema)))))

      (is (true? (m/validate (over-the-wire schema) 1)))

      (is (= {:type :enum, :children [1 2]}
             (m/walk schema m/map-syntax-walker)))

      (is (= [:enum 1 2] (m/form schema)))))

  (testing "maybe schemas"
    (let [schema (m/schema [:maybe int?])]

      (is (true? (m/validate schema 1)))
      (is (true? (m/validate schema nil)))
      (is (false? (m/validate schema "abba")))

      (is (nil? (m/explain schema 1)))
      (is (results= {:schema [:maybe int?], :value "abba", :errors [{:path [1], :in [], :schema int?, :value "abba"}]}
                    (m/explain [:maybe int?] "abba")))

      (is (= 1 (m/decode schema "1" mt/string-transformer)))
      (is (= "1" (m/decode schema "1" mt/json-transformer)))

      (is (= 4 (m/decode
                 [:maybe {:decode/string '{:enter inc, :leave (partial * 2)}} int?]
                 1 mt/string-transformer)))

      (is (true? (m/validate (over-the-wire schema) 1)))

      (is (= {:type :maybe, :children [{:type 'int?}]}
             (m/walk schema m/map-syntax-walker)))

      (is (= [:maybe 'int?] (m/form schema)))))

  (testing "string schemas"
    (let [schema (m/schema [:string {:min 1, :max 4}])]

      (is (true? (m/validate schema "abba")))
      (is (false? (m/validate schema "")))
      (is (false? (m/validate schema "invalid")))
      (is (false? (m/validate schema false)))

      (is (nil? (m/explain schema "1")))
      (is (results= {:schema [:string {:min 1, :max 4}]
                     :value false
                     :errors [{:path [], :in [], :schema [:string {:min 1, :max 4}], :value false}]}
                    (m/explain schema false)))
      (is (results= {:schema [:string {:min 1, :max 4}]
                     :value "invalid"
                     :errors [{:path [], :in [], :schema [:string {:min 1, :max 4}], :value "invalid"}]}
                    (m/explain schema "invalid")))

      (is (= "1" (m/decode schema "1" mt/string-transformer)))
      (is (= "1" (m/decode schema "1" mt/json-transformer)))

      (is (= "<-->" (m/decode
                      [:string {:decode/string {:enter #(str "<" %), :leave #(str % ">")}}]
                      "--" mt/string-transformer)))

      (is (true? (m/validate (over-the-wire schema) "123")))

      (is (= {:type :string, :properties {:min 1, :max 4}}
             (m/walk schema m/map-syntax-walker)))

      (is (= [:string {:min 1, :max 4}] (m/form schema)))))

  (testing "ref schemas"

    (testing "recursion"
      (let [ConsCell [:schema {:registry {::cons [:maybe [:tuple int? [:ref ::cons]]]}}
                      ::cons]]

        (is (true? (m/validate ConsCell [1 nil])))
        (is (true? (m/validate ConsCell [1 [2 nil]])))
        (is (false? (m/validate ConsCell [1 [2]])))

        (is (nil? (m/explain ConsCell [1 [2 nil]])))
        (is (results= {:schema ConsCell
                       :value [1 [2]]
                       :errors [{:in [1]
                                 :path [1 2 0 1]
                                 :schema (mu/get ConsCell 0)
                                 :type :malli.core/tuple-size
                                 :value [2]}]}
                      (m/explain ConsCell [1 [2]])))

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
               (m/walk ConsCell m/map-syntax-walker)))

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
                     :errors [{:path [2 1] :in [] :schema pos-int?, :value -1}
                              {:path [2 2], :in [], :schema pos-int?, :value -1}
                              {:path [2 3], :in [], :schema pos-int?, :value -1}]}

                    (m/explain schema -1)))

      (is (= 1 (m/decode schema "1" mt/string-transformer)))
      (is (= "1" (m/decode schema "1" mt/json-transformer)))

      (is (= "$kikka"
             (m/decode
               [:schema {:decode/custom (partial str "$")} string?] "kikka"
               (mt/transformer {:name :custom}))))

      (testing "deref"
        (is (mu/equals (m/schema int?) (m/-deref (m/schema [:schema int?])))))

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
             (m/walk schema m/map-syntax-walker)))

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

        (is (= 4 (m/decode
                   [:re {:decode/string '{:enter inc, :leave (partial * 2)}} ".*"]
                   1 mt/string-transformer)))

        (is (true? (m/validate (over-the-wire schema) "a.b")))

        (is (= {:type :re, :children [re]}
               (m/walk schema m/map-syntax-walker)))

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

        (is (= 4 (m/decode
                   [:fn {:decode/string '{:enter inc, :leave (partial * 2)}} 'int?]
                   1 mt/string-transformer)))

        (is (true? (m/validate (over-the-wire schema) 12)))

        (is (= {:type :fn
                :children [fn]
                :properties {:description "number between 10 and 18"}}
               (m/walk schema m/map-syntax-walker)))

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

      (is (entries=
            [[:x nil boolean?]
             [:y {:optional true} int?]
             [:z {:optional false} string?]]
            (m/map-entries schema)))

      (is (results= {:schema schema
                     :value {:y "invalid" :z "kikka"}
                     :errors
                     [{:path [1 0], :in [:x], :schema schema, :type ::m/missing-key}
                      {:path [2 2], :in [:y], :schema int?, :value "invalid"}]}
                    (m/explain schema {:y "invalid" :z "kikka"})))

      (is (results= {:schema schema,
                     :value {:x "invalid"},
                     :errors [{:path [1 1], :in [:x], :schema boolean?, :value "invalid"}
                              {:path [3 0],
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
                     :errors [{:path [],
                               :in [:extra],
                               :schema closed-schema,
                               :value nil,
                               :type :malli.core/extra-key,
                               :message nil}]}
                    (m/explain closed-schema valid-with-extras)))

      (is (= {:x true} (m/decode schema {:x "true"} mt/string-transformer)))
      (is (= {:x true, :y 1} (m/decode schema {:x "true", :y "1"} mt/string-transformer)))
      (is (= {:x "true", :y "1"} (m/decode schema {:x "true", :y "1"} mt/json-transformer)))
      (is (= {:x true, :y 1} (m/decode schema {:x true, :y 1, :a 1} mt/strip-extra-keys-transformer)))
      (is (= {:x_key true, :y_key 2} (m/decode schema {:x true, :y 2}
                                               (mt/key-transformer
                                                 {:decode #(-> % name (str "_key") keyword)}))))

      (is (= {:x 24}
             (m/decode
               [:map
                {:decode/string '{:enter #(update % :x inc), :leave #(update % :x (partial * 2))}}
                [:x [int? {:decode/string '{:enter (partial + 2), :leave (partial * 3)}}]]]
               {:x 1} mt/string-transformer)))

      (is (true? (m/validate (over-the-wire schema) valid)))

      (is (= {:type :map
              :children [[:x nil {:type 'boolean?}]
                         [:y {:optional true} {:type 'int?}]
                         [:z {:optional false} {:type 'string?}]]}
             (m/walk schema m/map-syntax-walker)))

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
                     :errors [{:path [2 1 2 1], :in [:size], :schema int?, :value "size"}]}
                    (m/explain schema invalid1)))

      (is (results= {:schema schema,
                     :value {:type :human, :namez "inkeri"},
                     :errors [{:path [3 1 2 0]
                               :in [:name]
                               :schema [:map [:type keyword?] [:name string?] [:address [:map [:country keyword?]]]]
                               :type :malli.core/missing-key}
                              {:path [3 1 3 0]
                               :in [:address]
                               :schema [:map [:type keyword?] [:name string?] [:address [:map [:country keyword?]]]]
                               :type :malli.core/missing-key}]}
                    (m/explain schema invalid2)))

      (is (results= {:schema schema,
                     :value {:type :worm}
                     :errors [{:path []
                               :in []
                               :schema schema
                               :value {:type :worm}
                               :type :malli.core/invalid-dispatch-value}]}
                    (m/explain schema invalid3)))

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


             (m/walk schema m/map-syntax-walker)))

      (is (entries= [[:sized nil [:map [:type keyword?] [:size int?]]]
                     [:human nil [:map [:type keyword?] [:name string?] [:address [:map [:country keyword?]]]]]]
                    (m/map-entries schema)))

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
                   :errors [{:path [1],
                             :in [:age],
                             :schema string?,
                             :value :age}]}
                  (m/explain [:map-of string? int?] {:age 18})))
    (is (results= {:schema [:map-of string? int?],
                   :value {:age "18"},
                   :errors [{:path [1],
                             :in [:age],
                             :schema string?,
                             :value :age}
                            {:path [2],
                             :in [:age],
                             :schema int?,
                             :value "18"}]}
                  (m/explain [:map-of string? int?] {:age "18"})))

    (is (= {1 1} (m/decode [:map-of int? pos-int?] {"1" "1"} mt/string-transformer)))

    (is (= {:x 24}
           (m/decode
             [:map-of {:decode/string '{:enter #(update % :x inc), :leave #(update % :x (partial * 2))}}
              keyword? [int? {:decode/string '{:enter (partial + 2), :leave (partial * 3)}}]]
             {:x 1} mt/string-transformer)))

    (is (true? (m/validate (over-the-wire [:map-of string? int?]) {"age" 18})))

    (is (= {:type :map-of, :children [{:type 'int?} {:type 'pos-int?}]}
           (m/walk [:map-of int? pos-int?] m/map-syntax-walker)))

    (testing "keyword keys are transformed via strings"
      (is (= {1 1} (m/decode [:map-of int? pos-int?] {:1 "1"} mt/string-transformer)))))

  (testing "sequence schemas"

    (testing "empty schemas fail"
      (doseq [element [:vector :list :sequential :set :tuple]]
        (is (thrown? #?(:clj Exception, :cljs js/Error) (m/schema [element])))))

    (testing "more than 1 elements fail on collections"
      (doseq [element [:vector :list :sequential :set]]
        (is (thrown? #?(:clj Exception, :cljs js/Error) (m/schema [element int? int?])))))

    (testing "validation"
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

                          "list" [[true [:list int?] '(1 2 3)]
                                  [false [:list int?] '(1 "2" 3)]
                                  [false [:list int?] '(1 2 "3")]
                                  [false [:list int?] nil]
                                  [false [:list int?] '(nil)]
                                  [false [:list int?] "invalid"]

                                  [true [:list {:min 3} int?] '(1 2 3)]
                                  [false [:list {:min 4} int?] '(1 2 3)]

                                  [true [:list {:max 3} int?] '(1 2 3)]
                                  [false [:list {:max 2} int?] '(1 2 3)]

                                  [true [:list {:min 1, :max 3} int?] '(1 2 3)]
                                  [false [:list {:min 4, :max 4} int?] '(1 2 3)]

                                  [false [:list int?] [1 2 3]]
                                  [false [:list int?] #{1 2 3}]]

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
            (is (= expected (m/validate (over-the-wire schema) value)))))))

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
                                        :errors [{:path [2], :in [2], :schema int?, :value "3"}]}]])

                          "list" (let [schema [:list {:min 2, :max 3} int?]]

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
                                      :errors [{:path [2], :in [2], :schema int?, :value "3"}]}]])

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
                                     :errors [{:path [2], :in [0], :schema int?, :value "3"}]}]])

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
                                       :errors [{:path [2], :in [1], :schema string?, :value 2}]}]])
                          "map+enum" (let [schema [:map [:x [:enum "x"]]
                                                   [:y [:enum "y"]]]]

                                       [[schema {:x "x" :y "y"}
                                         nil]

                                        [schema {:x "non-x" :y "y"}
                                         {:schema schema
                                          :value {:x "non-x" :y "y"}
                                          :errors [{:path [1 1 0], :in [:x], :schema [:enum "x"], :value "non-x"}]}]

                                        [schema {:x "x" :y "non-y"}
                                         {:schema schema
                                          :value {:x "x" :y "non-y"}
                                          :errors [{:path [2 1 0], :in [:y], :schema [:enum "y"], :value "non-y"}]}]

                                        [schema {:x "non-x" :y "non-y"}
                                         {:schema schema
                                          :value {:x "non-x" :y "non-y"}
                                          :errors [{:path [1 1 0], :in [:x], :schema [:enum "x"], :value "non-x"}
                                                   {:path [2 1 0], :in [:y], :schema [:enum "y"], :value "non-y"}]}]])}
            expectations (assoc expectations "sequential" (concat (get expectations "list") (get expectations "vector")))]

        (doseq [[name data] expectations
                [schema value expected] data]
          (testing name
            (is (results= expected (m/explain schema value)))))))

    (testing "visit"
      (doseq [name [:vector :list :sequential :set]]
        (is (= {:type name, :children [{:type 'int?}]}
               (m/walk [name int?] m/map-syntax-walker))))
      (is (= {:type :tuple, :children [{:type 'int?} {:type 'int?}]}
             (m/walk [:tuple int? int?] m/map-syntax-walker))))))

(deftest path-with-properties-test
  (let [?path #(-> % :errors first :path)]

    (is (= [1] (?path (m/explain [:and int?] "2"))))
    (is (= [2] (?path (m/explain [:and {:name "int?"} int?] "2"))))

    (is (= [1] (?path (m/explain [:vector int?] ["2"]))))
    (is (= [2] (?path (m/explain [:vector {:name "int?"} [int?]] ["2"]))))

    (is (= [1] (?path (m/explain [:tuple int?] ["2"]))))
    (is (= [2] (?path (m/explain [:tuple {:name "int?"} [int?]] ["2"]))))

    (is (= [1 1] (?path (m/explain [:map [:x int?]] {:x "1"}))))
    (is (= [2 1] (?path (m/explain [:map {:name int?} [:x int?]] {:x "1"}))))
    (is (= [2 2] (?path (m/explain [:map {:name int?} [:x {:optional false} int?]] {:x "1"}))))))

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
                   {:int (m/fn-schema :int int?)
                    :string (m/fn-schema :string string?)})]
    (is (true? (m/validate [:or :int :string] 123 {:registry registry})))
    (is (false? (m/validate [:or :int :string] 'kikka {:registry registry})))))

(deftest encode-decode-test
  (testing "works with custom registry"
    (let [opts {:registry (merge (m/default-schemas) {:test keyword?})}
          encoded (m/encode :test :foo opts mt/string-transformer)
          decoded (m/decode :test encoded opts mt/string-transformer)]
      (is (= "foo" encoded))
      (is (= :foo decoded)))))

(def sequential (#'m/-collection-schema `sequential sequential? seq nil))

(deftest custom-into-schema-test
  (doseq [value [[1 2 3] '(1 2 3)]]
    (is (= true (m/validate [sequential int?] value)))))

(deftest walker-in-test
  (is (form= [:map {:in []}
              [:id [string? {:in [:id]}]]
              [:tags [:set {:in [:tags]} [keyword? {:in [:tags :malli.core/in]}]]]
              [:address
               [:maybe {:in [:address]}
                [:vector {:in [:address]}
                 [:map {:in [:address :malli.core/in]}
                  [:street [string? {:in [:address :malli.core/in :street]}]]
                  [:lonlat
                   [:tuple {:in [:address :malli.core/in :lonlat]}
                    [double? {:in [:address :malli.core/in :lonlat 0]}]
                    [double? {:in [:address :malli.core/in :lonlat 1]}]]]]]]]]
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
               (fn [schema children in options]
                 (m/into-schema
                   (m/type schema)
                   (assoc (m/properties schema) :in in)
                   children
                   options))))))

(deftest to-from-maps-test
  (let [schema [:map {:registry {::size [:enum "S" "M" "L"]}}
                [:id string?]
                [:tags [:set keyword?]]
                [:size ::size]
                [:address
                 [:vector
                  [:map
                   [:street string?]
                   [:lonlat [:tuple double? double?]]]]]]]

    (testing "to-map-syntax"
      (is (= {:type :map,
              :properties {:registry {::size [:enum "S" "M" "L"]}}
              :children [[:id nil {:type 'string?}]
                         [:tags nil {:type :set
                                     :children [{:type 'keyword?}]}]
                         [:size nil {:type ::m/schema
                                     :children [::size]}]
                         [:address nil {:type :vector,
                                        :children [{:type :map,
                                                    :children [[:street nil {:type 'string?}]
                                                               [:lonlat nil {:type :tuple
                                                                             :children [{:type 'double?} {:type 'double?}]}]]}]}]]}
             (m/to-map-syntax schema))))

    (testing "from-map-syntax"
      (is (true? (mu/equals schema (-> schema (m/to-map-syntax) (m/from-map-syntax))))))))
