(ns malli.core-test
  (:require [clojure.test :refer [deftest testing is are]]
            [malli.core :as m]
            [malli.edn :as me]
            [malli.transform :as mt]))

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

(deftest expand-key-test
  (are [schema expected]
    (= expected (second (#'m/-expand-key schema nil identity)))

    [:x int?] nil
    [:x {:optional true} int?] {:optional true}
    [:x {:optional false} int?] {:optional false}))

(deftest eval-test
  (is (= 2 ((m/eval inc) 1)))
  (is (= 2 ((m/eval 'inc) 1)))
  (is (= 2 ((m/eval '#(inc %)) 1)))
  (is (= 2 ((m/eval '#(inc %1)) 1)))
  (is (= 2 ((m/eval '(fn [x] (inc x))) 1)))
  (is (= 2 ((m/eval "(fn [x] (inc x))") 1)))
  (is (= {:district 9} (m/eval "(m/properties [int? {:district 9}])")))
  (is (= :maybe (m/eval "(m/name [:maybe int?])")))
  (is (= ['int? 'string?] (m/eval "(m/children [:or {:some \"props\"} int? string?])")))
  (is (entries= [[:x nil 'int?] [:y nil 'string?]] (m/eval "(m/map-entries [:map [:x int?] [:y string?]])"))))

(deftest into-schema-test
  (is (form= [:map {:closed true} [:x int?]]
             (m/into-schema :map {:closed true} [[:x int?]]))))

(deftest schema-visitor-test
  (is (form= [:map {:closed true} [:x int?]]
             (m/accept [:map {:closed true} [:x int?]] (m/schema-visitor identity)))))

(deftest validation-test

  (testing "coercion"
    (is (= true
           (m/validate int? 1)
           (m/validate (m/schema int?) 1)
           (m/validate (get m/default-registry int?) 1)
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

      (is (= {:name 'int?}
             (m/accept schema m/map-syntax-visitor)))

      (is (= 'int? (m/form schema)))))

  (testing "composite schemas"
    (let [schema (m/schema [:and int? [:or pos-int? neg-int?]])]

      (is (true? (m/validate schema 1)))
      (is (true? (m/validate schema -1)))
      (is (false? (m/validate schema 0)))
      (is (false? (m/validate schema "1")))
      (is (false? (m/validate schema [1])))

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

      (is (= {:name :and
              :children [{:name 'int?}
                         {:name :or
                          :children [{:name 'pos-int?}
                                     {:name 'neg-int?}]}]}
             (m/accept schema m/map-syntax-visitor)))

      (is (= [:and 'int? [:or 'pos-int? 'neg-int?]] (m/form schema))))

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

      (is (= {:name :>, :children [0]}
             (m/accept schema m/map-syntax-visitor)))

      (is (= [:> 0] (m/form schema)))))

  (testing "enum schemas"
    (let [schema (m/schema [:enum 1 2])]

      (is (true? (m/validate schema 1)))
      (is (false? (m/validate schema 0)))
      (is (false? (m/validate schema "abba")))

      (is (nil? (m/explain schema 1)))
      (is (results= {:schema [:enum 1 2], :value 0, :errors [{:path [], :in [], :schema [:enum 1 2], :value 0}]}
                    (m/explain schema 0)))

      ;; TODO: infer type from :enum
      #_(is (= 1 (m/decode schema "1" mt/string-transformer)))
      #_(is (= "1" (m/decode schema "1" mt/json-transformer)))

      (testing "map enums require nil properties"
        (let [schema [:enum nil {:a 1} {:b 2}]]
          (is (= nil (m/properties schema)))
          (is (= [{:a 1} {:b 2}] (m/children schema)))))

      (is (true? (m/validate (over-the-wire schema) 1)))

      (is (= {:name :enum, :children [1 2]}
             (m/accept schema m/map-syntax-visitor)))

      (is (= [:enum 1 2] (m/form schema)))))

  (testing "maybe schemas"
    (let [schema (m/schema [:maybe int?])]

      (is (true? (m/validate schema 1)))
      (is (true? (m/validate schema nil)))
      (is (false? (m/validate schema "abba")))

      (is (nil? (m/explain schema 1)))
      (is (results= {:schema [:maybe int?], :value "abba", :errors [{:path [], :in [], :schema [:maybe int?], :value "abba"}]}
                    (m/explain schema "abba")))

      (is (= 1 (m/decode schema "1" mt/string-transformer)))
      (is (= "1" (m/decode schema "1" mt/json-transformer)))

      (is (= 4 (m/decode
                 [:maybe {:decode/string '{:enter inc, :leave (partial * 2)}} int?]
                 1 mt/string-transformer)))

      (is (true? (m/validate (over-the-wire schema) 1)))

      (is (= {:name :maybe, :children [{:name 'int?}]}
             (m/accept schema m/map-syntax-visitor)))

      (is (= [:maybe 'int?] (m/form schema)))))

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

        (is (= {:name :re, :children [re]}
               (m/accept schema m/map-syntax-visitor)))

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

        (is (= {:name :fn
                :children [fn]
                :properties {:description "number between 10 and 18"}}
               (m/accept schema m/map-syntax-visitor)))

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

      (is (= {:name :map
              :children [[:x nil {:name 'boolean?}]
                         [:y {:optional true} {:name 'int?}]
                         [:z {:optional false} {:name 'string?}]]}
             (m/accept schema m/map-syntax-visitor)))

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

      (is (= {:name :multi
              :properties {:dispatch :type, :decode/string '(fn [x] (update x :type keyword))}
              :children [[:sized nil {:name :map
                                      :children [[:type nil {:name 'keyword?}]
                                                 [:size nil {:name 'int?}]]}]
                         [:human nil {:name :map
                                      :children [[:type nil {:name 'keyword?}]
                                                 [:name nil {:name 'string?}]
                                                 [:address nil {:name :map
                                                                :children [[:country nil {:name 'keyword?}]]}]]}]]}


             (m/accept schema m/map-syntax-visitor)))

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

    (is (= {:name :map-of, :children [{:name 'int?} {:name 'pos-int?}]}
           (m/accept [:map-of int? pos-int?] m/map-syntax-visitor)))

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

                          "cat" [; [:seq-of ...] wrapper is implicitly used around [:cat ...]
                                 [true [:cat [:x int?]] '(1)]
                                 [true [:cat [:x int?]] [1]]

                                 [true [:seq-of [:cat [:x int?]]] '(1)]
                                 [true [:seq-of [:cat [:x int?]]] [1]]

                                 [true [:list-of [:cat [:x int?]]] '(1)]
                                 [false [:list-of [:cat [:x int?]]] [1]]

                                 [false [:vector-of [:cat [:x int?]]] '(1)]
                                 [true [:vector-of [:cat [:x int?]]] [1]]

                                 ; concatenated sequences
                                 [true [:cat [:a [:cat [:ax int?] [:ay int?]]]
                                             [:b [:cat [:bx int?] [:by int?]]]] [1 2 3 4]]

                                 ; sequence hierarchy
                                 ; :seq-of is needed explicitly when used as element of a :cat sequence
                                 [true [:cat [:a [:seq-of [:cat [:ax int?] [:ay int?]]]]
                                             [:b [:seq-of [:cat [:bx int?] [:by int?]]]] ['(1 2) [3 4]]]]
                                 [true [:cat [:a [:list-of [:cat [:ax int?] [:ay int?]]]]
                                             [:b [:vector-of [:cat [:bx int?] [:by int?]]]] ['(1 2) [3 4]]]]

                                 ; nameless variant, aka :cat-
                                 [true [:cat {:named false} int? int? [:* string?]] [1 2 "a" "b"]]

                                 ; :?, :+ and :* are all alias of [:repeat {:min a, :max b} ...]

                                 [true [:cat [:x [:? int?] [:y [string?]] ["a"]]]]
                                 [true [:cat [:x [:? int?] [:y [string?]] [1 "a"]]]]
                                 [false [:cat [:x [:? int?] [:y [string?]] [1 2 "a"]]]]

                                 [false [:cat [:x [:+ int?] [:y [string?]] ["a"]]]]
                                 [true [:cat [:x [:+ int?] [:y [string?]] [1 "a"]]]]
                                 [true [:cat [:x [:+ int?] [:y [string?]] [1 2 "a"]]]]

                                 [true [:cat [:x [:* int?] [:y [string?]] ["a"]]]]
                                 [true [:cat [:x [:* int?] [:y [string?]] [1 "a"]]]]
                                 [true [:cat [:x [:* int?] [:y [string?]] [1 2 "a"]]]]

                                 [true [:cat [:x [:? [:cat [:s string?] [:i int?]]]]] []]
                                 [true [:cat [:x [:? [:cat [:s string?] [:i int?]]]]] ["a" 1]]
                                 [false [:cat [:x [:? [:cat [:s string?] [:i int?]]]]] ["a" 1 "b" 2]]

                                 [false [:cat [:x [:+ [:cat [:s string?] [:i int?]]]]] []]
                                 [true [:cat [:x [:+ [:cat [:s string?] [:i int?]]]]] ["a" 1]]
                                 [true [:cat [:x [:+ [:cat [:s string?] [:i int?]]]]] ["a" 1 "b" 2]]

                                 [true [:cat [:x [:* [:cat [:x string?] [:y int?]]]]] []]
                                 [true [:cat [:x [:* [:cat [:x string?] [:y int?]]]]] ["a" 1]]
                                 [true [:cat [:x [:* [:cat [:x string?] [:y int?]]]]] ["a" 1 "b" 2]]

                                 [true [:cat [:x int?] [:y int?]] [1 2]]
                                 [true [:cat [:x int?] [:y int?] [:rest [:* string?]]] [1 2]]
                                 [true [:cat [:x int?] [:y int?] [:rest [:* string?]]] [1 2 "a" "b"]]
                                 [false [:cat [:x int?] [:y int?] [:rest [:* string?]]] [1 "2"]]
                                 [false [:cat [:x int?] [:y int?] [:rest [:* string?]]] [1 2 3]]]

                          "alt" [; [:cat- ...] wrapper is implicitly used around [:alt ...]
                                 [true [:alt [:kind1 [:cat [:x int?] [:y boolean?]]]
                                             [:kind2 [:cat [:a int?] [:b string?] [:c int?]]] [1 true]]]
                                 [true [:alt [:kind1 [:cat [:x int?] [:y boolean?]]]
                                             [:kind2 [:cat [:a int?] [:b string?] [:c int?]]] [1 "a" 2]]]
                                 [false [:alt [:kind1 [:cat [:x int?] [:y boolean?]]]
                                              [:kind2 [:cat [:a int?] [:b string?] [:c int?]]] [1 2]]]
                                 [false [:alt [:kind1 [:cat [:x int?] [:y boolean?]]]
                                              [:kind2 [:cat [:a int?] [:b string?] [:c int?]]] [1 "a" 2 3]]]

                                 ; Note:
                                 ;   [:alt [:s string?] [:b boolean?]] is implicitly treated as
                                 ;   [:alt [:s [:cat- string?]] [:b [:cat- boolean?]]]
                                 [true [:cat [:x int?] [:y [:alt [:s string?] [:b boolean?]]]] [1 "a"]]
                                 [true [:cat [:x int?] [:y [:alt [:s string?] [:b boolean?]]]] [1 false]]
                                 [false [:cat [:x int?] [:y [:alt [:s string?] [:b boolean?]]]] [1 2]]
                                 [false [:cat [:x int?] [:y [:alt [:s string?] [:b boolean?]]]] [1 "a" 2]]
                                 [false [:cat [:x int?] [:y [:alt [:s string?] [:b boolean?]]]] [1]]

                                 ; aka [:cat- [:alt- ...]]
                                 [true [:cat {:named false} [:alt {:named false} string? boolean?]]] ["a"]]

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
                                          :errors [{:path [1 1], :in [:x], :schema [:enum "x"], :value "non-x"}]}]

                                        [schema {:x "x" :y "non-y"}
                                         {:schema schema
                                          :value {:x "x" :y "non-y"}
                                          :errors [{:path [2 1], :in [:y], :schema [:enum "y"], :value "non-y"}]}]

                                        [schema {:x "non-x" :y "non-y"}
                                         {:schema schema
                                          :value {:x "non-x" :y "non-y"}
                                          :errors [{:path [1 1], :in [:x], :schema [:enum "x"], :value "non-x"}
                                                   {:path [2 1], :in [:y], :schema [:enum "y"], :value "non-y"}]}]])}
            expectations (assoc expectations "sequential" (concat (get expectations "list") (get expectations "vector")))]

        (doseq [[name data] expectations
                [schema value expected] data]
          (testing name
            (is (results= expected (m/explain schema value)))))))

    (testing "visit"
      (doseq [name [:vector :list :sequential :set]]
        (is (= {:name name, :children [{:name 'int?}]}
               (m/accept [name int?] m/map-syntax-visitor))))
      (is (= {:name :tuple, :children [{:name 'int?} {:name 'int?}]}
             (m/accept [:tuple int? int?] m/map-syntax-visitor))))))

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
    (is (= ['int? 'pos-int?]
           (m/children [:and {:a 1} int? pos-int?])
           (m/children [:and {} int? pos-int?])
           (m/children [:and int? pos-int?])))))

(deftest options-test
  (testing "options can be set and retrieved"
    (let [opts {:tyyris "tyllero"
                :registry m/default-registry}
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
                   m/comparator-registry
                   m/base-registry
                   {:int (m/fn-schema :int int?)
                    :string (m/fn-schema :string string?)})]
    (is (true? (m/validate [:or :int :string] 123 {:registry registry})))
    (is (false? (m/validate [:or :int :string] 'kikka {:registry registry})))))

(deftest encode-decode-test
  (testing "works with custom registry"
    (let [opts {:registry (merge m/default-registry {:test keyword?})}
          encoded (m/encode :test :foo opts mt/string-transformer)
          decoded (m/decode :test encoded opts mt/string-transformer)]
      (is (= "foo" encoded))
      (is (= :foo decoded)))))

(def sequential (#'m/-collection-schema `sequential sequential? seq nil))

(deftest custom-into-schema-test
  (doseq [value [[1 2 3] '(1 2 3)]]
    (is (= true (m/validate [sequential int?] value)))))

(deftest visitor-in-test
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
             (m/accept
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
                   (m/name schema)
                   (assoc (m/properties schema) :in in)
                   children
                   options))))))
