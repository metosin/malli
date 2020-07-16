(ns malli.transform-test
  (:require [clojure.test :refer [deftest testing is are]]
            [malli.core :as m]
            [malli.transform :as mt]
            [clojure.string :as str]))

(deftest ->interceptor-test
  (are [?interceptor expected]
    (= expected (is (#'mt/-interceptor ?interceptor {} {})))

    inc {:enter inc}
    {:enter inc} {:enter inc}
    {:leave dec} {:leave dec}
    {:enter inc, :leave dec} {:enter inc, :leave dec}
    {:compile (constantly inc)} {:enter inc}
    {:compile (constantly {:enter inc, :leave dec})} {:enter inc, :leave dec})

  (let [?interceptor {:compile (constantly {:compile (constantly inc)})}]
    (testing "shallow compilation succeeds"
      (binding [mt/*max-compile-depth* 2]
        (is (= {:enter inc} (#'mt/-interceptor ?interceptor {} {})))))
    (testing "too deep compilation fails"
      (binding [mt/*max-compile-depth* 1]
        (is (thrown? #?(:clj Exception, :cljs js/Error) (#'mt/-interceptor ?interceptor {} {})))))))

(deftest string->long
  (is (= 1 (mt/-string->long "1")))
  (is (= 1 (mt/-string->long 1)))
  (is (= "abba" (mt/-string->long "abba"))))

(deftest string->double
  (is (= 1.0 (mt/-string->double "1")))
  (is (= 1.0 (mt/-string->double 1.0)))
  (is (= 1 (mt/-string->double 1)))
  (is (= "abba" (mt/-string->double "abba"))))

(deftest string->keyword
  (is (= :abba (mt/-string->keyword "abba")))
  (is (= :abba (mt/-string->keyword :abba))))

(deftest string->boolean
  (is (= true (mt/-string->boolean "true")))
  (is (= false (mt/-string->boolean "false")))
  (is (= "abba" (mt/-string->boolean "abba"))))

(deftest string->uuid
  (is (= #uuid"5f60751d-9bf7-4344-97ee-48643c9949ce" (mt/-string->uuid "5f60751d-9bf7-4344-97ee-48643c9949ce")))
  (is (= #uuid"5f60751d-9bf7-4344-97ee-48643c9949ce" (mt/-string->uuid #uuid"5f60751d-9bf7-4344-97ee-48643c9949ce")))
  (is (= "abba" (mt/-string->uuid "abba"))))

(deftest string->date
  (is (= #inst "2018-04-27T18:25:37Z" (mt/-string->date "2018-04-27T18:25:37Z")))
  (is (= #inst "2018-04-27T00:00:00Z" (mt/-string->date "2018-04-27")))
  (is (= #inst "2018-04-27T05:00:00Z" (mt/-string->date "2018-04-27T08:00:00+03:00")))
  (is (= #inst "2018-04-27T18:25:37Z" (mt/-string->date "2018-04-27T18:25:37.000Z")))
  (is (= #inst "2018-04-27T18:25:37Z" (mt/-string->date "2018-04-27T18:25:37.000+0000")))
  (is (= #inst "2014-02-18T18:25:37Z" (mt/-string->date #inst "2014-02-18T18:25:37Z")))
  (is (= #inst "2018-04-27T00:00:00Z" (mt/-string->date #inst "2018-04-27")))
  (is (= #inst "2018-04-27T05:00:00Z" (mt/-string->date #inst "2018-04-27T08:00:00+03:00")))
  (is (= "abba" (mt/-string->date "abba"))))

#?(:clj
   (deftest string->decimal
     (is (= 42M (mt/-string->decimal "42")))
     (is (= 42.24M (mt/-string->decimal "42.24")))
     (is (= nil (mt/-string->decimal nil)))
     (is (= "42.42M" (mt/-string->decimal "42.42M")))))

(deftest date->string
  (is (= "2014-02-18T18:25:37.000Z" (mt/-date->string #inst "2014-02-18T18:25:37Z")))
  (is (= "abba" (mt/-date->string "abba"))))

(deftest string->symbol
  (is (= 'inc (mt/-string->symbol "inc")))
  (is (= 'inc (mt/-string->symbol 'inc))))

(deftest string->nil
  (is (= nil (mt/-string->nil "")))
  (is (= nil (mt/-string->nil nil))))

(deftest number->double
  #?(:clj (is (= 0.5 (mt/-number->double 1/2))))
  (is (= 1.0 (mt/-number->double 1)))
  (is (= "kikka" (mt/-number->double "kikka"))))

(deftest any->string
  #?(:clj (is (= "1/2" (mt/-any->string 1/2))))
  (is (= "0.5" (mt/-any->string 0.5)))
  (is (= nil (mt/-any->string nil))))

(deftest any->any
  #?(:clj (is (= 1/2 (mt/-any->any 1/2))))
  (is (= 0.5 (mt/-any->any 0.5)))
  (is (= nil (mt/-any->any nil))))

(deftest transform-test

  (testing "predicates"
    (testing "decode"
      (is (= 1 (m/decode int? "1" mt/string-transformer)))
      (is (= "1abc" (m/decode int? "1abc" mt/string-transformer)))
      (is (= "+1-2" (m/decode int? "+1-2" mt/string-transformer)))
      (is (= 1 (m/decode int? "+1" mt/string-transformer)))
      (is (= -1 (m/decode int? "-1" mt/string-transformer)))
      (is (= "1" (m/decode int? "1" mt/json-transformer)))
      (is (= 1.0 (m/decode double? 1 mt/json-transformer)))
      (is (= 1 (m/decode double? 1 mt/string-transformer)))
      (is (= :user/kikka (m/decode keyword? "user/kikka" mt/string-transformer))))
    (testing "encode"
      (is (= "1" (m/encode int? 1 mt/string-transformer)))
      (is (= 1 (m/encode int? 1 mt/json-transformer)))
      (is (= "user/kikka" (m/encode keyword? :user/kikka mt/string-transformer)))))

  (testing "comparators"
    (testing "decode"
      (doseq [schema (keys (m/comparator-schemas))]
        (is (= 1 (m/decode [schema 1] "1" mt/string-transformer)))))
    (testing "encode"
      (doseq [schema (keys (m/comparator-schemas))]
        (is (= "1" (m/encode [schema 1] 1 mt/string-transformer))))))

  (testing "and"
    (testing "decode"
      (is (= 1 (m/decode [:and int?] "1" mt/string-transformer)))
      (is (= :1 (m/decode [:and keyword?] "1" mt/string-transformer)))
      (is (= 1 (m/decode [:and int? keyword?] "1" mt/string-transformer)))
      (is (= 1 (m/decode [:and int? [:enum 1 2]] "1" mt/string-transformer)))
      (is (= :1 (m/decode [:and keyword? int?] "1" mt/string-transformer)))
      (is (= [1] (m/decode [:and [:vector int?]] ["1"] mt/string-transformer))))
    (testing "encode"
      (is (= "1" (m/encode [:and int?] 1 mt/string-transformer)))
      (is (= "1" (m/encode [:and keyword?] :1 mt/string-transformer)))
      (is (= "1" (m/encode [:and int? keyword?] 1 mt/string-transformer)))
      (is (= "1" (m/encode [:and int? [:enum 1 2]] 1 mt/string-transformer)))
      (is (= "1" (m/encode [:and keyword? int?] :1 mt/string-transformer)))
      (is (= ["1"] (m/encode [:and [:vector int?]] [1] mt/string-transformer)))))

  (testing "or"
    (testing "decode"
      (is (= 1 (m/decode [:or int? keyword?] "1" mt/string-transformer)))
      (is (= 1 (m/decode [:or int? [:enum 1 2]] "1" mt/string-transformer)))
      (is (= :1 (m/decode [:or keyword? int?] "1" mt/string-transformer))))
    (testing "encode"
      (is (= "1" (m/encode [:or int? keyword?] 1 mt/string-transformer)))
      (is (= "1" (m/encode [:or int? [:enum 1 2]] 1 mt/string-transformer)))
      (is (= "1" (m/encode [:or keyword? int?] 1 mt/string-transformer)))))

  ;; TODO: encode
  (testing "collections"
    (doseq [t [mt/string-transformer mt/json-transformer]]
      (testing "string transformer"
        (are [schema value expected f]
          (do
            (is (= expected (m/decode schema value t)))
            (is (f (m/decode schema value t))))

          [:set [:enum 1 2]] ["1" 2 "3"] #{"1" 2 "3"} set?

          [:set keyword?] nil nil nil?
          [:set keyword?] ["1" 2 "3"] #{:1 2 :3} set?
          [:set keyword?] '("1" 2 "3") #{:1 2 :3} set?
          [:set keyword?] (lazy-seq '("1" 2 "3")) #{:1 2 :3} set?
          [:set keyword?] ::invalid ::invalid keyword?

          [:set string?] ["1" 2 "3"] #{"1" 2 "3"} set?
          [:set string?] #{"1" 2 "3"} #{"1" 2 "3"} set?

          [:vector keyword?] nil nil nil?
          [:vector keyword?] ["1" 2 "3"] [:1 2 :3] vector?
          [:vector keyword?] '("1" 2 "3") [:1 2 :3] vector?
          [:vector keyword?] (lazy-seq '("1" 2 "3")) [:1 2 :3] vector?

          [:vector string?] ["1" 2 "3"] ["1" 2 "3"] vector?
          [:vector string?] #{"1" 2 "3"} #{"1" 2 "3"} set?

          [:sequential keyword?] nil nil nil?
          [:sequential keyword?] ["1" 2 "3"] [:1 2 :3] sequential?
          [:sequential keyword?] '("1" 2 "3") [:1 2 :3] sequential?
          [:sequential keyword?] (lazy-seq '("1" 2 "3")) [:1 2 :3] sequential?

          [:sequential string?] ["1" 2 "3"] ["1" 2 "3"] sequential?
          [:sequential string?] #{"1" 2 "3"} #{"1" 2 "3"} set?

          [:list keyword?] nil nil nil?
          [:list keyword?] ["1" 2 "3"] [:1 2 :3] sequential?
          [:list keyword?] '("1" 2 "3") [:1 2 :3] sequential?
          [:list keyword?] (lazy-seq '("1" 2 "3")) [:1 2 :3] sequential?

          [:list string?] ["1" 2 "3"] ["1" 2 "3"] sequential?
          [:list string?] #{"1" 2 "3"} #{"1" 2 "3"} set?)))

    (testing "json transformer"
      (testing "json vectors"
        (testing "by default"
          (testing "sequences are not decoded as vectors"
            (is (list? (m/decode [:vector string?] '("1") (mt/json-transformer)))))
          (testing "sets are not"
            (is (set? (m/decode [:vector string?] #{"1"} (mt/json-transformer))))))
        (testing "using option"
          (testing "sequences are decoded as vectors"
            (is (vector? (m/decode [:vector string?] '("1") (mt/json-transformer {::mt/json-vectors true})))))
          (testing "sets are not"
            (is (set? (m/decode [:vector string?] #{"1"} (mt/json-transformer {::mt/json-vectors true})))))))))

  (testing "map"
    (testing "decode"
      (is (= {:c1 1, ::c2 :kikka} (m/decode [:map [:c1 int?] [::c2 keyword?]] {:c1 "1", ::c2 "kikka"} mt/string-transformer)))
      (is (= {:c1 "1", ::c2 :kikka} (m/decode [:map [::c2 keyword?]] {:c1 "1", ::c2 "kikka"} mt/json-transformer)))
      (is (= nil (m/decode [:map] nil mt/string-transformer)))
      (is (= ::invalid (m/decode [:map] ::invalid mt/json-transformer))))
    (testing "encode"
      (is (= {:c1 "1", ::c2 "kikka"} (m/encode [:map [:c1 int?] [::c2 keyword?]] {:c1 1, ::c2 :kikka} mt/string-transformer)))
      (is (= {:c1 1, ::c2 "kikka"} (m/encode [:map [::c2 keyword?]] {:c1 1, ::c2 :kikka} mt/json-transformer)))
      (is (= nil (m/encode [:map] nil mt/string-transformer)))
      (is (= ::invalid (m/encode [:map] ::invalid mt/json-transformer)))))

  (testing "map-of"
    (is (= {1 :abba, 2 :jabba} (m/decode [:map-of int? keyword?] {"1" "abba", "2" "jabba"} mt/string-transformer)))
    (is (= {"1" :abba, "2" :jabba} (m/decode [:map-of int? keyword?] {"1" "abba", "2" "jabba"} mt/json-transformer)))
    (is (= nil (m/decode [:map-of int? keyword?] nil mt/string-transformer)))
    (is (= ::invalid (m/decode [:map-of int? keyword?] ::invalid mt/json-transformer))))

  (testing "maybe"
    (testing "decode"
      (is (= 1 (m/decode [:maybe int?] "1" mt/string-transformer)))
      (is (= nil (m/decode [:maybe int?] nil mt/string-transformer))))
    (testing "encode"
      (is (= "1" (m/encode [:maybe int?] 1 mt/string-transformer)))
      (is (= nil (m/encode [:maybe int?] nil mt/string-transformer)))))

  (testing "tuple"
    (testing "decode"
      (is (= [1] (m/decode [:tuple int?] ["1"] mt/string-transformer)))
      (is (= [1 :kikka] (m/decode [:tuple int? keyword?] ["1" "kikka"] mt/string-transformer)))
      (is (= [:kikka 1] (m/decode [:tuple keyword? int?] ["kikka" "1"] mt/string-transformer)))
      (is (= "1" (m/decode [:tuple keyword? int?] "1" mt/string-transformer)))
      (is (= nil (m/decode [:tuple keyword? int?] nil mt/string-transformer)))
      (is (= [:kikka 1 "2"] (m/decode [:tuple keyword? int?] ["kikka" "1" "2"] mt/string-transformer))))
    (testing "encode"
      (is (= ["1"] (m/encode [:tuple int?] [1] mt/string-transformer)))
      (is (= ["1" "kikka"] (m/encode [:tuple int? keyword?] [1 :kikka] mt/string-transformer)))
      (is (= ["kikka" "1"] (m/encode [:tuple keyword? int?] [:kikka 1] mt/string-transformer)))
      (is (= 1.0 (m/encode [:tuple keyword? int?] 1.0 mt/string-transformer)))
      (is (= nil (m/encode [:tuple keyword? int?] nil mt/string-transformer)))
      (is (= ["kikka" "1" "2"] (m/encode [:tuple keyword? int?] [:kikka 1 "2"] mt/string-transformer))))))

(deftest collection-transform-test
  (testing "decode"
    (is (= #{1 2 3} (m/decode [:set int?] [1 2 3] mt/collection-transformer)))
    ;; sets to sequences / vectors, lose order, is this a good transformation?
    (is (= [1 3 2] (m/decode [:vector int?] #{1 2 3} mt/collection-transformer)))
    (is (= #{1 2 3} (m/decode [:set {:decode/string #(map str %)} int?]
                              "123" mt/string-transformer))))
  (testing "encode"
    (is (= #{1 2 3} (m/encode [:set int?] [1 2 3] mt/collection-transformer))))

  (testing "does not interprit strings as collections"
    (is (= "123" (m/encode [:set int?] "123" mt/collection-transformer)))
    (is (= "abc" (m/encode [:vector keyword?] "abc" mt/json-transformer))))

  (testing "does not raise with bad input"
    (is (= 2 (m/encode [:set string?] 2 mt/collection-transformer))))

  (testing "allows transformers to change their type"
    (is (= "a,b,c" (m/encode [:vector {:encode/string {:leave #(str/join "," %)}}
                              string?] ["a" "b" "c"] mt/string-transformer)))))

(deftest composing-transformers
  (is (= nil (mt/transformer nil)))

  (let [strict-json-transformer (mt/transformer
                                  nil
                                  mt/strip-extra-keys-transformer
                                  mt/json-transformer
                                  {:opts {:random :opts}})]

    (testing "transformer chain has 3 transformers"
      (is (= 3 (count (m/-transformer-chain strict-json-transformer)))))

    (testing "decode"
      (is (= :kikka (m/decode keyword? "kikka" strict-json-transformer)))
      (is (= {:x :kikka} (m/decode [:map [:x keyword?]] {:x "kikka", :y "kukka"} strict-json-transformer))))
    (testing "encode"
      (is (= "kikka" (m/encode keyword? :kikka strict-json-transformer)))
      (is (= {:x "kikka"} (m/encode [:map [:x keyword?]] {:x :kikka, :y :kukka} strict-json-transformer)))))

  (let [transformer (mt/transformer
                      (mt/key-transformer
                        {:decode #(-> % (subs 4) keyword)
                         :encode #(->> % name (str "key_"))})
                      (mt/string-transformer)
                      (mt/strip-extra-keys-transformer))]
    (testing "decode"
      (is (= {:x 18 :y "john"}
             (m/decode
               [:map [:x int?] [:y string?] [[:opt :z] boolean?]]
               {"key_x" "18" "key_y" "john" "key_a" "doe"}
               transformer))))
    (testing "encode"
      (is (= {"key_x" "18" "key_y" "john"}
             (m/encode
               [:map [:x int?] [:y string?] [[:opt :z] boolean?]]
               {:x 18 :y "john" :a "doe"}
               transformer))))))

(deftest strip-extra-keys-transformer-test
  (let [strip-default (mt/strip-extra-keys-transformer)
        strip-closed (mt/strip-extra-keys-transformer {:accept (comp :closed m/properties)})
        strip-all (mt/strip-extra-keys-transformer {:accept (constantly true)})]

    (are [expected transformer schema]
      (is (= expected (m/decode schema {:x 1, :y 2} transformer)))

      {:x 1} strip-default [:map [:x int?]]
      {:x 1, :y 2} strip-default [:map {:closed false} [:x int?]]
      {:x 1} strip-default [:map {:closed true} [:x int?]]

      {:x 1, :y 2} strip-closed [:map [:x int?]]
      {:x 1, :y 2} strip-closed [:map {:closed false} [:x int?]]
      {:x 1} strip-closed [:map {:closed true} [:x int?]]

      {:x 1} strip-all [:map [:x int?]]
      {:x 1} strip-all [:map {:closed false} [:x int?]]
      {:x 1} strip-all [:map {:closed true} [:x int?]])))

(deftest key-transformer
  (let [key-transformer (mt/key-transformer
                          {:decode #(-> % name (str "_key") keyword)
                           :encode #(-> % name (str "_key"))})]
    (testing "decode"
      (is (= {:x_key 18 :y_key "john" :a_key "doe"}
             (m/decode [:map [:x int?] [:y string?] [[:opt :z] boolean?]]
                       {:x 18 :y "john" :a "doe"}
                       key-transformer))))
    (testing "encode"
      (is (= {"x_key" 18 "y_key" "john" "a_key" "doe"}
             (m/encode [:map [:x int?] [:y string?] [[:opt :z] boolean?]]
                       {:x 18 :y "john" :a "doe"}
                       key-transformer)))))

  (testing "from strings and back"
    (let [schema [:map
                  [:id :string]
                  [:github-followers pos-int?]]
          transformer (mt/transformer
                        (mt/key-transformer {:decode keyword, :encode name})
                        (mt/string-transformer))
          value {"id" "123", "github-followers" "10"}]
      (is (= {:id "123", :github-followers 10}
             (as-> value $ (m/decode schema $ transformer))))
      (is (= value
             (as-> value $ (m/decode schema $ transformer) (m/encode schema $ transformer)))))))

(deftest interceptor-style-transformers
  (testing "map"
    (let [raw-val {:x 5 :y :foo}
          map-interceptor {:enter (fn [m]
                                    (is (= raw-val m))
                                    (update m :x inc))
                           :leave (fn [m]
                                    (is (= "foo" (:y m)))
                                    (update m :y #(str % "!")))}
          transformer (mt/transformer
                        {:name :custom
                         :encoders {:map map-interceptor
                                    'keyword? name}})]
      (is (= {:x 6 :y "foo!"}
             (m/encode [:map [:x int?] [:y keyword?]]
                       raw-val
                       transformer)))))
  (testing "call order"
    (are [schema data call-order]
      (let [calls (atom [])
            record-call (fn [n] {:enter (fn [x]
                                          (swap! calls conj [:enter n])
                                          x)
                                 :leave (fn [x]
                                          (swap! calls conj [:leave n])
                                          x)})
            transformer (mt/transformer
                          {:name :order-test
                           :decoders {:map (record-call :map)
                                      :map-of (record-call :map-of)
                                      :vector (record-call :vector)
                                      :multi (record-call :multi)
                                      :tuple (record-call :tuple)
                                      'int? (record-call :int)
                                      'string? (record-call :string)}})]
        (m/decode schema data transformer)
        (= call-order
           @calls))
      [:map
       [:foo int?]
       [:bar string?]]
      {:foo 5 :bar "wee"}
      [[:enter :map]
       [:enter :int]
       [:enter :string]
       [:leave :int]
       [:leave :string]
       [:leave :map]]
      [:map-of int? string?]
      {5 "foo"}
      [[:enter :map-of]
       [:enter :int]
       [:enter :string]
       [:leave :int]
       [:leave :string]
       [:leave :map-of]]
      [:vector int?]
      [5 6]
      [[:enter :vector]
       [:enter :int]
       [:enter :int]
       [:leave :int]
       [:leave :int]
       [:leave :vector]]
      [:tuple string? int?]
      ["Foo" 5]
      [[:enter :tuple]
       [:enter :string]
       [:enter :int]
       [:leave :string]
       [:leave :int]
       [:leave :tuple]]
      [:multi {:dispatch :kind}
       [:person [:map [:name string?]]]
       [:food [:map [:weight int?]]]]
      {:kind :food
       :weight 42}
      [[:enter :multi]
       [:enter :map]
       [:enter :int]
       [:leave :int]
       [:leave :map]
       [:leave :multi]])))

(deftest default-transformers
  (let [state (atom nil)
        transform (fn [schema method phase]
                    (fn [value]
                      (swap! state (fnil conj []) [method phase (m/type schema) value])
                      value))
        schema (m/schema [:map [:x int?] [:y string?]])
        transformer (mt/transformer {:decoders {'int? identity}
                                     :default-decoder {:compile (fn [schema _]
                                                                  {:enter (transform schema :decode :enter)
                                                                   :leave (transform schema :decode :leave)})}
                                     :encoders {'int? identity}
                                     :default-encoder {:compile (fn [schema _]
                                                                  {:enter (transform schema :encode :enter)
                                                                   :leave (transform schema :encode :leave)})}})]
    (testing "decode"
      (reset! state nil)
      (m/decode schema {:x 1, :y "2"} transformer)
      (is (= [[:decode :enter :map {:x 1, :y "2"}]
              [:decode :enter :malli.core/entry 1]
              [:decode :enter :malli.core/entry "2"]
              [:decode :enter 'string? "2"]
              [:decode :leave :malli.core/entry 1]
              [:decode :leave 'string? "2"]
              [:decode :leave :malli.core/entry "2"]
              [:decode :leave :map {:x 1, :y "2"}]]
             @state)))

    (testing "encode"
      (reset! state nil)
      (m/encode schema {:x 1, :y "2"} transformer)
      (is (= [[:encode :enter :map {:x 1, :y "2"}]
              [:encode :enter :malli.core/entry 1]
              [:encode :enter :malli.core/entry "2"]
              [:encode :enter 'string? "2"]
              [:encode :leave :malli.core/entry 1]
              [:encode :leave 'string? "2"]
              [:encode :leave :malli.core/entry "2"]
              [:encode :leave :map {:x 1, :y "2"}]]
             @state)))))

(deftest schema-hinted-transformation
  (let [schema [string? {:title "lower-upper-string"
                         :decode/string 'str/upper-case
                         :encode/string 'str/lower-case}]
        value "KiKkA"]
    (testing "defined transformations"
      (is (= "KIKKA" (m/decode schema value mt/string-transformer)))
      (is (= "kikka" (m/encode schema value mt/string-transformer)))
      (is (= "kikka" (as-> value $
                           (m/decode schema $ mt/string-transformer)
                           (m/encode schema $ mt/string-transformer)))))
    (testing "undefined transformations"
      (is (= value (m/decode schema value mt/json-transformer)))
      (is (= value (m/encode schema value mt/json-transformer)))))

  (let [transformer (mt/transformer
                      {:name :before}
                      mt/string-transformer
                      {:decoders {'int? inc}}
                      {:name :after})]
    (testing "nil punning"
      (is (= identity (m/decoder string? transformer))))
    (is (= 23 (m/decode
                [int? {:decode/before '{:leave inc}
                       :decode/after '(partial * 2)}]
                "10"
                transformer))))

  (testing "and"
    (testing "decode"
      (are [x expected]
        (is (= expected
               (m/decode [:and {:decode/string {:enter #(if (re-matches #"\d{2}" %) (str % "0") %)
                                                :leave #(if (>= % 100) (* 10 %) %)}}
                          int?
                          [any? {:decode/string {:enter inc
                                                 :leave (partial * 2)}}]]
                         x mt/string-transformer)))
        "1" 4
        "11" 2220))
    (testing "encode"
      (are [x expected]
        (is (= expected
               (m/encode [:and {:encode/string {:enter #(if (> % 10) (* % 10) %)
                                                :leave #(if (re-matches #"<<\d{3}>>" %) (str "<<" % ">>") %)}}
                          keyword?
                          [pos-int? {:encode/string {:enter #(str "<<" %), :leave #(str % ">>")}}]
                          int?] x mt/string-transformer)))
        1 "<<1>>"
        11 "<<<<110>>>>")))

  (testing "or"
    (testing "decode"
      (are [x expected]
        (is (= expected
               (m/decode [:or {:decode/string {:enter #(if (re-matches #"\d{2}" %) (str % "0") %)
                                               :leave #(cond (and (int? %) (>= % 100)) (* 10 %)
                                                             (keyword? %) (keyword "decoded" (name %))
                                                             :else %)}}
                          [pos-int? {:decode/string {:enter mt/-string->long
                                                     :leave #(if (int? %) (inc %) %)}}]
                          keyword?]
                         x mt/string-transformer)))
        "1" 2
        "11" 1110
        "-1" :decoded/-1))
    (testing "encode"
      (are [x expected]
        (is (= expected
               (m/encode [:or {:encode/string {:enter #(if (> % 10) (* % 10) %)
                                               :leave #(if (re-matches #"<<\d{3}>>" %) (str "<<" % ">>") %)}}
                          keyword?
                          [pos-int? {:encode/string {:enter #(str "<<" %), :leave #(str % ">>")}}]
                          int?] x mt/string-transformer)))
        1 "<<1>>"
        -1 "-1"
        11 "<<<<110>>>>"))))

(deftest transformation-targets
  (let [P1 {:decode/string 'str/upper-case}
        PS {:decode/string '(partial mapv str/upper-case)}
        PM {:decode/string '(fn [x] (->> (for [[k v] x] [(keyword k) (str/upper-case v)]) (into {})))}
        expectations [[[keyword? P1] "kikka" "KIKKA"]
                      [[:and P1 keyword?] "kikka" :KIKKA]
                      [[:or P1 int? keyword?] "kikka" :KIKKA]
                      [[:map PM [:x keyword?]] {"x" "kikka", "y" "kukka"} {:x :KIKKA, :y "KUKKA"}]
                      [[:map-of PM string? keyword?] {"x" "kikka", "y" "kukka"} {:x :KIKKA, :y :KUKKA}]
                      [[:tuple PS keyword? int?] ["kikka" "1"] [:KIKKA 1]]
                      [[:enum P1 "S" "M" "L"] "s" "S"]
                      [[:re P1 ".*"] "kikka" "KIKKA"]
                      [[:fn P1 'string?] "kikka" "KIKKA"]
                      [[:maybe P1 keyword?] "kikka" :KIKKA]
                      [[:vector PS keyword?] ["kikka"] [:KIKKA]]
                      [[:sequential PS keyword?] ["kikka"] [:KIKKA]]
                      [[:sequential PS keyword?] '("kikka") '(:KIKKA)]
                      [[:list PS keyword?] '("kikka") '(:KIKKA)]
                      [[:set PS keyword?] #{"kikka"} #{:KIKKA}]]]
    (doseq [[schema value expected] expectations]
      (is (= expected (m/decode schema value mt/string-transformer))))))

(deftest options-in-transformaton
  (let [schema [:and int? [any? {:decode/string '{:compile (fn [_ {::keys [increment]}] (partial + (or increment 0)))}}]]
        transformer (mt/transformer mt/string-transformer)]
    (is (= 0 (m/decode schema "0" transformer)))
    (is (= 1 (m/decode schema "0" {::increment 1} transformer)))
    (is (= 1000 (m/decode schema "0" {::increment 1000} transformer)))))

(deftest default-transformer
  (testing "nil collections"
    (are [schema expected]
      (is (= expected (m/decode schema nil mt/default-value-transformer)))

      [:vector {:default [1 2 3]} int?] [1 2 3]
      [:map {:default {:x 10}} [:x int?]] {:x 10}
      [:tuple {:default [1 2]} int? int?] [1 2]
      [:map-of {:default {1 1}} int? int?] {1 1}))

  (testing "nested"
    (let [schema [:map {:default {}}
                  [:a [int? {:default 1}]]
                  [:b [:vector {:default [1 2 3]} int?]]
                  [:c [:map {:default {}}
                       [:x [int? {:default 42}]]
                       [:y int?]]]
                  [:d [:map
                       [:x [int? {:default 42}]]
                       [:y int?]]]
                  [:e int?]
                  [:f [boolean? {:default true}]]
                  [:g [boolean? {:default false}]]]]

      (is (= {:a 1
              :b [1 2 3]
              :c {:x 42}
              :f true
              :g false}
             (m/encode schema nil mt/default-value-transformer)))

      (is (= {:a "1"
              :b ["1" "2" "3"]
              :c {:x "42"}
              :f true
              :g false}
             (m/encode schema nil (mt/transformer
                                    mt/default-value-transformer
                                    mt/string-transformer))))))

  (testing "default false"
    (is (= {:user/verified false} (m/decode [:map [:user/verified [:and {:default false} boolean?]]] {} mt/default-value-transformer)))
    (is (= false (m/decode [:and {:default false} boolean?] nil mt/default-value-transformer)))))
