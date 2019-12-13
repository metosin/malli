(ns malli.transform-test
  (:require [clojure.test :refer [deftest testing is are]]
            [malli.core :as m]
            [malli.transform :as mt]
            [clojure.string :as str]))

(deftest ->interceptor-test
  (are [?interceptor expected]
    (= expected (is (#'mt/->interceptor ?interceptor {} {})))

    inc {:enter inc}
    {:enter inc} {:enter inc}
    {:leave dec} {:leave dec}
    {:enter inc, :leave dec} {:enter inc, :leave dec}
    {:compile (constantly inc)} {:enter inc}
    {:compile (constantly {:enter inc, :leave dec})} {:enter inc, :leave dec})

  (let [?interceptor {:compile (constantly {:compile (constantly inc)})}]
    (testing "shallow compilation succeeds"
      (binding [mt/*max-compile-depth* 2]
        (is (= {:enter inc} (#'mt/->interceptor ?interceptor {} {})))))
    (testing "too deep compilation fails"
      (binding [mt/*max-compile-depth* 1]
        (is (thrown? #?(:clj Exception, :cljs js/Error) (#'mt/->interceptor ?interceptor {} {})))))))

(deftest string->long
  (is (= 1 (mt/string->long "1")))
  (is (= 1 (mt/string->long 1)))
  (is (= "abba" (mt/string->long "abba"))))

(deftest string->double
  (is (= 1.0 (mt/string->double "1")))
  (is (= 1.0 (mt/string->double 1.0)))
  (is (= 1 (mt/string->double 1)))
  (is (= "abba" (mt/string->double "abba"))))

(deftest string->keyword
  (is (= :abba (mt/string->keyword "abba")))
  (is (= :abba (mt/string->keyword :abba))))

(deftest string->boolean
  (is (= true (mt/string->boolean "true")))
  (is (= false (mt/string->boolean "false")))
  (is (= "abba" (mt/string->boolean "abba"))))

(deftest string->uuid
  (is (= #uuid"5f60751d-9bf7-4344-97ee-48643c9949ce" (mt/string->uuid "5f60751d-9bf7-4344-97ee-48643c9949ce")))
  (is (= #uuid"5f60751d-9bf7-4344-97ee-48643c9949ce" (mt/string->uuid #uuid"5f60751d-9bf7-4344-97ee-48643c9949ce")))
  (is (= "abba" (mt/string->uuid "abba"))))

(deftest string->date
  (is (= #inst "2018-04-27T18:25:37Z" (mt/string->date "2018-04-27T18:25:37Z")))
  (is (= #inst "2018-04-27T00:00:00Z" (mt/string->date "2018-04-27")))
  (is (= #inst "2018-04-27T05:00:00Z" (mt/string->date "2018-04-27T08:00:00+03:00")))
  (is (= #inst "2018-04-27T18:25:37Z" (mt/string->date "2018-04-27T18:25:37.000Z")))
  (is (= #inst "2018-04-27T18:25:37Z" (mt/string->date "2018-04-27T18:25:37.000+0000")))
  (is (= #inst "2014-02-18T18:25:37Z" (mt/string->date #inst "2014-02-18T18:25:37Z")))
  (is (= #inst "2018-04-27T00:00:00Z" (mt/string->date #inst "2018-04-27")))
  (is (= #inst "2018-04-27T05:00:00Z" (mt/string->date #inst "2018-04-27T08:00:00+03:00")))
  (is (= "abba" (mt/string->date "abba"))))

(deftest date->string
  (is (= "2014-02-18T18:25:37.000Z" (mt/date->string #inst "2014-02-18T18:25:37Z")))
  (is (= "abba" (mt/date->string "abba"))))

(deftest string->symbol
  (is (= 'inc (mt/string->symbol "inc")))
  (is (= 'inc (mt/string->symbol 'inc))))

(deftest string->nil
  (is (= nil (mt/string->nil "")))
  (is (= nil (mt/string->nil nil))))

(deftest number->double
  #?(:clj (is (= 0.5 (mt/number->double 1/2))))
  (is (= 1.0 (mt/number->double 1)))
  (is (= "kikka" (mt/number->double "kikka"))))

(deftest any->string
  #?(:clj (is (= "1/2" (mt/any->string 1/2))))
  (is (= "0.5" (mt/any->string 0.5)))
  (is (= nil (mt/any->string nil))))

(deftest any->any
  #?(:clj (is (= 1/2 (mt/any->any 1/2))))
  (is (= 0.5 (mt/any->any 0.5)))
  (is (= nil (mt/any->any nil))))

(deftest transform-test
  (testing "predicates"
    (testing "decode"
      (is (= 1 (m/decode int? "1" mt/string-transformer)))
      (is (= "1" (m/decode int? "1" mt/json-transformer)))
      (is (= :user/kikka (m/decode keyword? "user/kikka" mt/string-transformer))))
    (testing "encode"
      (is (= "1" (m/encode int? 1 mt/string-transformer)))
      (is (= "1" (m/encode int? 1 mt/json-transformer)))
      (is (= "user/kikka" (m/encode keyword? :user/kikka mt/string-transformer)))))
  (testing "comparators"
    (testing "decode"
      (doseq [schema (keys m/comparator-registry)]
        (is (= 1 (m/decode [schema 1] "1" mt/string-transformer)))))
    (testing "encode"
      (doseq [schema (keys m/comparator-registry)]
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
    (is (= #{1 2 3} (m/decode [:set int?] ["1" 2 "3"] mt/string-transformer)))
    (is (= #{"1" 2 "3"} (m/decode [:set [:enum 1 2]] ["1" 2 "3"] mt/string-transformer)))
    (is (= #{"1" 2 "3"} (m/decode [:set int?] ["1" 2 "3"] mt/json-transformer)))
    (is (= [:1 2 :3] (m/decode [:vector keyword?] ["1" 2 "3"] mt/string-transformer)))
    (is (= [:1 2 :3] (m/decode [:sequential keyword?] ["1" 2 "3"] mt/string-transformer)))
    (is (= '(:1 2 :3) (m/decode [:sequential keyword?] '("1" 2 "3") mt/string-transformer)))
    (is (= '(:1 2 :3) (m/decode [:list keyword?] '("1" 2 "3") mt/string-transformer)))
    (is (= '(:1 2 :3) (m/decode [:list keyword?] (seq '("1" 2 "3")) mt/string-transformer)))
    (is (= '(:1 2 :3) (m/decode [:list keyword?] (lazy-seq '("1" 2 "3")) mt/string-transformer)))
    (is (= ::invalid (m/decode [:vector keyword?] ::invalid mt/string-transformer))))
  (testing "map"
    (testing "decode"
      (is (= {:c1 1, ::c2 :kikka} (m/decode [:map [:c1 int?] [::c2 keyword?]] {:c1 "1", ::c2 "kikka"} mt/string-transformer)))
      (is (= {:c1 "1", ::c2 :kikka} (m/decode [:map [::c2 keyword?]] {:c1 "1", ::c2 "kikka"} mt/json-transformer)))
      (is (= ::invalid (m/decode [:map] ::invalid mt/json-transformer))))
    (testing "encode"
      (is (= {:c1 "1", ::c2 "kikka"} (m/encode [:map [:c1 int?] [::c2 keyword?]] {:c1 1, ::c2 :kikka} mt/string-transformer)))
      (is (= {:c1 1, ::c2 "kikka"} (m/encode [:map [::c2 keyword?]] {:c1 1, ::c2 :kikka} mt/json-transformer)))
      (is (= ::invalid (m/encode [:map] ::invalid mt/json-transformer)))))
  (testing "map-of"
    (is (= {1 :abba, 2 :jabba} (m/decode [:map-of int? keyword?] {"1" "abba", "2" "jabba"} mt/string-transformer)))
    (is (= {"1" :abba, "2" :jabba} (m/decode [:map-of int? keyword?] {"1" "abba", "2" "jabba"} mt/json-transformer)))
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
      (is (= [:kikka 1 "2"] (m/decode [:tuple keyword? int?] ["kikka" "1" "2"] mt/string-transformer))))
    (testing "encode"
      (is (= ["1"] (m/encode [:tuple int?] [1] mt/string-transformer)))
      (is (= ["1" "kikka"] (m/encode [:tuple int? keyword?] [1 :kikka] mt/string-transformer)))
      (is (= ["kikka" "1"] (m/encode [:tuple keyword? int?] [:kikka 1] mt/string-transformer)))
      (is (= 1.0 (m/encode [:tuple keyword? int?] 1.0 mt/string-transformer)))
      (is (= ["kikka" "1" "2"] (m/encode [:tuple keyword? int?] [:kikka 1 "2"] mt/string-transformer))))))

;; TODO: this is wrong!
(deftest collection-transform-test
  (testing "decode"
    (is (= #{1 2 3} (m/decode [:set int?] [1 2 3] mt/collection-transformer))))
  (testing "encode"
    (is (= #{1 2 3} (m/encode [:set int?] [1 2 3] mt/collection-transformer))))

  (testing "does not interprit strings as collections"
    (is (= "123" (m/encode [:set string?] "123" mt/collection-transformer)))
    (is (= "abc" (m/encode [:vector keyword?] "abc" mt/json-transformer))))


  (testing "does not raise with bad input"
    (is (= 2 (m/encode [:set string?] 2 mt/collection-transformer))))

  (testing "allows transformers to change their type"
    (is (= "a,b,c" (m/encode [:vector {:encode/string {:leave #(str/join "," %)}}
                              string?] ["a" "b" "c"] mt/string-transformer)))))

(deftest composing-transformers
  (let [strict-json-transformer (mt/transformer
                                  mt/strip-extra-keys-transformer
                                  mt/json-transformer
                                  {:opts {:random :opts}})]

    (testing "transformer chain has 3 named transformers"
      (is (= [::mt/strip-extra-keys :json] (keep :name (m/-transformer-chain strict-json-transformer)))))

    (testing "decode"
      (is (= :kikka (m/decode keyword? "kikka" strict-json-transformer)))
      (is (= {:x :kikka} (m/decode [:map [:x keyword?]] {:x "kikka", :y "kukka"} strict-json-transformer))))
    (testing "encode"
      (is (= "kikka" (m/encode keyword? :kikka strict-json-transformer)))
      (is (= {:x "kikka"} (m/encode [:map [:x keyword?]] {:x :kikka, :y :kukka} strict-json-transformer)))))

  (let [strip-extra-key-transformer (mt/transformer
                                      mt/string-transformer
                                      mt/strip-extra-keys-transformer
                                      (mt/key-transformer
                                        #(-> % name (str "_key") keyword)
                                        #(-> % name (str "_key"))))]
    (testing "decode"
      (is (= {:x_key 18 :y_key "john"}
             (m/decode
               [:map [:x int?] [:y string?] [[:opt :z] boolean?]]
               {:x "18" :y "john" :a "doe"}
               strip-extra-key-transformer))))
    (testing "encode"
      (is (= {"x_key" "18" "y_key" "john"}
             (m/encode
               [:map [:x int?] [:y string?] [[:opt :z] boolean?]]
               {:x 18 :y "john" :a "doe"}
               strip-extra-key-transformer))))))

(deftest key-transformer
  (let [key-transformer (mt/key-transformer
                          #(-> % name (str "_key") keyword)
                          #(-> % name (str "_key")))]
    (testing "decode"
      (is (= {:x_key 18 :y_key "john" :a_key "doe"}
             (m/decode [:map [:x int?] [:y string?] [[:opt :z] boolean?]]
                       {:x 18 :y "john" :a "doe"}
                       key-transformer))))
    (testing "encode"
      (is (= {"x_key" 18 "y_key" "john" "a_key" "doe"}
             (m/encode [:map [:x int?] [:y string?] [[:opt :z] boolean?]]
                       {:x 18 :y "john" :a "doe"}
                       key-transformer))))))

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
        (let [calls       (atom [])
              record-call (fn [n] (constantly
                                   {:enter (fn [x]
                                             (swap! calls conj [:enter n])
                                             x)
                                    :leave (fn [x]
                                             (swap! calls conj [:leave n])
                                             x)}))
              transformer (mt/transformer
                           {:name     :order-test
                            :decoders {:map     (record-call :map)
                                       :map-of  (record-call :map-of)
                                       :vector  (record-call :vector)
                                       :multi   (record-call :multi)
                                       :tuple   (record-call :tuple)
                                       'int?    (record-call :int)
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
       [:food   [:map [:weight int?]]]]
      {:kind   :food
       :weight 42}
      [[:enter :multi]
       [:enter :map]
       [:enter :int]
       [:leave :int]
       [:leave :map]
       [:leave :multi]])))

(deftest schema-hinted-tranformation
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
                transformer)))))

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
        transformer (mt/transformer mt/string-transformer)
        transformer1 (mt/transformer mt/string-transformer {:opts {::increment 1}})
        transformer1000 (mt/transformer mt/string-transformer {:opts {::increment 1000}})]
    (is (= 0 (m/decode schema "0" transformer)))
    (is (= 1 (m/decode schema "0" transformer1)))
    (is (= 1000 (m/decode schema "0" transformer1000)))))
