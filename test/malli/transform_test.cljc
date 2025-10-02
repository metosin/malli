(ns malli.transform-test
  (:require [clojure.string :as str]
            [clojure.test :refer [are deftest is testing]]
            [malli.core :as m]
            [malli.core-test]
            [malli.registry :as mr]
            [malli.transform :as mt])
  #?(:clj (:import (java.net URI))))

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
  (is (= 9007199254740991 (mt/-string->long "9007199254740991")))
  (is (= -9007199254740991 (mt/-string->long "-9007199254740991")))
  ;; Unfortunately, the number in the CLJ branch here isn't representable in JS 'integers'.
  (is (= #?(:clj 9007199254740993 :cljs "9007199254740993")
         (mt/-string->long "9007199254740993")))
  (is (= #?(:clj -9007199254740993 :cljs "-9007199254740993")
         (mt/-string->long "-9007199254740993")))
  (is (= "abba" (mt/-string->long "abba"))))

(deftest string->double
  (is (= 1.0 (mt/-string->double "1")))
  (is (= 1.0 (mt/-string->double 1.0)))
  (is (= 1 (mt/-string->double 1)))
  (is (= "1.0abba" (mt/-string->double "1.0abba")))
  (is (= "abba" (mt/-string->double "abba"))))

(deftest string->keyword
  (is (= :abba (mt/-string->keyword "abba")))
  (is (= :abba (mt/-string->keyword :abba))))

(deftest string->boolean
  (is (= true (mt/-string->boolean "true")))
  (is (= false (mt/-string->boolean "false")))
  (is (= "abba" (mt/-string->boolean "abba"))))

(deftest string->uuid
  (is (= #uuid "5f60751d-9bf7-4344-97ee-48643c9949ce" (mt/-string->uuid "5f60751d-9bf7-4344-97ee-48643c9949ce")))
  (is (= #uuid "5f60751d-9bf7-4344-97ee-48643c9949ce" (mt/-string->uuid #uuid "5f60751d-9bf7-4344-97ee-48643c9949ce")))
  (is (= #uuid "b3c4e6b4-6304-4a52-99c3-cb50e737bb94" (mt/-string->uuid "B3C4E6B4-6304-4A52-99C3-CB50E737BB94")))

  (is (= "abba" (mt/-string->uuid "abba")))

  ;; Regression tests: we should ensure that invalid or incomplete
  ;; uuids are handled unformly in CLJ and CLJS
  (is (= "5f60751d-9bf7-4344-97ee-48643c" (mt/-string->uuid "5f60751d-9bf7-4344-97ee-48643c")))
  (is (= "1-1-1-1-1" (mt/-string->uuid "1-1-1-1-1")))

  ;; Ensure that uuid0 is also a valid uuid
  (is (= #uuid "00000000-0000-0000-0000-000000000000" (mt/-string->uuid "00000000-0000-0000-0000-000000000000"))))

#?(:clj
   (deftest string->uri
     (is (= (URI. "http://example.com") (mt/-string->uri "http://example.com")))
     (is (= "broken link" (mt/-string->uri "broken link")))))

(deftest string->date
  (is (= #inst "2018-04-27T18:25:37Z" (mt/-string->date "2018-04-27T18:25:37Z")))
  (is (= #inst "2018-04-27T18:25:37.100Z" (mt/-string->date "2018-04-27T18:25:37.1Z")))
  (is (= #inst "2018-04-27T18:25:37.123Z" (mt/-string->date "2018-04-27T18:25:37.123Z")))
  (is (= #inst "2018-04-27T18:25:37.123Z" (mt/-string->date "2018-04-27T18:25:37.123456Z")))
  (is (= #inst "2018-04-27T18:25:37.123456Z" (mt/-string->date "2018-04-27T18:25:37.123456Z")))
  (is (= #inst "2018-04-27T00:00:00Z" (mt/-string->date "2018-04-27")))
  (is (= #inst "2018-04-27T05:00:00Z" (mt/-string->date "2018-04-27T08:00:00+03:00")))
  (is (= #inst "2018-04-27T18:25:37Z" (mt/-string->date "2018-04-27T18:25:37.000Z")))
  (is (= #inst "2018-04-27T18:25:37Z" (mt/-string->date "2018-04-27T18:25:37.000+0000")))
  (is (= #inst "2018-04-27T18:25:37.100Z" (mt/-string->date "2018-04-27T18:25:37.1+0000")))
  (is (= #inst "2018-04-27T18:25:37.123Z" (mt/-string->date "2018-04-27T18:25:37.123+0000")))
  (is (= #inst "2018-04-27T18:25:37.123Z" (mt/-string->date "2018-04-27T18:25:37.123456+0000")))
  (is (= #inst "2018-04-27T18:25:37.123456Z" (mt/-string->date "2018-04-27T18:25:37.123456+0000")))
  (is (= #inst "2018-04-27T18:25:37.123Z" (mt/-string->date "2018-04-27T18:25:37.123+0000")))
  (is (= #inst "2018-04-27T18:25:37.123Z" (mt/-string->date "2018-04-27T20:25:37.123+0200")))
  (is (= #inst "2018-04-27T18:25:37.123Z" (mt/-string->date "2018-04-27T16:25:37.123-0200")))
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

(deftest number->long
  (is (= 1 (mt/-number->long 1.0)))
  (is (= 2 (mt/-number->long 2.0)))
  (is (= 2.5 (mt/-number->long 2.5)))
  (is (= "2.5" (mt/-number->long "2.5")))
  #?(:clj (is (= 2 (mt/-number->long 4/2))))
  #?(:clj (is (= 2 (mt/-number->long (float 2.0)))))
  #?(:clj (is (= 2 (mt/-number->long (double 2.0)))))
  (is (= 2 (mt/-number->long 2))))

(deftest any->string
  #?(:clj (is (= "1/2" (mt/-any->string 1/2))))
  #?(:clj (is (= "http://example.com" (mt/-any->string (URI. "http://example.com")))))
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
      (is (= 1 (m/decode int? 1.0 mt/json-transformer)))
      (is (= 1 (m/decode :int 1.0 mt/json-transformer)))
      (is (= 1.5 (m/decode int? 1.5 mt/json-transformer)))
      (is (= 1.5 (m/decode :int 1.5 mt/json-transformer)))
      (is (= 1 (m/decode pos-int? 1.0 mt/json-transformer)))
      (is (= 0 (m/decode zero? 0.0 mt/json-transformer)))
      (is (= 1.0 (m/decode double? 1 mt/json-transformer)))
      (is (= 1 (m/decode double? 1 mt/string-transformer)))
      (is (= "1.0x" (m/decode double? "1.0x" mt/string-transformer)))
      (is (= 1.0 (m/decode float? 1 mt/json-transformer)))
      (is (= 1 (m/decode float? 1 mt/string-transformer)))
      (is (= "1.0x" (m/decode float? "1.0x" mt/string-transformer)))
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

          [:set [:enum 1 2 "3"]] ["1" 2 "3"] #{"1" 2 "3"} set?

          [:set keyword?] nil nil nil?
          [:set keyword?] ["1" 2 "3"] #{:1 2 :3} set?
          [:set keyword?] '("1" 2 "3") #{:1 2 :3} set?
          [:set keyword?] (lazy-seq '("1" 2 "3")) #{:1 2 :3} set?
          [:set keyword?] ::invalid ::invalid keyword?

          [:set string?] ["1" 2 "3"] #{"1" 2 "3"} set?
          [:set string?] #{"1" 2 "3"} #{"1" 2 "3"} set?

          [:vector keyword?] nil nil nil?
          [:vector keyword?] [] [] vector?
          [:vector keyword?] ["1" 2 "3"] [:1 2 :3] vector?
          [:vector keyword?] '("1" 2 "3") [:1 2 :3] vector?
          [:vector keyword?] (lazy-seq '("1" 2 "3")) [:1 2 :3] vector?

          [:vector string?] ["1" 2 "3"] ["1" 2 "3"] vector?
          [:vector string?] #{"1" 2 "3"} #{"1" 2 "3"} set?

          [:sequential keyword?] nil nil nil?
          [:sequential keyword?] [] [] sequential?
          [:sequential keyword?] ["1" 2 "3"] [:1 2 :3] sequential?
          [:sequential keyword?] '("1" 2 "3") [:1 2 :3] sequential?
          [:sequential keyword?] (lazy-seq '("1" 2 "3")) [:1 2 :3] sequential?

          [:sequential string?] ["1" 2 "3"] ["1" 2 "3"] sequential?
          [:sequential string?] #{"1" 2 "3"} #{"1" 2 "3"} set?)))

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
      (is (= ::invalid (m/encode [:map] ::invalid mt/json-transformer))))
    (testing "keep original type"
      (let [decoded (m/decode [:map [:a :int]] (sorted-map :a "1") mt/string-transformer)]
        (is (= {:a 1} decoded))
        (is (sorted? decoded)))
      (let [decoded (m/decode [:map [:a :keyword]] (sorted-map :a "x") mt/json-transformer)]
        (is (= {:a :x} decoded))
        (is (sorted? decoded)))
      (let [decoded (m/decode [:map [:a :keyword]] (sorted-map "a" "x") (mt/json-transformer {::mt/keywordize-map-keys true}))]
        (is (= {:a :x} decoded))
        (is (sorted? decoded)))))

  (testing "map-of"
    (is (= {1 :abba, 2 :jabba} (m/decode [:map-of int? keyword?] {"1" "abba", "2" "jabba"} mt/string-transformer)))
    (is (= {1 :abba, 2 :jabba} (m/decode [:map-of int? keyword?] {"1" "abba", "2" "jabba"} mt/json-transformer)))
    (testing "keeps original value if decoding fails"
      (is (= {1 :abba, :xyz :jabba} (m/decode [:map-of int? keyword?] {"1" "abba", :xyz "jabba"} mt/json-transformer)))
      (is (= {1 :abba, :xyz :jabba} (m/decode [:map-of int? keyword?] {"1" "abba", :xyz "jabba"} mt/json-transformer))))
    (is (= nil (m/decode [:map-of int? keyword?] nil mt/string-transformer)))
    (is (= ::invalid (m/decode [:map-of int? keyword?] ::invalid mt/json-transformer)))

    (testing "keep original type"
      (let [input-1    (sorted-map "a" "b")
            input-2    (sorted-map :a "b")

            result-1-a (m/decode [:map-of :string :string] input-1 mt/string-transformer)
            result-1-b (m/decode [:map-of :string :string] input-1 mt/json-transformer)

            result-2-a (m/decode [:map-of :keyword :string] input-1 mt/string-transformer)
            result-2-b (m/decode [:map-of :keyword :string] input-1 mt/json-transformer)

            result-3-a (m/decode [:map-of :string :string] input-2 mt/string-transformer)
            result-3-b (m/decode [:map-of :string :string] input-2 mt/json-transformer)]

        (is (sorted? result-1-a))
        (is (sorted? result-1-b))
        (is (sorted? result-2-a))
        (is (sorted? result-2-b))
        (is (sorted? result-3-a))
        (is (sorted? result-3-b))

        (is (= input-1 result-1-a))
        (is (= input-1 result-1-b))
        (is (= input-2 result-2-a))
        (is (= input-2 result-2-b))
        (is (= input-1 result-3-a))
        (is (= input-1 result-3-b)))))

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
      (is (= ["kikka" "1" "2"] (m/encode [:tuple keyword? int?] [:kikka 1 "2"] mt/string-transformer)))))

  (testing "seqex"
    (testing "decode"
      (is (= [1 2] (m/decode [:repeat {:min 2, :max 4} int?] ["1" "2"] mt/string-transformer)))
      (is (= [1 2] (m/decode [:repeat [:repeat int?]] ["1" "2"] mt/string-transformer)))
      (is (= [1 2] (m/decode [:* [:repeat int?]] ["1" "2"] mt/string-transformer)))
      (is (= [1 2] (m/decode [:repeat [:* int?]] ["1" "2"] mt/string-transformer)))
      (are [s v v*]
        (= v* (m/decode s v mt/string-transformer))

        [:cat] [] []
        [:cat] "1" "1"
        [:cat] nil nil
        [:cat int?] ["1"] [1]
        [:cat int? keyword?] ["1" "kikka"] [1 :kikka]
        [:cat int? keyword?] ["kikka" "kukka"] ["kikka" "kukka"]

        [:catn] [] []
        [:catn] "1" "1"
        [:catn] nil nil
        [:catn [:n int?]] ["1"] [1]
        [:catn [:n int?] [:k keyword?]] ["1" "kikka"] [1 :kikka]
        [:catn [:n int?] [:k keyword?]] ["kikka" "kukka"] ["kikka" "kukka"]

        [:alt int?] ["1"] [1]
        [:alt int? keyword?] ["1"] [1]
        [:alt keyword? int?] ["1"] [:1]
        [:alt int? keyword?] ["kikka"] [:kikka]

        [:altn [:n int?]] ["1"] [1]
        [:altn [:n int?] [:k keyword?]] ["1"] [1]
        [:altn [:k keyword?] [:n int?]] ["1"] [:1]
        [:altn [:n int?] [:k keyword?]] ["kikka"] [:kikka]

        [:? int?] [] []
        [:? int?] "1" "1"
        [:? int?] nil nil
        [:? int?] ["1"] [1]
        [:? int?] ["1" "2"] ["1" "2"]

        [:* int?] [] []
        [:* int?] ["1"] [1]
        [:* int?] ["1" "2"] [1 2]
        [:* int?] ["1" "kikka"] ["1" "kikka"]

        [:+ int?] [] []
        [:+ int?] ["1"] [1]
        [:+ int?] ["1" "2"] [1 2]
        [:+ int?] ["1" "kikka"] ["1" "kikka"]

        [:repeat {:min 2, :max 4} int?] [] []
        [:repeat {:min 2, :max 4} int?] nil nil
        [:repeat {:min 2, :max 4} int?] ["1"] ["1"]
        [:repeat {:min 2, :max 4} int?] ["1" "2"] [1 2]
        [:repeat {:min 2, :max 4} int?] ["1" "kikka"] ["1" "kikka"]
        [:repeat {:min 2, :max 4} int?] ["1" "2" "3" "4" "5"] ["1" "2" "3" "4" "5"]))

    (testing "encode"
      (are [s v v*]
        (= (m/encode s v mt/string-transformer) v*)

        [:cat] [] []
        [:cat] 1 1
        [:cat] nil nil
        [:cat int?] [1] ["1"]
        [:cat int? keyword?] [1 :kikka] ["1" "kikka"]
        [:cat int? keyword?] [:kikka :kukka] [:kikka :kukka]

        [:alt int?] [1] ["1"]
        [:alt int? keyword?] [1] ["1"]
        [:alt keyword? int?] [:1] ["1"]
        [:alt int? keyword?] [:kikka] ["kikka"]

        [:? int?] [] []
        [:? int?] 1 1
        [:? int?] nil nil
        [:? int?] [1] ["1"]
        [:? int?] [1 2] [1 2]

        [:* int?] [] []
        [:* int?] [1] ["1"]
        [:* int?] [1 2] ["1" "2"]
        [:* int?] [1 :kikka] [1 :kikka]

        [:+ int?] [] []
        [:+ int?] [1] ["1"]
        [:+ int?] [1 2] ["1" "2"]
        [:+ int?] [1 :kikka] [1 :kikka]

        [:repeat {:min 2, :max 4} int?] [] []
        [:repeat {:min 2, :max 4} int?] nil nil
        [:repeat {:min 2, :max 4} int?] [1] [1]
        [:repeat {:min 2, :max 4} int?] [1 2] ["1" "2"]
        [:repeat {:min 2, :max 4} int?] [1 :kikka] [1 :kikka]
        [:repeat {:min 2, :max 4} int?] [1 2 3 4 5] [1 2 3 4 5]))))

(deftest map-of-enum-transform-test-819
  (let [schema [:map-of
                [:enum {:encode/json name
                        :decode/json keyword}
                 :a :b :c]
                int?]
        m {:a 1 :b 2}
        encoded (m/encode schema m mt/json-transformer)
        decoded (m/decode schema encoded mt/json-transformer)]
    (is (= {"a" 1 "b" 2} encoded))
    (is (= m decoded))))

(deftest collection-transform-test
  (testing "decode"
    (is (= #{1 2 3} (m/decode [:set int?] [1 2 3] mt/collection-transformer)))
    ;; sets to sequences / vectors, lose order, is this a good transformation?
    (is (= [1 3 2] (m/decode [:vector int?] #{1 2 3} mt/collection-transformer)))
    (is (= #{1 2 3} (m/decode [:set {:decode/string #(map str %)} int?]
                              "123" mt/string-transformer)))
    (is (= [1 2 3] (m/decode [:tuple int? int? int?] '(1 2 3) mt/collection-transformer))))
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

    (testing "transformer chain has 4 transformers"
      (is (= 3 (count (m/-transformer-chain strict-json-transformer)))))

    (testing "decode"
      (is (= :kikka (m/decode keyword? "kikka" strict-json-transformer)))
      (is (= {:x :kikka} (m/decode [:map [:x keyword?]] {:x "kikka", :y "kukka"} strict-json-transformer))))
    (testing "encode"
      (is (= "kikka" (m/encode keyword? :kikka strict-json-transformer)))
      (is (= {:x "kikka"} (m/encode [:map [:x keyword?]] {:x :kikka, :y :kukka} strict-json-transformer)))

      (testing "nested map encode"
        (is (= {:x {:a {:b {}}}}
               (m/encode [:map
                          [:x [:map
                               [:a [:map [:b [:map]]]]]]]
                         {:x          {:a {:b {}
                                           :c {}}}
                          :additional 1}
                         strict-json-transformer))))

      (testing "recursive map encode"
        (is (= {:x {"a" {"b" {}}}}
               (m/encode [:map {:registry {::kw-map [:map-of :keyword [:ref ::kw-map]]}}
                          [:x [:ref ::kw-map]]]

                         {:x          {:a {:b {}
                                           ;; TODO: Additional invalid param invalidates the recursive map
                                           ;;"c" {}
                                           }}
                          :additional 1}
                         strict-json-transformer
                         ))))))

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
              transformer)))))

  (testing "extra-keys-transformer shouldn't break non map values"
    (is (= {:foo "bar"}
           (m/decode
            [:map {:decode/string (fn [s] {:foo s})}
             [:foo :string]]
            "bar"
            (mt/transformer
             (mt/strip-extra-keys-transformer)
             (mt/string-transformer)))))))

(deftest strip-extra-keys-transformer-test

  (testing "extra keys from :map are stripped"
    (is (= {:x 1, :y 2}
           (m/decode
            [:map [:x :int] [:y :int]]
            {:x 1, :y 2, :z 3}
            (mt/strip-extra-keys-transformer)))))

  (testing "extra keys from :map-of are stripped"
    (is (= {1 1}
           (m/decode
            [:map-of :int :int]
            {1 1, "2" 2, 3 "3", "4" "4"}
            (mt/strip-extra-keys-transformer))))

    (testing "composing with other transformers"
      (is (= {1 1, 2 2, 3 3, 4 4}
             (m/decode
              [:map-of :int :int]
              {1 1, "2" 2, 3 "3", "4" "4"}
              (mt/transformer
               (mt/strip-extra-keys-transformer)
               (mt/string-transformer)))))))

  (testing "::m/default defines how extra keys are stripped"
    (let [value {:x 1, :y 2, :z 3, 1 1, "2" 2, 3 "3", "4" "4", "string" 5}
          expected {1 1, :x 1, :y 2}]

      (testing "stripping applies for both explicit and default values"
        (is (= expected
               (m/decode
                [:map
                 [:x :int]
                 [:y :int]
                 [::m/default [:map-of :int :int]]]
                value
                (mt/strip-extra-keys-transformer)))))

      (testing "explictly open map works the same way"
        (is (= expected
               (m/decode
                [:map {:closed false}
                 [:x :int]
                 [:y :int]
                 [::m/default [:map-of :int :int]]]
                value
                (mt/strip-extra-keys-transformer)))))

      (testing "default schemas can be arbitrary nested"
        (is (= expected
               (m/decode
                [:map
                 [:x :int]
                 [::m/default [:map
                               [:y :int]
                               [::m/default [:map-of :int :int]]]]]
                value
                (mt/strip-extra-keys-transformer))))

        (testing "composing with other transformers"
          (is (= {:x 1, :y 2, 1 1, 2 2, 3 3, 4 4}
                 (m/decode
                  [:map
                   [:x :int]
                   [::m/default [:map
                                 [:y :int]
                                 [::m/default [:map-of :int :int]]]]]
                  value
                  (mt/transformer
                   (mt/strip-extra-keys-transformer)
                   (mt/string-transformer)))))))))

  (let [strip-default (mt/strip-extra-keys-transformer)
        strip-closed (mt/strip-extra-keys-transformer {:accept (comp :closed m/properties)})
        strip-all (mt/strip-extra-keys-transformer {:accept (constantly true)})]

    (are [expected transformer schema]
      (= expected (m/decode schema {:x 1, :y 2} transformer))

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

  (testing "more types"
    (let [original {"id" "123", "github-followers" 10}
          expected {:id "123", :github-followers 10}]

      (testing "maps"
        (is (= expected (m/decode
                         [:map [:id :string] [:github-followers pos-int?]]
                         original (mt/key-transformer {:decode keyword, :encode name}))))

        (is (= expected (m/decode
                         [:map [:id :string] [:github-followers pos-int?]]
                         original (mt/key-transformer {:decode keyword, :encode name, :types :default}))))

        (is (= original (m/decode
                         [:map [:id :string] [:github-followers pos-int?]]
                         original (mt/key-transformer {:decode keyword, :encode name, :types #{}})))))

      (testing "multi"
        (testing "does not transform by default"
          (is (= original
                 (m/decode
                  [:multi {:dispatch :id}
                   ["123" [:map [:id :string] [:github-followers pos-int?]]]]
                  original (mt/key-transformer {:decode keyword, :encode name})))))

        (testing "can be transformed"
          (is (= expected (m/decode
                           [:and :map [:multi {:dispatch :id}
                                       ["123" [:map [:id :string] [:github-followers pos-int?]]]]]
                           original (mt/key-transformer {:decode keyword, :encode name}))))
          (is (= expected (m/decode
                           [:multi {:dispatch :id}
                            ["123" [:map [:id :string] [:github-followers pos-int?]]]]
                           original (mt/key-transformer {:decode keyword, :encode name, :types #{:map :multi}}))))
          (is (= expected (m/decode
                           [:multi {:dispatch :id}
                            ["123" [:map [:id :string] [:github-followers pos-int?]]]]
                           original (mt/key-transformer {:decode keyword, :encode name, :types :default}))))))))

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
        (= call-order @calls))

      [:map
       [:foo int?]
       [:bar string?]]
      {:foo 5 :bar "wee"}
      [[:enter :map]
       [:enter :int]
       [:leave :int]
       [:enter :string]
       [:leave :string]
       [:leave :map]]
      [:map-of int? string?]
      {5 "foo"}
      [[:enter :map-of]
       [:enter :int]
       [:leave :int]
       [:enter :string]
       [:leave :string]
       [:leave :map-of]]
      [:vector int?]
      [5 6]
      [[:enter :vector]
       [:enter :int]
       [:leave :int]
       [:enter :int]
       [:leave :int]
       [:leave :vector]]
      [:tuple string? int?]
      ["Foo" 5]
      [[:enter :tuple]
       [:enter :string]
       [:leave :string]
       [:enter :int]
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
              [:decode :enter ::m/val 1]
              [:decode :leave ::m/val 1]
              [:decode :enter ::m/val "2"]
              [:decode :enter 'string? "2"]
              [:decode :leave 'string? "2"]
              [:decode :leave ::m/val "2"]
              [:decode :leave :map {:x 1, :y "2"}]]
             @state)))

    (testing "encode"
      (reset! state nil)
      (m/encode schema {:x 1, :y "2"} transformer)
      (is (= [[:encode :enter :map {:x 1, :y "2"}]
              [:encode :enter ::m/val 1]
              [:encode :leave ::m/val 1]
              [:encode :enter ::m/val "2"]
              [:encode :enter 'string? "2"]
              [:encode :leave 'string? "2"]
              [:encode :leave ::m/val "2"]
              [:encode :leave :map {:x 1, :y "2"}]]
             @state)))))

(deftest schema-hinted-transformation
  (let [schema [string? {:title "lower-upper-string"
                         :decode/string 'str/upper-case
                         :encode/string 'str/lower-case}]
        schema2 [string? {:title "lower-upper-string"
                          :decode {:string 'str/upper-case}
                          :encode {:string 'str/lower-case}}]
        value "KiKkA"]
    (testing "defined transformations"
      (is (= "KIKKA" (m/decode schema value mt/string-transformer)))
      (is (= "KIKKA" (m/decode schema2 value mt/string-transformer)))
      (is (= "kikka" (m/encode schema value mt/string-transformer)))
      (is (= "kikka" (m/encode schema2 value mt/string-transformer)))
      (is (= "kikka" (as-> value $
                       (m/decode schema $ mt/string-transformer)
                       (m/encode schema $ mt/string-transformer))))
      (is (= "kikka" (as-> value $
                       (m/decode schema2 $ mt/string-transformer)
                       (m/encode schema2 $ mt/string-transformer)))))
    (testing "undefined transformations"
      (is (= value (m/decode schema value mt/json-transformer)))
      (is (= value (m/decode schema2 value mt/json-transformer)))
      (is (= value (m/encode schema value mt/json-transformer)))
      (is (= value (m/encode schema2 value mt/json-transformer)))))

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

  (testing "full override encode and decode"
    (testing "namespaced transformer names"
      (let [transformer (mt/transformer {:name ::kikka}
                                        mt/string-transformer)
            schema [keyword? {:encode {::kikka {:leave 'str/upper-case}}
                              :decode {::kikka {:enter 'str/lower-case}}}]]
        (is (= :kukka (m/decode schema "KUKKA" transformer)))
        (is (= "KUKKA" (m/encode schema :kukka transformer)))))
    (testing "full override masks compact keys with simple keyword names"
      (let [transformer (mt/transformer {:name :kikka})
            schema [:string {:encode {:kikka {:leave 'str/upper-case}}
                             :encode/kikka {:enter (partial str "masked")}
                             :decode {:kikka {:enter 'str/lower-case}}
                             :decode/kikka {:enter (partial str "masked")}}]]
        (is (= "kukka" (m/decode schema "KUKKA" transformer)))
        (is (= "KUKKA" (m/encode schema "kukka" transformer))))))

  (testing "and"
    (testing "decode"
      (are [x expected]
        (= expected
           (m/decode [:and {:decode/string {:enter #(if (re-matches #"\d{2}" %) (str % "0") %)
                                            :leave #(if (>= % 100) (* 10 %) %)}}
                      int?
                      [any? {:decode/string {:enter inc
                                             :leave (partial * 2)}}]]
                     x mt/string-transformer))
        "1" 4
        "11" 2220))
    (testing "encode"
      (are [x expected]
        (= expected
           (m/encode [:and {:encode/string {:enter #(if (> % 10) (* % 10) %)
                                            :leave #(if (re-matches #"<<\d{3}>>" %) (str "<<" % ">>") %)}}
                      keyword?
                      [pos-int? {:encode/string {:enter #(str "<<" %), :leave #(str % ">>")}}]
                      int?] x mt/string-transformer))
        1 "<<1>>"
        11 "<<<<110>>>>")))

  (testing "or"
    (testing "decode"
      (are [x expected]
        (= expected
           (m/decode [:or {:decode/string {:enter #(if (re-matches #"\d{2}" %) (str % "0") %)
                                           :leave #(cond (and (int? %) (>= % 100)) (* 10 %)
                                                         (keyword? %) (keyword "decoded" (name %))
                                                         :else %)}}
                      [pos-int? {:decode/string {:enter mt/-string->long
                                                 :leave #(if (int? %) (inc %) %)}}]
                      keyword?]
                     x mt/string-transformer))
        "1" 2
        "11" 1110
        "-1" :decoded/-1))
    (testing "encode"
      (are [x expected]
        (= expected
           (m/encode [:or {:encode/string {:enter #(if (> % 10) (* % 10) %)
                                           :leave #(if (re-matches #"<<\d{3}>>" %) (str "<<" % ">>") %)}}
                      keyword?
                      [pos-int? {:encode/string {:enter #(str "<<" %), :leave #(str % ">>")}}]
                      int?] x mt/string-transformer))
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
                      [[ifn? P1] "kikka" "KIKKA"]
                      [[:maybe P1 keyword?] "kikka" :KIKKA]
                      [[:vector PS keyword?] ["kikka"] [:KIKKA]]
                      [[:sequential PS keyword?] ["kikka"] [:KIKKA]]
                      [[:sequential PS keyword?] '("kikka") '(:KIKKA)]
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
      (= expected (m/decode schema nil mt/default-value-transformer))

      [:vector {:default [1 2 3]} int?] [1 2 3]
      [:map {:default {:x 10}} [:x int?]] {:x 10}
      [:tuple {:default [1 2]} int? int?] [1 2]
      [:map-of {:default {1 1}} int? int?] {1 1}))

  (testing "nested"
    (let [schema [:map {:default {}}
                  [:a {:default 1} int?]
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
    (is (= {:user/verified false} (m/decode [:map [:user/verified {:default false} boolean?]] {} mt/default-value-transformer)))
    (is (= false (m/decode [:and {:default false} boolean?] nil mt/default-value-transformer))))

  (testing "optional key"
    (is (= {:x 5} (m/decode [:map [:x :int] [:y {:optional true, :default 0} :int]] {:x 5}
                            (mt/default-value-transformer))))
    (is (= {:x 5, :y 0} (m/decode [:map [:x :int] [:y {:optional true, :default 0} :int]] {:x 5}
                                  (mt/default-value-transformer {::mt/add-optional-keys true}))))
    (is (= {:x 5} (m/decode [:map [:x :int] [:y {:optional true} [:int {:default 0}]]] {:x 5}
                            (mt/default-value-transformer))))
    (is (= {:x 5, :y 0} (m/decode [:map [:x :int] [:y {:optional true} [:int {:default 0}]]] {:x 5}
                                  (mt/default-value-transformer {::mt/add-optional-keys true})))))

  (testing "with custom options"
    (is (= false (m/decode [:and {:? false} boolean?] nil (mt/default-value-transformer {:key :?}))))
    (is (= {:user {:first-name "", :last-name ""}}
           (m/decode [:map
                      [:user [:map
                              [:first-name :string]
                              [:last-name :string]]]]
                     nil
                     (mt/default-value-transformer {:defaults {:map (constantly {})
                                                               :string (constantly "")}})))))

  (testing "nil as default value, #576"
    (is (= {:age nil
            :name ""
            :weight nil
            :address {:street ""
                      :lonlat nil}}
           (m/decode
            [:map
             [:age int?]
             [:name string?]
             [:weight double?]
             [:address [:map
                        [:street string?]
                        [:lonlat [:tuple double? double?]]]]]
            nil
            (mt/default-value-transformer
             {:defaults {:map (constantly {})
                         :tuple (constantly nil)
                         'string? (constantly "")
                         'int? (constantly nil)
                         'double? (constantly nil)}})))))

  (testing "default-fn applied on default value, when any"
    (let [schema [:map {:default {}}
                  [:first {:default 1} int?]
                  [:second {:default 2} int?]]]
      (testing "called on each defaulted value"
        (let [seen (atom [])
              transformer (mt/default-value-transformer {:default-fn (fn [_ x] (swap! seen conj x) x)})]
          (is (= {:first 1, :second 2} (m/encode schema nil transformer)))
          (is (= [{} 1 2] @seen))))

      (testing "only called on defaulted value"
        (let [seen (atom [])
              transformer (mt/default-value-transformer {:default-fn (fn [_ x] (swap! seen conj x) x)})]
          (is (= {:first -1, :second 2} (m/encode schema {:first -1} transformer)))
          (is (= [2] @seen)))))

    (testing "custom default :key"
      (let [schema [:map {}
                    [:first {:default 1, :name 'one} int?]
                    [:second {:default 2, :name 'two} int?]]
            seen (atom [])
            transformer (mt/default-value-transformer {:key :name, :default-fn (fn [_ x] (swap! seen conj x) x)})]
        (is (= {:first 'one, :second 'two} (m/encode schema {} transformer)))
        (is (= ['one 'two] @seen)))))

  (testing ":default/fn property"
    (testing "on schema"
      (let [schema [:string {:default/fn (fn [] "called")}]]
        (is (= "called" (m/decode schema nil mt/default-value-transformer)))))
    (testing "on map entry"
      (let [schema [:map [:s {:default/fn (fn [] "called")} :string]]]
        (is (= {:s "called"} (m/decode schema {} mt/default-value-transformer))))))
  (testing ":refs"
    (let [opts {:registry (mr/composite-registry m/default-registry
                                                 {"bing" :int})}
          transformer (mt/default-value-transformer {:defaults {:int (constantly 7)}})]
      (is (= 7 (m/decode [:ref "bing"] nil opts transformer)))
      (is (= [7] (m/decode [:vector [:ref "bing"]] [nil] opts transformer)))
      (is (= {:a 7} (m/decode [:map [:a [:ref "bing"]]] {:a nil} opts transformer)))
      (is (= {:a 7} (m/decode [:map [:a [:ref "bing"]]] {} opts transformer)))
      (is (= {:a 8} (m/decode [:map [:a [:ref {:default 8} "bing"]]] {:a nil} opts transformer)))
      (is (= {:a 8} (m/decode [:map [:a [:ref {:default 8} "bing"]]] {} opts transformer))))))

(deftest type-properties-based-transformations
  (is (= 12 (m/decode malli.core-test/Over6 "12" mt/string-transformer))))

(deftest map-of-json-keys-transform
  (let [schema [:map-of int? uuid?]]
    (doseq [data [{:0 "2ac307dc-4ec8-4046-9b7e-57716b7ecfd2"
                   :1 "820e5003-6fff-480b-9e2b-ec3cdc5d2f78"
                   :2 "017de28f-5801-8c62-9ce9-cef70883794a"}
                  {"0" "2ac307dc-4ec8-4046-9b7e-57716b7ecfd2"
                   "1" "820e5003-6fff-480b-9e2b-ec3cdc5d2f78"
                   "2" "017de28f-5801-8c62-9ce9-cef70883794a"}]]
      (testing data
        (is (= {0 #uuid"2ac307dc-4ec8-4046-9b7e-57716b7ecfd2"
                1 #uuid"820e5003-6fff-480b-9e2b-ec3cdc5d2f78"
                2 #uuid"017de28f-5801-8c62-9ce9-cef70883794a"}
               (m/decode schema data mt/json-transformer))))))
  #?(:clj
     (let [schema [:map-of uri? uri?]
           good "http://example.com"
           bad "invalid url"
           data {good good
                 bad bad}]
       (testing data
         (is (= {(URI. good) (URI. good)
                 bad bad}
                (m/decode schema data mt/json-transformer)))))))

#?(:clj
   (deftest -safe-test
     (let [schema [:int {:decode/num (mt/-safe #(Long/parseLong %))}]]
       (is (= 1 (m/decode schema "1" (mt/transformer {:name :num}))))
       (is (= 1 (m/decode schema 1 (mt/transformer {:name :num}))))
       (is (= "safe" (m/decode schema "safe" (mt/transformer {:name :num})))))))

(deftest regression-480-test
  (let [value {:b #uuid"f5a54a8f-7d78-4495-9138-e810885d1cdb"}
        schema [:map [:a :int] [:b :uuid]]]
    (is (= value
           (as-> value $
             (m/encode schema $ mt/string-transformer)
             (m/decode schema $ mt/string-transformer))))))

(def child-inference-test-schema
  [:map
   [:enum1 [:enum :kikka :kukka]]
   [:enum2 [:enum 'kikka 'kukka]]
   [:enum3 [:enum 1 2]]
   [:enum4 [:enum 1.1 2.2]]
   [:equals1 [:= :kikka]]
   [:equals2 [:= 'kikka]]
   [:equals3 [:= 1]]
   [:equals4 [:= 1.1]]])

(deftest child-inference-default-test
  (let [encoded {:enum1 "kikka"
                 :enum2 "kikka"
                 :enum3 "1"
                 :enum4 "1.1"
                 :equals1 "kikka"
                 :equals2 "kikka"
                 :equals3 "1"
                 :equals4 "1.1"}
        decoded {:enum1 :kikka
                 :enum2 'kikka
                 :enum3 1
                 :enum4 1.1
                 :equals1 :kikka
                 :equals2 'kikka
                 :equals3 1
                 :equals4 1.1}]
    (testing "decoding is not enabled by default"
      (is (= encoded (m/decode child-inference-test-schema encoded nil))))
    (testing "encoding is not enabled by default"
      (is (= decoded (m/encode child-inference-test-schema decoded nil))))))

(deftest child-inference-json-test
  (let [encoded {:enum1 "kikka"
                 :enum2 "kikka"
                 :enum3 1
                 :enum4 1.1
                 :equals1 "kikka"
                 :equals2 "kikka"
                 :equals3 1
                 :equals4 1.1}

        decoded {:enum1 :kikka
                 :enum2 'kikka
                 :enum3 1
                 :enum4 1.1
                 :equals1 :kikka
                 :equals2 'kikka
                 :equals3 1
                 :equals4 1.1}

        stringy-encoded {:enum1 "kikka"
                         :enum2 "kikka"
                         :enum3 "1"
                         :enum4 "1.1"
                         :equals1 "kikka"
                         :equals2 "kikka"
                         :equals3 "1"
                         :equals4 "1.1"}
        stringy-decoded {:enum1 :kikka
                         :enum2 'kikka
                         :enum3 "1"
                         :enum4 "1.1"
                         :equals1 :kikka
                         :equals2 'kikka
                         :equals3 "1"
                         :equals4 "1.1"}]
    (testing "decoding children using the json transformer works"
      (is (= decoded (m/decode child-inference-test-schema encoded (mt/json-transformer))))
      (is (= decoded (m/decode child-inference-test-schema decoded (mt/json-transformer)))))
    (testing "invalid strings are not decoded by the json transformer"
      (is (= stringy-decoded (m/decode child-inference-test-schema stringy-encoded (mt/json-transformer))))
      (is (= stringy-decoded (m/decode child-inference-test-schema stringy-decoded (mt/json-transformer)))))
    (testing "encoding children using the json transformer works"
      (is (= encoded (m/encode child-inference-test-schema decoded (mt/json-transformer))))
      (is (= encoded (m/encode child-inference-test-schema encoded (mt/json-transformer)))))))

(deftest child-inference-string-test
  (let [encoded {:enum1 "kikka"
                 :enum2 "kikka"
                 :enum3 "1"
                 :enum4 "1.1"
                 :equals1 "kikka"
                 :equals2 "kikka"
                 :equals3 "1"
                 :equals4 "1.1"}
        decoded {:enum1 :kikka
                 :enum2 'kikka
                 :enum3 1
                 :enum4 1.1
                 :equals1 :kikka
                 :equals2 'kikka
                 :equals3 1
                 :equals4 1.1}]
    (testing "decoding children using the string transformer works"
      (is (= decoded (m/decode child-inference-test-schema encoded (mt/string-transformer))))
      (is (= decoded (m/decode child-inference-test-schema decoded (mt/string-transformer)))))
    (testing "encoding children using the string transformer works"
      (is (= encoded (m/encode child-inference-test-schema decoded (mt/string-transformer))))
      (is (= encoded (m/encode child-inference-test-schema encoded (mt/string-transformer)))))))
