(ns malli.transform-test
  (:require [clojure.test :refer [deftest testing is]]
            [malli.core :as m]
            [malli.transform :as transform]))

(deftest string->long
  (is (= 1 (transform/string->long "1")))
  (is (= 1 (transform/string->long 1)))
  (is (= "abba" (transform/string->long "abba"))))

(deftest string->double
  (is (= 1.0 (transform/string->double "1")))
  (is (= 1.0 (transform/string->double 1.0)))
  (is (= 1 (transform/string->double 1)))
  (is (= "abba" (transform/string->double "abba"))))

(deftest string->keyword
  (is (= :abba (transform/string->keyword "abba")))
  (is (= :abba (transform/string->keyword :abba))))

(deftest string->boolean
  (is (= true (transform/string->boolean "true")))
  (is (= false (transform/string->boolean "false")))
  (is (= "abba" (transform/string->boolean "abba"))))

(deftest string->uuid
  (is (= #uuid"5f60751d-9bf7-4344-97ee-48643c9949ce" (transform/string->uuid "5f60751d-9bf7-4344-97ee-48643c9949ce")))
  (is (= #uuid"5f60751d-9bf7-4344-97ee-48643c9949ce" (transform/string->uuid #uuid"5f60751d-9bf7-4344-97ee-48643c9949ce")))
  (is (= "abba" (transform/string->uuid "abba"))))

(deftest string->date
  (is (= #inst "2018-04-27T18:25:37Z" (transform/string->date "2018-04-27T18:25:37Z")))
  (is (= #inst "2018-04-27T00:00:00Z" (transform/string->date "2018-04-27")))
  (is (= #inst "2018-04-27T05:00:00Z" (transform/string->date "2018-04-27T08:00:00+03:00")))
  (is (= #inst "2018-04-27T18:25:37Z" (transform/string->date "2018-04-27T18:25:37.000Z")))
  (is (= #inst "2018-04-27T18:25:37Z" (transform/string->date "2018-04-27T18:25:37.000+0000")))
  (is (= #inst "2014-02-18T18:25:37Z" (transform/string->date #inst "2014-02-18T18:25:37Z")))
  (is (= #inst "2018-04-27T00:00:00Z" (transform/string->date #inst "2018-04-27")))
  (is (= #inst "2018-04-27T05:00:00Z" (transform/string->date #inst "2018-04-27T08:00:00+03:00")))
  (is (= "abba" (transform/string->date "abba"))))

(deftest date->string
  (is (= "2014-02-18T18:25:37.000Z" (transform/date->string #inst "2014-02-18T18:25:37Z")))
  (is (= "abba" (transform/date->string "abba"))))

(deftest string->symbol
  (is (= 'inc (transform/string->symbol "inc")))
  (is (= 'inc (transform/string->symbol 'inc))))

(deftest string->nil
  (is (= nil (transform/string->nil "")))
  (is (= nil (transform/string->nil nil))))

(deftest number->double
  #?(:clj (is (= 0.5 (transform/number->double 1/2))))
  (is (= 1.0 (transform/number->double 1)))
  (is (= "kikka" (transform/number->double "kikka"))))

(deftest any->string
  #?(:clj (is (= "1/2" (transform/any->string 1/2))))
  (is (= "0.5" (transform/any->string 0.5)))
  (is (= nil (transform/any->string nil))))

(deftest any->any
  #?(:clj (is (= 1/2 (transform/any->any 1/2))))
  (is (= 0.5 (transform/any->any 0.5)))
  (is (= nil (transform/any->any nil))))

(deftest transform-test
  (testing "predicates"
    (is (= 1 (m/transform int? "1" transform/string-transformer)))
    (is (= "1" (m/transform int? "1" transform/json-transformer)))
    (is (= :user/kikka (m/transform keyword? "user/kikka" transform/string-transformer))))
  (testing "comparators"
    (doseq [schema (keys m/comparator-registry)]
      (is (= 1 (m/transform [schema 1] "1" transform/string-transformer)))))
  (testing "and"
    (is (= 1 (m/transform [:and int?] "1" transform/string-transformer)))
    (is (= :1 (m/transform [:and keyword?] "1" transform/string-transformer)))
    (is (= 1 (m/transform [:and int? keyword?] "1" transform/string-transformer)))
    (is (= 1 (m/transform [:and int? [:enum 1 2]] "1" transform/string-transformer)))
    (is (= :1 (m/transform [:and keyword? int?] "1" transform/string-transformer)))
    (is (= [1] (m/transform [:and [:vector int?]] ["1"] transform/string-transformer))))
  (testing "or"
    (is (= 1 (m/transform [:or int? keyword?] "1" transform/string-transformer)))
    (is (= 1 (m/transform [:or int? [:enum 1 2]] "1" transform/string-transformer)))
    (is (= :1 (m/transform [:or keyword? int?] "1" transform/string-transformer))))
  (testing "collections"
    (is (= #{1 2 3} (m/transform [:set int?] ["1" 2 "3"] transform/string-transformer)))
    (is (= #{"1" 2 "3"} (m/transform [:set [:enum 1 2]] ["1" 2 "3"] transform/string-transformer)))
    (is (= #{"1" 2 "3"} (m/transform [:set int?] ["1" 2 "3"] transform/json-transformer)))
    (is (= [:1 2 :3] (m/transform [:vector keyword?] ["1" 2 "3"] transform/string-transformer)))
    (is (= '(:1 2 :3) (m/transform [:list keyword?] '("1" 2 "3") transform/string-transformer)))
    (is (= '(:1 2 :3) (m/transform [:list keyword?] (seq '("1" 2 "3")) transform/string-transformer)))
    (is (= '(:1 2 :3) (m/transform [:list keyword?] (lazy-seq '("1" 2 "3")) transform/string-transformer)))
    (is (= ::invalid (m/transform [:vector keyword?] ::invalid transform/string-transformer))))
  (testing "map"
    (is (= {:c1 1, ::c2 :kikka} (m/transform [:map [:c1 int?] [::c2 keyword?]] {:c1 "1", ::c2 "kikka"} transform/string-transformer)))
    (is (= {:c1 "1", ::c2 :kikka} (m/transform [:map [::c2 keyword?]] {:c1 "1", ::c2 "kikka"} transform/json-transformer)))
    (is (= ::invalid (m/transform [:map] ::invalid transform/json-transformer))))
  #_(testing "s/map-of"
    (is (= {1 :abba, 2 :jabba} (m/transform (s/map-of int? keyword?) {"1" "abba", "2" "jabba"} transform/string-transformer)))
    (is (= {"1" :abba, "2" :jabba} (m/transform (s/map-of int? keyword?) {"1" "abba", "2" "jabba"} transform/json-transformer)))
    (is (= ::invalid (m/transform (s/map-of int? keyword?) ::invalid transform/json-transformer))))
  (testing "maybe"
    (is (= 1 (m/transform [:maybe int?] "1" transform/string-transformer)))
    (is (= nil (m/transform [:maybe int?] nil transform/string-transformer))))
  (testing "tuple"
    (is (= [1] (m/transform [:tuple int?] ["1"] transform/string-transformer)))
    (is (= [1 :kikka] (m/transform [:tuple int? keyword?] ["1" "kikka"] transform/string-transformer)))
    (is (= [:kikka 1] (m/transform [:tuple keyword? int?] ["kikka" "1"] transform/string-transformer)))
    (is (= "1" (m/transform [:tuple keyword? int?] "1" transform/string-transformer)))
    (is (= [:kikka 1 "2"] (m/transform [:tuple keyword? int?] ["kikka" "1" "2"] transform/string-transformer)))))
