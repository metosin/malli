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
    (testing "decode"
      (is (= 1 (m/decode int? "1" transform/string-transformer)))
      (is (= "1" (m/decode int? "1" transform/json-transformer)))
      (is (= :user/kikka (m/decode keyword? "user/kikka" transform/string-transformer))))
    (testing "encode"
      (is (= "1" (m/encode int? 1 transform/string-transformer)))
      (is (= "1" (m/encode int? 1 transform/json-transformer)))
      (is (= "user/kikka" (m/encode keyword? :user/kikka transform/string-transformer)))))
  (testing "comparators"
    (testing "decode"
      (doseq [schema (keys m/comparator-registry)]
        (is (= 1 (m/decode [schema 1] "1" transform/string-transformer)))))
    (testing "encode"
      (doseq [schema (keys m/comparator-registry)]
        (is (= "1" (m/encode [schema 1] 1 transform/string-transformer))))))
  (testing "and"
    (testing "decode"
      (is (= 1 (m/decode [:and int?] "1" transform/string-transformer)))
      (is (= :1 (m/decode [:and keyword?] "1" transform/string-transformer)))
      (is (= 1 (m/decode [:and int? keyword?] "1" transform/string-transformer)))
      (is (= 1 (m/decode [:and int? [:enum 1 2]] "1" transform/string-transformer)))
      (is (= :1 (m/decode [:and keyword? int?] "1" transform/string-transformer)))
      (is (= [1] (m/decode [:and [:vector int?]] ["1"] transform/string-transformer))))
    (testing "encode"
      (is (= "1" (m/encode [:and int?] 1 transform/string-transformer)))
      (is (= "1" (m/encode [:and keyword?] :1 transform/string-transformer)))
      (is (= "1" (m/encode [:and int? keyword?] 1 transform/string-transformer)))
      (is (= "1" (m/encode [:and int? [:enum 1 2]] 1 transform/string-transformer)))
      (is (= "1" (m/encode [:and keyword? int?] :1 transform/string-transformer)))
      (is (= ["1"] (m/encode [:and [:vector int?]] [1] transform/string-transformer)))))
  (testing "or"
    (testing "decode"
      (is (= 1 (m/decode [:or int? keyword?] "1" transform/string-transformer)))
      (is (= 1 (m/decode [:or int? [:enum 1 2]] "1" transform/string-transformer)))
      (is (= :1 (m/decode [:or keyword? int?] "1" transform/string-transformer))))
    (testing "encode"
      (is (= "1" (m/encode [:or int? keyword?] 1 transform/string-transformer)))
      (is (= "1" (m/encode [:or int? [:enum 1 2]] 1 transform/string-transformer)))
      (is (= "1" (m/encode [:or keyword? int?] 1 transform/string-transformer)))))
  ;; TODO: encode
  (testing "collections"
    (is (= #{1 2 3} (m/decode [:set int?] ["1" 2 "3"] transform/string-transformer)))
    (is (= #{"1" 2 "3"} (m/decode [:set [:enum 1 2]] ["1" 2 "3"] transform/string-transformer)))
    (is (= #{"1" 2 "3"} (m/decode [:set int?] ["1" 2 "3"] transform/json-transformer)))
    (is (= [:1 2 :3] (m/decode [:vector keyword?] ["1" 2 "3"] transform/string-transformer)))
    (is (= '(:1 2 :3) (m/decode [:list keyword?] '("1" 2 "3") transform/string-transformer)))
    (is (= '(:1 2 :3) (m/decode [:list keyword?] (seq '("1" 2 "3")) transform/string-transformer)))
    (is (= '(:1 2 :3) (m/decode [:list keyword?] (lazy-seq '("1" 2 "3")) transform/string-transformer)))
    (is (= ::invalid (m/decode [:vector keyword?] ::invalid transform/string-transformer))))
  (testing "map"
    (testing "decode"
      (is (= {:c1 1, ::c2 :kikka} (m/decode [:map [:c1 int?] [::c2 keyword?]] {:c1 "1", ::c2 "kikka"} transform/string-transformer)))
      (is (= {:c1 "1", ::c2 :kikka} (m/decode [:map [::c2 keyword?]] {:c1 "1", ::c2 "kikka"} transform/json-transformer)))
      (is (= ::invalid (m/decode [:map] ::invalid transform/json-transformer))))
    (testing "encode"
      (is (= {:c1 "1", ::c2 "kikka"} (m/encode [:map [:c1 int?] [::c2 keyword?]] {:c1 1, ::c2 :kikka} transform/string-transformer)))
      (is (= {:c1 1, ::c2 "kikka"} (m/encode [:map [::c2 keyword?]] {:c1 1, ::c2 :kikka} transform/json-transformer)))
      (is (= ::invalid (m/encode [:map] ::invalid transform/json-transformer)))))
  #_(testing "s/map-of"
      (is (= {1 :abba, 2 :jabba} (m/decode (s/map-of int? keyword?) {"1" "abba", "2" "jabba"} transform/string-transformer)))
      (is (= {"1" :abba, "2" :jabba} (m/decode (s/map-of int? keyword?) {"1" "abba", "2" "jabba"} transform/json-transformer)))
      (is (= ::invalid (m/decode (s/map-of int? keyword?) ::invalid transform/json-transformer))))
  (testing "maybe"
    (testing "decode"
      (is (= 1 (m/decode [:maybe int?] "1" transform/string-transformer)))
      (is (= nil (m/decode [:maybe int?] nil transform/string-transformer))))
    (testing "encode"
      (is (= "1" (m/encode [:maybe int?] 1 transform/string-transformer)))
      (is (= nil (m/encode [:maybe int?] nil transform/string-transformer)))))
  (testing "tuple"
    (testing "decode"
      (is (= [1] (m/decode [:tuple int?] ["1"] transform/string-transformer)))
      (is (= [1 :kikka] (m/decode [:tuple int? keyword?] ["1" "kikka"] transform/string-transformer)))
      (is (= [:kikka 1] (m/decode [:tuple keyword? int?] ["kikka" "1"] transform/string-transformer)))
      (is (= "1" (m/decode [:tuple keyword? int?] "1" transform/string-transformer)))
      (is (= [:kikka 1 "2"] (m/decode [:tuple keyword? int?] ["kikka" "1" "2"] transform/string-transformer))))
    (testing "encode"
      (is (= ["1"] (m/encode [:tuple int?] [1] transform/string-transformer)))
      (is (= ["1" "kikka"] (m/encode [:tuple int? keyword?] [1 :kikka] transform/string-transformer)))
      (is (= ["kikka" "1"] (m/encode [:tuple keyword? int?] [:kikka 1] transform/string-transformer)))
      (is (= 1.0 (m/encode [:tuple keyword? int?] 1.0 transform/string-transformer)))
      (is (= ["kikka" "1" "2"] (m/encode [:tuple keyword? int?] [:kikka 1 "2"] transform/string-transformer))))))

;; TODO: this is wrong!
(deftest collection-transform-test
  (testing "decode"
    (is (= #{1 2 3} (m/decode [:set int?] [1 2 3] transform/collection-transformer))))
  (testing "encode"
    (is (= #{1 2 3} (m/encode [:set int?] [1 2 3] transform/collection-transformer)))))

(deftest composing-transformers
  (let [strict-json-transformer (transform/transformer
                                  transform/strip-extra-keys-transformer
                                  transform/json-transformer)]
    (testing "decode"
      (is (= :kikka (m/decode keyword? "kikka" strict-json-transformer)))
      (is (= {:x :kikka} (m/decode [:map [:x keyword?]] {:x "kikka", :y "kukka"} strict-json-transformer))))
    (testing "encode"
      (is (= "kikka" (m/encode keyword? :kikka strict-json-transformer)))
      (is (= {:x "kikka"} (m/encode [:map [:x keyword?]] {:x :kikka, :y :kukka} strict-json-transformer)))))

  (let [strip-extra-key-transformer (transform/transformer
                                      transform/string-transformer
                                      transform/strip-extra-keys-transformer
                                      (transform/key-transformer
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
  (let [key-transformer (transform/key-transformer
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
