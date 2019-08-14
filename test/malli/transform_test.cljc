(ns malli.transform-test
  (:require [clojure.test :refer [deftest testing is]]
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

(deftest keyword->string
  (is (= "abba" (transform/keyword->string :abba)))
  (is (= "jabba/abba" (transform/keyword->string :jabba/abba)))
  (is (= "abba" (transform/keyword->string "abba"))))

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
