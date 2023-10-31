(ns malli.assert-test
  (:refer-clojure :exclude [assert])
  (:require
   [clojure.test :refer [deftest is]]
   [malli.core :refer [assert]]))


(set! *assert* true)

(deftest assert-throws-test
  (is (thrown? #?(:clj Exception, :cljs js/Error)
               (assert :int "42" )))
  (is (thrown? #?(:clj Exception, :cljs js/Error)
               (assert int? "42" )))
  (is (thrown? #?(:clj Exception, :cljs js/Error)
               (assert string? 42)))
  (is (thrown? #?(:clj Exception, :cljs js/Error)
               (assert int? nil)))
  (is (thrown? #?(:clj Exception, :cljs js/Error)
               (assert [:map [:a int?]] {:a "42"})))
  (is (thrown? #?(:clj Exception, :cljs js/Error)
               (assert ::invalid-schema 42))))

(deftest assert-checked-and-does-not-throw
  (is (= 42 (assert :int 42 )))
  (is (= 42 (assert int? 42 )))
  (is (= "42" (assert string? "42")))
  (is (= nil (assert any? nil)))
  (is (= {:a 42} (assert [:map [:a int?]] {:a 42}))))
