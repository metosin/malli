(ns malli.mutable-test
  (:require [clojure.test :refer [deftest testing is]]
            [malli.core :as m]
            [malli.mutable :as mm]))

(deftest mutable-test
  (let [registry (mm/default-registry)]
    (testing "default registy"
      (is (thrown? #?(:clj Exception, :cljs js/Error) (m/validate ::id 1 {:registry registry})))
      (mm/register! ::id int?)
      (is (true? (m/validate ::id 1 {:registry registry})))
      (mm/register! ::id nil))))
