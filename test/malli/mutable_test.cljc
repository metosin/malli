(ns malli.mutable-test
  (:require [clojure.test :refer [deftest testing is]]
            [malli.core :as m]
            [malli.registry :as mr]))

(deftest mutable-test
  (let [registry* (atom {})
        registry (mr/mutable-registry (m/default-schemas) registry*)
        register! (fn [t ?s] (swap! registry* assoc t (m/schema ?s)))]
    (testing "default registy"
      (is (thrown? #?(:clj Exception, :cljs js/Error) (m/validate ::id 1 {:registry registry})))
      (register! ::id int?)
      (is (true? (m/validate ::id 1 {:registry registry}))))))
