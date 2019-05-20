(ns malli.core-test
  (:require [clojure.test :refer :all]
            [malli.core :as malli]))

(deftest expand-test
  (is (= (malli/expand {::foo (fn [arg] arg)} [::foo ::bar]) ::bar))

  (is (= (malli/expand malli/default-registry ::malli/int) malli/Int)))
