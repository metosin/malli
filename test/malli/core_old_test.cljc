(ns malli.core-old-test
  (:require [clojure.test :refer [deftest is]]
            [malli.core-old :as malli]))

(deftest expand-test
  (is (= (malli/expand {::foo (fn [arg] arg)} [::foo ::bar]) ::bar))
  (is (= (malli/expand malli/default-registry ::malli/int) malli/Int)))
