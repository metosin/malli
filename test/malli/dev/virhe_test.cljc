(ns malli.dev.virhe-test
  (:require [clojure.test :refer [deftest is testing]]
            [malli.dev.virhe :as virhe]))

(deftest -printer-test
  (testing "function values can be printed"
    (is (virhe/-visit {:fn inc} (virhe/-printer)))))

