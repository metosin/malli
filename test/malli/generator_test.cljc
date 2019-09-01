(ns malli.generator-test
  (:require [clojure.test :refer [deftest testing is]]
            [malli.json-schema-test :as json-schema-test]
            [malli.generator :as mg]
            [malli.core :as m]))

(deftest generator-test
  (doseq [[?schema] json-schema-test/expectations]
    (testing (m/form ?schema)
      (is (= (mg/sample ?schema {:seed 123})
             (mg/sample ?schema {:seed 123})
             (mg/sample (mg/generator ?schema) {:seed 123})
             (mg/sample (mg/generator ?schema) {:seed 123})))
      (doseq [value (mg/sample ?schema)]
        (is (m/validate ?schema value))))))
