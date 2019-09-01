(ns malli.generator-test
  (:require [clojure.test :refer [deftest testing is]]
            [malli.json-schema-test :as json-schema-test]
            [malli.generator :as mg]
            [malli.core :as m]))

(deftest generator-test
  (doseq [[?schema] json-schema-test/expectations
          value (mg/sample (mg/generator ?schema))]
    (testing (m/form ?schema)
      (is (m/validate ?schema value)))))
