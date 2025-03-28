(ns bb-test-runner
  (:require
   [clojure.test :as t]
   [malli.clj-kondo-test]
   [malli.core-test]
   [malli.destructure-test]
   [malli.dot-test]
   [malli.error-test]
   [malli.experimental-test]
   [malli.generator-test]
   [malli.instrument-test]
   [malli.json-schema-test]
   [malli.parser-test]
   [malli.plantuml-test]
   [malli.provider-test]
   [malli.registry-test]
   [malli.swagger-test]
   [malli.transform-test]
   [malli.util-test]))

(defn run-tests [& _args]
  (let [{:keys [fail error]}
        (t/run-tests
         'malli.core-test
         'malli.clj-kondo-test
         'malli.destructure-test
         'malli.dot-test
         'malli.error-test
         'malli.experimental-test
         'malli.instrument-test
         'malli.json-schema-test
         ;; 'malli.generator-test ;; skipped for now due to test.chuck incompatibility
         'malli.parser-test
         'malli.plantuml-test
         'malli.provider-test
         'malli.registry-test
         'malli.swagger-test
         'malli.transform-test
         'malli.util-test)]
    (when (or (pos? fail)
              (pos? error))
      (System/exit 1))))
