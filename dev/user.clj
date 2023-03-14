(ns user
  (:require
   [clojure.pprint :refer [pprint]]
   [clojure.test :as test]
   [clojure.tools.namespace.repl :as r]
   [clojure.walk :refer [macroexpand-all]]))

(r/set-refresh-dirs "src/malli" "dev" "test/malli")

(defn- run-test
  ([] (run-test #"^malli.*test$"))
  ([o]
   (r/refresh)
   (cond
     (instance? java.util.regex.Pattern o)
     (test/run-all-tests o)

     (symbol? o)
     (if-let [sns (namespace o)]
       (do (require (symbol sns))
           (test/test-vars [(resolve o)]))
       (test/test-ns o)))))

(comment
  ;; Refresh changed namespaces
  (r/refresh)

  ;; Run all tests
  (run-test)

  ;; Run all transform tests
  (run-test 'malli.transform-test)

  ;; Run a specific test case of transform tests
  (run-test 'malli.transform-test/string->uuid)

  )
