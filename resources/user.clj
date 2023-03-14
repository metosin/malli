(ns user
  (:require
   [clojure.pprint :refer [pprint]]
   [clojure.test :as test]
   [clojure.tools.namespace.repl :as r]
   [clojure.walk :refer [macroexpand-all]]))

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
