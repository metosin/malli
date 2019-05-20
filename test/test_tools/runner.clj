(ns test-tools.runner
  (:require [circleci.test :as ci-test])
  (:gen-class))

;; As circleci does not contain a way to run all tests from dir
;; through it's main method, this wrapper is needed for deps

(defn -main [dir & _]
  (when (empty? dir)
    (println "Error! Required test directory argument is missing")
    (System/exit 1))
  (println "About to start running tests from directory:" dir)
  (ci-test/dir (pr-str [dir])))

