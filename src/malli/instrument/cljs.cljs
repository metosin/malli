(ns malli.instrument.cljs
  (:require-macros [malli.instrument.cljs])
  (:require [malli.generator :as mg]))

(defonce instrumented-vars (atom {}))

(defn -filter-var [f] (fn [_ s _] (f s)))
(defn -filter-ns [& ns] (fn [n _ _] ((set ns) n)))

(defn meta-fn
  ;; Taken from https://clojure.atlassian.net/browse/CLJS-3018
  ;; Because the current MetaFn implementation can cause quirky errors in CLJS
  [f m]
  (let [new-f (goog/bind f #js{})]
    (js/Object.assign new-f f)
    (specify! new-f IMeta #_:clj-kondo/ignore (-meta [_] m))
    new-f))

(defn perform-check [schema f]
  (mg/check schema f))
