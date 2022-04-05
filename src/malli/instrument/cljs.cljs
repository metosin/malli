(ns malli.instrument.cljs
  (:require-macros [malli.instrument.cljs])
  (:require [malli.generator :as mg]))

(defonce instrumented-vars (atom {}))

(defn -filter-var [f] (fn [_ s _] (f s)))
(defn -filter-ns [& ns] (fn [n _ _] ((set ns) n)))

(defn perform-check [schema f]
  (mg/check schema f))
