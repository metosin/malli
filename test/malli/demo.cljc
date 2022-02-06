(ns malli.demo
  (:require [malli.core :as m]
            [malli.dev :as dev]
            [malli.experimental :as mx]))

(defn kikka
  "schema via var metadata"
  {:malli/schema [:=> [:cat :int] :int]}
  [x] (inc x))

(m/=> kukka [:=> [:cat :int] :int])
(defn kukka
  "schema via separate declaration"
  [x] (inc x))

(mx/defn kakka :- :int
  "inline schemas (plumatic-style)"
  [x :- :int] (inc x))

(comment
  (dev/start!)
  (dev/stop!))

(comment
  (kikka "1")
  (kukka 1 2)
  (kakka "1"))
