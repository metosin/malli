(ns malli.demo
  (:require [malli.core :as m]
            [malli.dev :as dev]
            [malli.experimental :as mx]))

;; via var metadata
(defn kikka
  {:malli/schema [:-> :int :int]}
  [x] (inc x))

;; external malli definition
(m/=> kukka [:-> :int :int])
(defn kukka [x]
  (inc x))

;; inline schemas (plumatic-style)
(mx/defn kakka :- :int [x :- :int]
  (inc x))

(comment
 (dev/start!)
 (dev/stop!))

(comment
 (kikka "1")
 (kukka "1")
 (kakka "1"))
