(ns malli.helpers)

(def defs-small-int
  [:int {:max 6}])

(def int-schema :int)

(defn x+y
  {:malli/schema [:=> [:cat float? float?] :double]}
  [x y]
  (+ x y))
