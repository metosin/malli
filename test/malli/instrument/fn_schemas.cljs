(ns malli.instrument.fn-schemas
  (:require [malli.experimental :as mx]
            [malli.instrument.fn-schemas2 :as schemas :refer [small-int int-arg VecOfStrings]]))

(def VecOfInts [:vector :int])

(defn sum-nums
  {:malli/schema [:=> [:cat VecOfInts] :int]}
  [args]
  (apply + args))

(defn sum-nums2
  {:malli/schema [:=> [:cat VecOfInts] float?]}
  [args]
  (apply + args))

(defn str-join
  {:malli/schema [:=> [:cat VecOfStrings] schemas/string]}
  [args]
  (apply str args))

(def str-join-schema [:=> [:cat VecOfStrings] schemas/string])

(defn str-join2
  {:malli/schema [:-> VecOfStrings schemas/string]}
  [args]
  (apply str args))

(defn str-join3
  {:malli/schema str-join-schema}
  [args]
  (apply str args))

(defn str-join4
  {:malli/schema [:=> [:cat malli.instrument.fn-schemas2/VecOfStrings] schemas/string]}
  [args]
  (apply str args))

(mx/defn str-join-mx2 :- int?
  [args :- VecOfInts]
  (apply str args))

(mx/defn power-ret-refer :- small-int
  [x :- :int] (* x x))

(mx/defn power-ret-ns :- schemas/small-int
  [x :- :int] (* x x))

(mx/defn power-arg-refer :- [:int {:max 6}]
  [x :- int-arg] (* x x))

(mx/defn power-arg-ns :- [:int {:max 6}]
  [x :- schemas/int-arg] (* x x))

(mx/defn power-full :- malli.instrument.fn-schemas2/small-int
  [x :- malli.instrument.fn-schemas2/int-arg] (* x x))

(mx/defn power-int? :- small-int
  [x :- int?] (* x x))
