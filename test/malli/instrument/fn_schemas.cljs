(ns malli.instrument.fn-schemas
  (:require [malli.instrument.fn-schemas2 :as schemas :refer [VecOfStrings]]))

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
  {:malli/schema [:=> [:cat VecOfStrings] schemas/string]}
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
