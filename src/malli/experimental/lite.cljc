(ns malli.experimental.lite
  (:refer-clojure :exclude [set vector and or])
  (:require [malli.core :as m]))

(declare schema)

(defrecord -Optional [value])
(defn -schema [t & xs] (schema (into [t] (map schema xs))))
(defn -entry [[k v]]
  (let [[v optional] (if (instance? -Optional v) [(:value v) true] [v])]
    (cond-> [k] optional (conj {:optional true}) :always (conj (schema v)))))

(defn schema [x] (m/schema (if (map? x) (into [:map] (map -entry x)) x)))

(defn optional [x] (->-Optional x))
(defn maybe [x] (-schema :maybe x))
(defn set [x] (-schema :set x))
(defn vector [x] (-schema :vector x))
(defn map-of [k v] (-schema :map-of k v))
(defn tuple [& xs] (apply -schema :tuple xs))
(defn and [& xs] (apply -schema :and xs))
(defn or [& xs] (apply -schema :or xs))
