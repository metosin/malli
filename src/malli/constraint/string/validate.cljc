(ns malli.constraint.string.validate
  (:require [malli.constraint.char :as char]))

(defn validators []
  {:alpha-string (fn [s] (every? char/alpha? s))
   :non-alpha-string (fn [s] (not-any? char/alpha? s))
   :numeric-string (fn [s] (every? char/numeric? s))
   :non-numeric-string (fn [s] (not-any? char/numeric? s))
   :alphanumeric-string (fn [s] (every? char/alphanumeric? s))
   :non-alphanumeric-string (fn [s] (not-any? char/alphanumeric? s))})
