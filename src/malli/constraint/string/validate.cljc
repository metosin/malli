(ns malli.constraint.string.validate
  (:require [clojure.string :as str]
            [malli.constraint.char :as char]))

(defn -idempotent [f] (fn [s] (= s (f s))))

(defn validators []
  {:alpha-string (fn [s] (every? char/alpha? s))
   :non-alpha-string (fn [s] (not-any? char/alpha? s))
   :numeric-string (fn [s] (every? char/numeric? s))
   :non-numeric-string (fn [s] (not-any? char/numeric? s))
   :alphanumeric-string (fn [s] (every? char/alphanumeric? s))
   :non-alphanumeric-string (fn [s] (not-any? char/alphanumeric? s))
   :trim-string (-idempotent str/trim)
   :triml-string (-idempotent str/triml)
   :trimr-string (-idempotent str/trimr)})
