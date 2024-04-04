(ns malli.constraint.string.generate
  (:require [malli.impl.util :as miu]))

(defn- -string-class [k]
  (fn [{:keys [constraint]} options]
    (let [[s] (next constraint)]
      (assert (not (false? s)))
      [{:string-class #{k}}])))

(defn solvers []
  {:alpha-string (-string-class :alpha)
   :non-alpha-string (-string-class :non-alpha)
   :numeric-string (-string-class :numeric)
   :non-numeric-string (-string-class :non-numeric)
   :alphanumeric-string (-string-class :alphanumeric)
   :edn-string (fn [{:keys [constraint]} options]
                 (let [[s] (next constraint)]
                   (assert (not (false? s)))
                   [{:string-class #{(cond-> [:edn]
                                       s (conj s))}}]))
   })


(defn negate-string-class [cls]
  {:pre [(vector? cls)]}
  (case (first cls)
    :alpha [:not-alpha]
    :not-alpha [:alpha]
    :numeric [:not-numeric]
    :not-numeric [:numeric]
    :alphanumeric [:not-alphanumeric]
    :not-alphanumeric [:alphanumeric]
    (miu/-fail! ::unsupported-negated-class
                {:string-class cls})))

(defn generators []
  {})
