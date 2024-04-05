(ns malli.constraint.string.generate
  (:require [malli.impl.util :as miu]))

(defn- -string-class [k]
  (fn [{:keys [constraint]} options]
    (let [[s] (next constraint)]
      (assert (not (false? s)))
      [{:string-class #{k}}])))

(defn solvers []
  {:alpha-string (-string-class [:alpha])
   :non-alpha-string (-string-class [:non-alpha])
   :numeric-string (-string-class [:numeric])
   :non-numeric-string (-string-class [:non-numeric])
   :alphanumeric-string (-string-class [:alphanumeric])
   :edn-string (fn [{:keys [constraint]} options]
                 (let [[s] (next constraint)]
                   (assert (not (false? s)))
                   [{:string-class #{(cond-> [:edn]
                                       s (conj s))}}]))
   :includes-string (fn [{:keys [constraint]} options]
                      (let [[s] (next constraint)]
                        [{:string-class #{[:includes s]}}]))

   })

;; TODO calculate compatibility with set intersection
(defn string-class->char-codes [cls]
  {:pre [(vector? cls)]}
  (case (first cls)
    :alphanumeric (concat (string-class->char-codes [:numeric])
                          (string-class->char-codes [:alpha]))
    :alpha [{:min (int \A) :max (int \Z)}
            {:min (int \a) :max (int \z)}]
    :numeric [{:min (int \0) :max (int \9)}]))

(defn negate-string-class [cls]
  (assert (vector? cls) (pr-str cls))
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
