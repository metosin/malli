(ns malli.constraint.string.validate
  (:require [clojure.string :as str]
            [malli.constraint.char :as char]
            [malli.impl.util :as miu]))

(defn -wrap [f] (fn [_ _] f))
(defn -idempotent [f] (-wrap (fn [s] (= s (f s)))))

(defn validators []
  {:alpha-string (-wrap (fn [s] (every? char/alpha? s)))
   :non-alpha-string (-wrap (fn [s] (not-any? char/alpha? s)))
   :numeric-string (-wrap (fn [s] (every? char/numeric? s)))
   :non-numeric-string (-wrap (fn [s] (not-any? char/numeric? s)))
   :alphanumeric-string (-wrap (fn [s] (every? char/alphanumeric? s)))
   :non-alphanumeric-string (-wrap (fn [s] (not-any? char/alphanumeric? s)))
   :trim-string (-idempotent str/trim)
   :triml-string (-idempotent str/triml)
   :trimr-string (-idempotent str/trimr)
   :trim-newline-string (-idempotent str/trim-newline)
   :blank-string (-wrap str/blank?)
   :non-blank-string (-wrap (complement str/blank?))
   :escapes-string (fn [{:keys [constraint]} _]
                     (when-not (= 2 (count constraint))
                       (miu/-fail! ::escapes-constraint-takes-one-child {:constraint constraint}))
                     (let [m (not-empty (nth constraint 1))
                           _ (when-not (map? m)
                               (miu/-fail! ::escapes-constraint-takes-non-empty-map-child {:constraint constraint}))
                           _ (run! (fn [s]
                                     (or (if (string? s)
                                           (not-any? #(contains? m %) s)
                                           (not (contains? m s)))
                                         (miu/-fail! ::escape-constraint-map-cannot-overlap-keys-vals)))
                                   (vals m))
                           not-allowed (into #{} (map (fn [c]
                                                        (when-not (char? c)
                                                          (miu/-fail! ::escapes-constraint-map-takes-characters
                                                                      {:constraint constraint}))
                                                        c))
                                             (keys m))]
                       (fn [s]
                         (not-any? not-allowed s))))})
