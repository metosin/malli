(ns malli.constraint.number.validate
  (:require [malli.impl.util :as miu]))

(defn- -first-child [f]
  (fn [{:keys [constraint]} _]
    (let [[n :as all] (subvec constraint 1)
          _ (when-not (= 1 (count all))
              (miu/-fail! ::numeric-constraint-takes-one-child {:constraint constraint}))
          _ (when-not (number? n)
              (miu/-fail! ::numeric-constraint-takes-integer {:constraint constraint}))]
      (f n))))

(defn validators []
  ;; aligns with existing :< :> schema semantics
  {:<  (-first-child (fn [n] #(<  % n)))
   :<= (-first-child (fn [n] #(<= % n)))
   :>  (-first-child (fn [n] #(>  % n)))
   :>= (-first-child (fn [n] #(>= % n)))})
