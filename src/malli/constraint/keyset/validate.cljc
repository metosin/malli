(ns malli.constraint.keyset.validate
  (:require [malli.impl.util :as miu]))

(defn validators []
  {:contains (fn [{:keys [constraint]} _]
               (let [[k :as all] (subvec constraint 1)
                     _ (when-not (= 1 (count all))
                         (miu/-fail! ::contains-constraint-takes-one-child {:constraint constraint}))]
                 #(contains? % k)))})
