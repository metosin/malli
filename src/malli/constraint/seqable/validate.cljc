(ns malli.constraint.seqable.validate
  (:require [malli.impl.util :as miu]))

(defn validators []
  {:distinct-seqable (fn [{:keys [constraint]} _]
                       (let [[v :as all] (subvec constraint 1)
                             _ (when-not (#{[] [true]} all)
                                 (miu/-fail! ::distinct-constraint-takes-no-children {:constraint constraint}))]
                         #(if-some [s (seq %)]
                            (apply distinct? s)
                            true)))})
