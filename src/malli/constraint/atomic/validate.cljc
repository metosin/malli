(ns malli.constraint.atomic.validate
  (:require [malli.impl.util :as miu]))

(defn validators []
  {:any (fn [{:keys [constraint]} _]
          (let [[v :as all] (subvec constraint 1)
                _ (when-not (#{[] [true]} all)
                    (miu/-fail! ::any-constraint-takes-no-children {:constraint constraint}))]
            any?))})
