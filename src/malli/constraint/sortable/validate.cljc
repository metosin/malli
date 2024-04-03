(ns malli.constraint.sortable.validate
  (:require [malli.impl.util :as miu]))

(defn validators []
  {:sorted (fn [{:keys [constraint]} _]
             (let [[v :as all] (subvec constraint 1)
                   _ (when-not (#{[] [true]} all)
                       (miu/-fail! ::sorted-constraint-takes-no-children {:constraint constraint}))]
               #(or (sorted? %)
                    (and (or (string? %) ;; TODO test string
                             (sequential? %))
                         (try (= (seq %) (sort %))
                              (catch Exception _ false))))))})
