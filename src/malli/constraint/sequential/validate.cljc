(ns malli.constraint.sequential.validate
  (:require [malli.impl.util :as miu]))

(defn validators []
  {:palindrome (fn [{:keys [constraint]} _]
                 (let [[v :as all] (subvec constraint 1)
                       _ (when-not (#{[] [true]} all)
                           (miu/-fail! ::palindrome-constraint-takes-no-children {:constraint constraint}))]
                   #(= (sequence %)
                       (if (reversible? %)
                         (-> % rseq sequence)
                         (reverse %)))))
   :distinct (fn [{:keys [constraint]} _]
               (let [[v :as all] (subvec constraint 1)
                     _ (when-not (#{[] [true]} all)
                         (miu/-fail! ::distinct-constraint-takes-no-children {:constraint constraint}))]
                 #(or (empty? %) (apply distinct? %))))})
