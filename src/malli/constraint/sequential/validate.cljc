(ns malli.constraint.sequential.validate
  (:require [malli.impl.util :as miu]))

(defn validators []
  {:palindrome (fn [{:keys [constraint]} _]
                 (let [[v :as all] (subvec constraint 1)
                       _ (when-not (#{[] [true]} all)
                           (miu/-fail! ::palindrome-constraint-takes-no-children {:constraint constraint}))]
                   #(do (when (string? %)
                          (assert (= (count %) (.codePointCount ^String % 0 (count %)))
                                  "TODO palindrome with surrogate pairs"))
                        (= (sequence %)
                           (if (reversible? %)
                             (-> % rseq sequence)
                             (reverse %))))))})
