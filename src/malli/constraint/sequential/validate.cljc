(ns malli.constraint.sequential.validate
  (:require [malli.impl.util :as miu]))

(defn validators []
  {:sorted-sequential (fn [{:keys [constraint]} _]
                        (let [[v :as all] (subvec constraint 1)
                              _ (when-not (#{[] [true]} all)
                                  (miu/-fail! ::sorted-sequential-constraint-takes-no-children {:constraint constraint}))]
                          #(try (let [s (sequence %)]
                                  (= s (sort s)))
                                (catch #?(:clj Exception, :cljs js/Error) _ false))))
   :palindrome-sequential (fn [{:keys [constraint]} _]
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
