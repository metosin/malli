(ns malli.constraint.compound.humanize
  (:require [clojure.math.combinatorics :as comb]
            [malli.error.utils :refer [-flatten-errors]]))

(defn -de-morgan []
  (fn [{:keys [constraint humanize-constraint-violation]} _]
    (let [[and-or & rands] constraint]
      (humanize-constraint-violation
        (into [({:and :or :or :and} and-or)]
              (map #(vector :not %))
              rands)))))

(defn humanizers []
  {;;TODO [:not :iff] == every combination of success/fail of clauses except all fail and all succeed
   :iff (fn [{:keys [constraint constraint-validator humanize-constraint-violation
                     value]} _]
          (let [results (map #(do [((constraint-validator %) value)
                                   %])
                             (next constraint))]
            (when-not (apply = (map first results))
              [:xor
               (-flatten-errors (into [:and] (keep (fn [[valid? constraint]]
                                                     (when-not valid?
                                                       (humanize-constraint-violation constraint))))
                                      results))
               (-flatten-errors (into [:and] (keep (fn [[valid? constraint]]
                                                     (when valid?
                                                       (humanize-constraint-violation [:not constraint]))))
                                      results))])))
   :implies (fn [{:keys [constraint constraint-validator humanize-constraint-violation
                         value]} _]
              (let [[test-result & results] (map #(do [((constraint-validator %) value)
                                                       %])
                                                 (next constraint))]
                (when (first test-result)
                  (when-not (apply = true (map first results))
                    (mapcat (fn [[valid? constraint]]
                              (when-not valid?
                                (let [errors (humanize-constraint-violation constraint)]
                                  (if (string? errors)
                                    [errors]
                                    errors))))
                            results)))))
   [:not :or] (-de-morgan)
   [:not :and] (-de-morgan)

   :or (fn [{:keys [constraint constraint-validator
                    humanize-constraint-violation value]} _]
         (let [results (map #(do [((constraint-validator %) value)
                                  %])
                            (next constraint))]
           (when (not-any? first results)
             (-flatten-errors
               (into [:or] (comp (remove first)
                                 (keep (comp humanize-constraint-violation second)))
                     results)))))
   :and (fn [{:keys [constraint constraint-validator
                     humanize-constraint-violation value]} _]
          (-flatten-errors
            (into [:and] (keep (fn [constraint]
                                 (let [validator (constraint-validator constraint)]
                                   (when-not (validator value)
                                     (humanize-constraint-violation constraint)))))
                  (next constraint))))

   :xor (fn [{:keys [constraint constraint-validator
                     humanize-constraint-violation value]} _]
          (let [rands (subvec constraint 1)
                results (map #(do [((constraint-validator %) value)
                                   %])
                             rands)
                succeed (filter first results)
                nsucceed (count succeed)]
            (when-not (= 1 nsucceed)
              (if (zero? nsucceed)
                (-flatten-errors
                  (into [:xor] (keep humanize-constraint-violation)
                        rands))
                (-flatten-errors
                  (into [:xor] (keep #(humanize-constraint-violation
                                        (into [:and]
                                              (map (fn [c] [:not c]))
                                              %)))
                        (comb/combinations (map second succeed) (dec nsucceed)))))))) })
