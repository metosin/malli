(ns malli.constraint.compound.humanize
  (:require [malli.error.utils :refer [-flatten-errors]]))

(defn -de-morgan []
  (fn [{:keys [constraint humanize-constraint-violation]} _]
    (let [[and-or & rands] constraint]
      (humanize-constraint-violation
        (into [({:and :or :or :and} and-or)]
              (map #(vector :not %))
              rands)))))

(defn humanizers []
  {;;TODO [:not :iff]
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
   })
