(ns malli.constraint.compound.humanize
  (:require [malli.error.utils :refer [-flatten-errors]]))

(defn humanizers []
  {;;TODO [:not :iff]
   :iff (fn [{:keys [constraint constraint-validator humanize-constraint-violation
                     value]}
             _]
          (let [results (map #(do [((constraint-validator %) value) %])
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
                                      results))])))})
