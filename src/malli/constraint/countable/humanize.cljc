(ns malli.constraint.countable.humanize
  )

(defn humanizers []
  {[:not :min-count] (fn foo [{:keys [constraint value]} _]
                       (let [cnt (count value)
                             min (nth constraint 1)]
                         (when (<= min cnt)
                           (str "should be less than " min
                                (if (string? value)
                                  " character"
                                  " element")
                                (when-not (= 1 min) "s")
                                ", given " cnt))))})
