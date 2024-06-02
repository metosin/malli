(ns malli.constraint.countable.humanize
  )

(defn humanizers []
  {:max-count (fn [{:keys [constraint value]} _]
                (let [cnt (count value)
                      max (nth constraint 1)]
                  (when-not (<= cnt max)
                    (str "should be at most " max
                         (if (string? value)
                           " character"
                           " element")
                         (when-not (= 1 max) "s")
                         ", given " cnt))))
   [:not :max-count] (fn [{:keys [constraint value]} _]
                       (let [cnt (count value)
                             max (nth constraint 1)]
                         (when (<= cnt max)
                           (str "should be more than " max
                                (if (string? value)
                                  " character"
                                  " element")
                                (when-not (= 1 max) "s")
                                ", given " cnt))))

   :min-count (fn [{:keys [constraint value]} _]
                (let [cnt (count value)
                      min (nth constraint 1)]
                  (when-not (<= min cnt)
                    (str "should be at least " min
                         (if (string? value)
                           " character"
                           " element")
                         (when-not (= 1 min) "s")
                         ", given " cnt))))
   [:not :min-count] (fn [{:keys [constraint value]} _]
                       (let [cnt (count value)
                             min (nth constraint 1)]
                         (when (<= min cnt)
                           (str "should be less than " min
                                (if (string? value)
                                  " character"
                                  " element")
                                (when-not (= 1 min) "s")
                                ", given " cnt))))})
