(ns malli.constraint.keyset.humanize
  )

(defn humanizers []
  {;;TODO [:not :disjoint]
   :disjoint (fn [{:keys [constraint constraint-validator humanize-constraint-violation
                          value]}
                  _]
               (let [ksets (subvec constraint 1)
                     has? #(contains? value %)
                     [has-constraint has-k] (some (fn [i]
                                                    (when-some [[has-k] (not-empty
                                                                          (filter has? (nth ksets i)))]
                                                      [i has-k]))
                                                  (range (count ksets)))
                     violating-ks (filterv has?
                                           (apply concat (subvec ksets (inc has-constraint))))]
                 (str "should not combine key " (pr-str has-k)
                      " with key" (if (next violating-ks) "s" "") ": "
                      (apply str (interpose " " (map pr-str violating-ks))))))
   :contains (fn [{:keys [constraint validator value]} _]
               (when-not (validator value)
                 (str "should provide key: " (pr-str (nth constraint 1)))))
   [:not :contains] (fn [{:keys [constraint validator value]} _]
                      (when-not (validator value)
                        (str "should not provide key: " (pr-str (nth constraint 1)))))
   })
