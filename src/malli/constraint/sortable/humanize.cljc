(ns malli.constraint.sortable.humanize)

(defn humanizers []
  {:sorted (fn [{:keys [validator value]} _]
             (when-not (validator value)
               (if (map? value)
                 "should be a sorted map"
                 (if (set? value)
                   "should be a sorted set"
                   (if-not (or (string? value)
                               (sequential? value))
                     "should be sortable"
                     (let [sv (delay (sort value))]
                       (or (try @sv
                                nil
                                (catch #?(:clj Exception, :cljs js/Error) _
                                  "should be sorted but elements are not comparable"))
                           (let [[i v s] (some identity
                                               (map (fn [i v s]
                                                      (when (not= v s)
                                                        [i v s]))
                                                    (range) value @sv))]
                             (str "should be sorted: index "
                                  i " has " (pr-str v) " but"
                                  " expected " (pr-str s))))))))))})
