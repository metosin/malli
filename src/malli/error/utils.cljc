(ns malli.error.utils)

(defn -flatten-errors [errors]
  (when-not (= [:and] errors)
    (if (= [:or] errors)
      "unsolvable"
      (if-some [op (when (and (vector? errors)
                              (keyword? (first errors)))
                     (first errors))]
        (if (= 2 (count errors))
          (second errors)
          (into [op] (mapcat (fn [errors]
                               (if (and (vector? errors)
                                        (= op (first errors)))
                                 (next errors)
                                 [errors])))
                (next errors)))
        errors))))
