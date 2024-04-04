(ns malli.error.utils)

(defn -flatten-errors [errors]
  (when-not (= [:and] errors)
    (if (= [:or] errors)
      "unsolvable"
      (if (and (vector? errors)
               (keyword? (first errors))
               (= 2 (count errors)))
        (second errors)
        errors))))
