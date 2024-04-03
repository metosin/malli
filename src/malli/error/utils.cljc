(ns malli.error.utils)

(defn -flatten-errors [errors]
  (if (and (vector? errors)
           (keyword? (first errors))
           (= 2 (count errors)))
    (second errors)
    errors))
