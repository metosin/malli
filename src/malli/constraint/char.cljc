(ns malli.constraint.char)

(defn alpha? [c] #?(:clj (Character/isAlphabetic (int c))
                    :cljs (throw (ex-info (str `alpha) {}))))
(defn numeric? [c] #?(:clj (Character/isDigit (int c))
                      :cljs (throw (ex-info (str `numeric?) {}))))
(defn alphanumeric? [c] (or (alpha? c) (numeric? c)))
