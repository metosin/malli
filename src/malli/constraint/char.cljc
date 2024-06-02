(ns malli.constraint.char)

(defn alpha? [c] (or (<= 65 (int c) 90) (<= 97 (int c) 122)))
(defn numeric? [c] (<= 48 (int c) 57))
(defn alphanumeric? [c] (or (alpha? c) (numeric? c)))
(defn printable-ascii? [ch] (<= 32 (int ch) 126))
(defn whitespace? [ch] #?(:clj (Character/isWhitespace (int ch))
                          :cljs (throw (ex-info (str `whitespace?) {}))))
