(ns malli.constraint.string.humanize
  (:require [clojure.string :as str]
            [malli.constraint.char :as mcc]))

(defn -msg-or-pred [msg pred]
  (fn [{:keys [value]} _options]
    (when-not (pred value)
      msg)))

(defn -msg-or-validates [msg]
  (fn [{:keys [validator value]} _options]
    (when-not (validator value)
      msg)))

(defn -msg-check-each [msg f]
  (fn [{:keys [value]} _options]
    (keep-indexed (fn [i v]
                    (when-not (f v)
                      (str msg ": "
                           "index " i " has " (pr-str v) ".")))
                  value)))

(defn humanizers []
  {:alphanumeric-string (-msg-check-each "should be alphanumeric" mcc/alphanumeric?)
   [:not :alphanumeric-string] (-msg-or-validates "should contain a non-alphanumeric character")
   :non-alphanumeric-string (-msg-check-each "should not contain alphanumeric characters" (complement mcc/alphanumeric?))
   [:not :non-alphanumeric-string] (-msg-or-validates "should contain an alphanumeric character")

   :alpha-string (-msg-check-each "should be alphabetic" mcc/alpha?)
   [:not :alpha-string] (-msg-or-validates "should contain a non-alphabetic character")
   :non-alpha-string (-msg-check-each "should not contain alphabetic characters" (complement mcc/alpha?))
   [:not :non-alpha-string] (-msg-or-validates "should contain an alphabetic character")

   :numeric-string (-msg-check-each "should be numeric" mcc/numeric?)
   [:not :numeric-string] (-msg-or-validates "should contain a non-numeric character")
   :non-numeric-string (-msg-check-each "should not contain numeric characters" (complement mcc/numeric?))
   [:not :non-numeric-string] (-msg-or-validates "should contain a numeric character")
   
   :trim-string (fn [{:keys [value]} _options]
                  (cond-> []
                    (not= value (str/triml value)) (conj "should not have leading whitespace")
                    (not= value (str/trimr value)) (conj "should not have trailing whitespace")))
   [:not :trim-string] (-msg-or-pred "should have leading or trailing whitespace" #(not= % (str/trim %)))

   :triml-string (-msg-or-validates "should not have leading whitespace")
   [:not :triml-string] (-msg-or-validates "should have leading whitespace")
   :trimr-string (-msg-or-validates "should not have trailing whitespace")
   [:not :trimr-string] (-msg-or-validates "should have trailing whitespace")
   :trim-newline-string (-msg-or-validates "should not have trailing newline")
   [:not :trim-newline-string] (-msg-or-validates "should have trailing newline")
   :blank-string (-msg-or-validates "should be blank")
   [:not :blank-string] (-msg-or-validates "should not be blank")
   :non-blank-string (-msg-or-validates "should not be blank")
   [:not :non-blank-string] (-msg-or-validates "should be blank")
   })
