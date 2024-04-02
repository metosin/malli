(ns malli.constraint.humanize-string
  (:require [malli.constraint.char :as mcc]))

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

(def humanizers
  {:alphanumeric-string (-msg-check-each "should be alphanumeric" mcc/alphanumeric?)
   [:not :alphanumeric-string] (-msg-or-validates "should contain a non-alphanumeric character")
   :non-alphanumeric-string (-msg-check-each "should not contain alphanumeric characters" (complement mcc/alphanumeric?))
   [:not :non-alphanumeric-string] (-msg-or-validates "should contain an alphanumeric character")})
