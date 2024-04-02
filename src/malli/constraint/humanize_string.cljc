(ns malli.constraint.humanize-string
  (:require [malli.constraint.char :as mcc]))

(def humanizers
  {:alphanumeric-string (fn [{:keys [value]} _options]
                          (keep-indexed (fn [i v]
                                          (when-not (mcc/alphanumeric? v)
                                            (str "should be alphanumeric: "
                                                 "index " i " has " (pr-str v) ".")))
                                        value))
   [:not :alphanumeric-string] (fn [{:keys [validator value ]} _options]
                                 (when-not (validator value)
                                   (str "should contain a non-alphanumeric character")))})
