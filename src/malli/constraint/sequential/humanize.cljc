(ns malli.constraint.sequential.humanize
  (:require [malli.impl.util :as miu]))

(defn humanizers []
  {:palindrome (fn [{:keys [validator value]} _]
                 (when-not (validator value)
                   (if (or (string? value)
                           (sequential? value))
                     "should be a palindrome"
                     "should be a sequential collection")))
   [:not :palindrome] (fn [{:keys [validator value]} _]
                        (when-not (validator value)
                          (if (or (string? value)
                                  (sequential? value))
                            "should not be a palindrome"
                            "should be a sequential collection")))})
