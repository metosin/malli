(ns malli.constraint.sequential.humanize
  (:require [malli.impl.util :as miu]))

(defn humanizers []
  {:sorted-sequential (fn [{:keys [validator value]} _]
                        (when-not (validator value)
                          "should have sorted elements"))
   [:not :sorted-sequential] (fn [{:keys [validator value]} _]
                               (when-not (validator value)
                                 "should have elements out of order"))
   ;;FIXME create :palindrome-string, doesn't work for strings (code points)
   :palindrome-sequential (fn [{:keys [validator value]} _]
                            (when-not (validator value)
                              (if (sequential? value)
                                "should be a palindrome"
                                "should be a sequential collection")))
   [:not :palindrome-sequential] (fn [{:keys [validator value]} _]
                                   (when-not (validator value)
                                     (if (sequential? value)
                                       "should not be a palindrome"
                                       "should be a sequential collection")))})
