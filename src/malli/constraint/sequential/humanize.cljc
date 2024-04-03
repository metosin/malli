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
                            "should be a sequential collection")))
   :distinct (fn [{:keys [validator value]} _]
               (when-not (validator value)
                 (let [freq (into {} (remove (comp #(= 1 %) val))
                                  (frequencies value))]
                   (str "should be distinct: "
                        (apply str
                               (interpose
                                 ", " (map (fn [[v i]]
                                             (str (pr-str v)
                                                  " provided "
                                                  i " times"))
                                           freq)))))))})
