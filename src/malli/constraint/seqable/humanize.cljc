(ns malli.constraint.seqable.humanize
  (:require [malli.impl.util :as miu]))

(defn humanizers []
  {:distinct-seqable (fn [{:keys [validator value]} _]
                       (let [value (sequence value)]
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
                                                     freq))))))))})
