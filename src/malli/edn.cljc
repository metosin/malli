(ns malli.edn
  (:refer-clojure :exclude [read-string])
  (:require [malli.core :as m]
            [edamame.core :as edamame]))

(defn write-string
  ([?schema]
   (write-string ?schema nil))
  ([?schema opts]
   (pr-str (m/form ?schema opts))))

(defn read-string
  ([form]
   (read-string form nil))
  ([form opts]
   (m/schema (edamame/parse-string form {:dispatch {\# {\" #(re-pattern %)}}}) opts)))
