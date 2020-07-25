(ns malli.edn
  (:refer-clojure :exclude [read-string])
  (:require [malli.core :as m]
            [edamame.core :as edamame]))

(defn write-string
  ([?schema]
   (write-string ?schema (m/default-options)))
  ([?schema options]
   (pr-str (m/form ?schema options))))

(defn read-string
  ([form]
   (read-string form (m/default-options)))
  ([form options]
   (m/schema (edamame/parse-string form {:dispatch {\# {\" #(re-pattern %)}}}) options)))
