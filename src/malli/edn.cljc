(ns malli.edn
  (:refer-clojure :exclude [read-string])
  (:require [malli.core :as m]
            [edamame.core :as edamame]))

(defn -parse-string [x]
  (edamame/parse-string x {:dispatch {\# {\" #(re-pattern %)}}}))

(defn write-string
  ([?schema]
   (write-string ?schema nil))
  ([?schema options]
   (pr-str (m/form ?schema options))))

(defn read-string
  ([form]
   (read-string form nil))
  ([form options]
   (m/schema (-parse-string form) options)))
