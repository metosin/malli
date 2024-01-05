(ns malli.edn
  (:refer-clojure :exclude [read-string])
  (:require [edamame.core :as edamame]
            [malli.core :as m]))

(defn -parse-string [x]
  (edamame/parse-string x {:var resolve, :regex true, :fn true}))

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
