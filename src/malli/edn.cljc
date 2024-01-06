(ns malli.edn
  (:refer-clojure :exclude [read-string])
  (:require [edamame.core :as edamame]
            [malli.core :as m]))

(defn -var-symbol [s] (symbol (str "#'" s)))
(defn -fail! [s] (fn [v] (m/-fail! ::var-parsing-not-supported {:var (-var-symbol v), :string s})))

(defn -parse-string
  ([x] (-parse-string x nil))
  ([x options] (edamame/parse-string x (or options {:regex true, :fn true, :var (-fail! x)}))))

(defn write-string
  ([?schema]
   (write-string ?schema nil))
  ([?schema options]
   (pr-str (m/form ?schema options))))

(defn read-string
  ([form]
   (read-string form nil))
  ([form {::keys [edamame-options] :as options}]
   (m/schema (-parse-string form edamame-options) options)))
