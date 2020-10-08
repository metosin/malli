(ns malli.graalvm.demosci
  (:gen-class)
  (:require [sci.core]
            [malli.core :as m]
            [malli.edn :as edn]
            [malli.error :as me]
            [malli.transform :as mt]))

(defn -main [& [?schema ?value]]
  (println "  ?schema:" (pr-str ?schema))
  (println "   ?value:" (pr-str ?value))
  (let [schema (edn/read-string ?schema)
        value (edn/-parse-string ?value)
        _ (println "   schema:" (pr-str schema))
        _ (println "    value:" (pr-str value))
        decoded (m/decode schema value (mt/string-transformer))
        valid (m/validate schema decoded)]
    (println "  decoded:" (pr-str decoded))
    (println "    valid:" (m/validate schema decoded))
    (when-not valid
      (let [explain (m/explain schema decoded)]
        (println)
        (println "  explain:" (pr-str explain))
        (println)
        (println "humanized:" (pr-str (me/humanize explain)))))))