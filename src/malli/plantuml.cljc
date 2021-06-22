(ns malli.plantuml
  (:require [malli.core :as m]
            [malli.dot :as md]
            [clojure.string :as str]))

(defn transform
  ([?schema] (transform ?schema nil))
  ([?schema options]
   (let [registry (-> ?schema (m/schema options) md/-lift md/-collect md/-normalize :registry)
         entity? #(->> % (get registry) m/properties ::md/entity not)
         esc #(str/escape (str %) {\> "\\>", \{ "\\{", \} "\\}", \< "\\<", \" "\\\""})
         sorted #(sort-by (m/-comp str first) %)]
     (with-out-str
       (println "@startuml")
       (doseq [[k v] (sorted registry)]
         (println (if (entity? k) "entity" "abstract") k "{\n"
            (or (some->> (m/entries v) (map (fn [[k s]] (str k " " (m/form (m/deref s))))) (str/join "\n "))
                (esc (m/form v))))
         (println "}\n"))
       (doseq [[from tos] (sorted (md/-get-links registry)), to tos]
         (println from (if (entity? to) "<|--" "*--") to))
       (println "@enduml")))))
