(ns malli.plantuml
  (:require [malli.core :as m]
            [malli.dot :as md]
            [clojure.string :as str]))

(defn transform
  ([?schema] (transform ?schema nil))
  ([?schema options]
   (let [registry (-> ?schema (m/schema options) md/-lift md/-collect md/-normalize :registry)
         entity? #(->> % (get registry) m/properties ::md/entity not)
         sorted #(sort-by (m/-comp str first) %)]
     (with-out-str
       (println "@startuml")
       (doseq [[k v] (sorted registry)]
         (println (if (entity? k) "entity" "abstract") k "{\n"
                  (or (some->> (m/entries v) (map (fn [[k s]] (str (pr-str k) " " (pr-str (m/form (m/deref s)))))) (str/join "\n "))
                      (pr-str (m/form v))))
         (println "}"))
       (doseq [[from tos] (sorted (md/-get-links registry)), to tos]
         (println from (if (entity? to) "o--" "*--") to))
       (println "@enduml")))))
