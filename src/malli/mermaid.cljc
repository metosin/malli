(ns malli.mermaid
  (:require [malli.core :as m]
            [malli.util :as mu]
            [malli.registry :as mr]
            [clojure.string :as str]))

(defn collect [schema]
  (let [state (atom {})]
    (m/accept
      schema
      (fn [schema _ _ _]
        (let [properties (m/properties schema)]
          (doseq [[k v] (-> (m/-properties-and-options properties (m/options schema) identity) first :registry)]
            (swap! state assoc-in [:registry k] v))
          (swap! state assoc :schema schema))))
    @state))

(defn -schema-name [base in]
  (->> in (remove #{:malli.core/in}) (map (comp str/capitalize m/keyword->string)) (into [base]) (str/join "_")))

(defn normalize [{:keys [registry] :as ctx}]
  (let [registry* (atom registry)]
    (doseq [[k v] registry]
      (swap! registry* assoc k
             (m/accept v (fn [schema children in _]
                           (let [options (update (m/options schema) :registry (partial mr/composite-registry @registry*))
                                 schema (m/into-schema (m/type schema) (m/properties schema) children options)]
                             (if (and (seq in) (= :map (m/type schema)))
                               (let [ref (-schema-name k in)]
                                 (swap! registry* assoc ref (mu/update-properties schema assoc ::root k))
                                 ref)
                               schema))))))
    (assoc ctx :registry @registry*)))

(defn get-links [registry]
  (let [links (atom {})]
    (doseq [[from schema] registry]
      (m/accept
        schema
        (fn [schema _ _ _]
          (when-let [to (if (satisfies? m/RefSchema schema) (m/-ref schema))]
            (swap! links update from (fnil conj #{}) to)))))
    @links))

(defn class-diagram
  ([?schema] (class-diagram ?schema nil))
  ([?schema options]
   (let [registry (-> ?schema (m/schema options) collect normalize :registry)
         links (get-links registry)]
     (with-out-str
       (println "classDiagram")
       (doseq [[k v] registry]
         (println "  class" k "{")
         (doseq [[k _ s] (m/map-entries v)]
           (println "   " k s))
         (println "  }"))
       (doseq [[from tos] links
               to tos]
         (let [root (::root (m/properties (get registry to)))]
           (println " " from (if root "*--" "o--") to)))))))
