(ns malli.mermaid
  (:require [malli.core :as m]
            [clojure.string :as str]
            #?@(:cljs [[goog.string :as gstring]
                       [goog.string.format]])))

(defn leaf? [schema]
  (let [found (atom nil)]
    (m/accept
      schema
      (fn [schema _ _ _]
        (when (m/map-entries schema)
          (reset! found true))))
    (not @found)))

(defn prune [in] (seq (remove #{:malli.core/in} in)))

(defn collect [?schema]
  (let [in->id (atom {})]
    (m/accept
      ?schema
      (fn [schema _ in _]
        (let [{:keys [id]} (m/properties schema)
              id (if id {:id id} {:form (str (m/form schema))})]
          (when-not (leaf? schema)
            (swap! in->id update (prune in) #(or % id))))))
    @in->id))

(defn class-info [in->id in]
  (loop [h (prune in), t nil]
    (when-let [[_ {:keys [id]}] (find in->id h)]
      (if id
        (if (seq t)
          {:name (str id (->> t (map (comp str/capitalize m/keyword->string)) (str/join)))
           :base id
           :embedded true}
          {:name id})
        (if h (recur (butlast h) (conj t (last h))))))))

(defn class-diagram [?schema]
  (with-out-str
    (let [in->id (collect ?schema)
          classes (atom {})]
      (m/accept
        ?schema
        (fn [schema _ in _]
          (when (m/map-entries schema)
            (let [{:keys [name] :as info} (class-info in->id in)]
              (swap! classes update name #(-> %
                                              (merge info)
                                              (assoc :schema schema)
                                              (update :in (fnil conj #{}) in)))))))
      (println "classDiagram")
      (doseq [{:keys [name schema in embedded]} (vals @classes)]
        (println "  class" name "{")
        (when embedded
          (println "    <<embedded>>"))
        (doseq [[k _ s] (m/map-entries schema)]
          (when-let [s' (or (:name (class-info in->id (conj (first in) k))) (str (m/form s)))]
            (println (#?(:clj format, :cljs gstring/format) "    + %s %s" k s'))))
        (println "  }")
        (doseq [[k _ _] (m/map-entries schema)]
          (when-let [info (class-info in->id (conj (first in) k))]
            (println (#?(:clj format, :cljs gstring/format) "  %s %s %s" name (if (:embedded info) "*--" "o--") (:name info)))))))))

