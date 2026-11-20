(ns malli.dot
  (:require [clojure.string :as str]
            [malli.core :as m]
            [malli.registry :as mr]
            [malli.util :as mu]))

(defn -ref-name
  "name for a reference in the output: vars as their symbol name, as #'... is not a valid PlantUML name"
  [ref]
  (if (var? ref) (subs (str ref) 2) ref))

(defn -lift [?schema]
  (let [schema (m/schema ?schema)]
    (cond
      ;; a named root, e.g. "Curve" from the :registry option, is its own entity
      (and (#{::m/schema :ref} (m/type schema)) (m/-ref schema))
      (let [id (-ref-name (m/-ref schema))] [:schema {:registry {id (m/deref schema)}} id])
      (and (m/-ref-schema? schema) (-> schema m/deref m/type (= ::m/schema))) ?schema
      :else [:schema {:registry {::schema ?schema}} ?schema])))

(defn -collect [schema]
  (let [state (atom {})]
    (m/walk
     schema
     (fn [schema _ _ _]
       (let [properties (m/properties schema)]
         (doseq [[k v] (-> (m/-properties-and-options properties (m/options schema) identity) first :registry)]
           (swap! state assoc-in [:registry k] v))
         (swap! state assoc :schema schema))))
    @state))

(defn -schema-name [base path]
  (->> path (remove #{:malli.core/in}) (map (m/-comp str/capitalize m/-keyword->string)) (into [base]) (str/join "$")))

(defn -normalize [{:keys [registry] :as ctx}]
  (let [registry* (atom registry)
        ;; original forms of the referable schemas, to tell apart different schemas with the same name
        forms (atom (reduce-kv (fn [acc k v] (assoc acc k (m/form v))) {} registry))
        added (atom {})
        ;; name under which the schema `target`, referenced as `ref` from `k`, is in the registry.
        ;; schemas that are not in the registry yet (e.g. from the :registry option) are added. If the
        ;; name is taken by a different schema (e.g. a local registry entry in another scope), uses a new name.
        ->name (fn [k ref target]
                 (let [form (m/form target)]
                   (loop [name (-ref-name ref), i 0]
                     (let [f (get @forms name ::none)]
                       (cond
                         (= f form) name
                         (and (= f ::none) (not (contains? @registry* name)))
                         (do (swap! forms assoc name form)
                             (swap! registry* assoc name target)
                             (swap! added assoc name target)
                             name)
                         :else (recur (str (-schema-name k []) "$" (m/-keyword->string (-ref-name ref)) (when (pos? i) i)) (inc i)))))))]
    (loop [todo registry]
      (when (seq todo)
        (reset! added {})
        (doseq [[k v] todo]
          (swap! registry* assoc k
                 (m/walk v (fn [schema path children _]
                             (let [ref (when (m/-ref-schema? schema) (m/-ref schema))
                                   name (when ref (->name k ref (m/deref schema)))
                                   options (update (m/options schema) :registry #(mr/composite-registry @registry* %))
                                   schema (cond
                                            (= ref name) (m/into-schema (m/parent schema) (m/properties schema) children options)
                                            (= :ref (m/type schema)) (m/into-schema (m/parent schema) (m/properties schema) [name] options)
                                            :else (m/schema name options))]
                               (if (and (seq path) (= :map (m/type schema)))
                                 (let [ref (-schema-name k path)]
                                   (swap! registry* assoc ref (mu/update-properties schema assoc ::entity k))
                                   ref)
                                 schema))))))
        (recur @added)))
    (assoc ctx :registry @registry*)))

(defn -get-links [registry]
  (let [links (atom {})]
    (doseq [[from schema] registry]
      (m/walk
       schema
       (fn [schema _ _ _]
         (when-let [to (when (m/-ref-schema? schema) (m/-ref schema))]
           (swap! links update from (fnil conj #{}) to)))))
    @links))

;;
;; public api
;;

(defn transform
  ([?schema] (transform ?schema nil))
  ([?schema options]
   (let [registry (-> ?schema (m/schema options) -lift -collect -normalize :registry)
         entity? #(->> % (get registry) m/properties ::entity)
         props #(str "[" (str/join ", " (map (fn [[k v]] (str (name k) "=" (if (fn? v) (v) (pr-str v)))) %)) "]")
         esc #(str/escape (str %) {\> "\\>", \{ "\\{", \} "\\}", \< "\\<", \" "\\\""})
         sorted #(sort-by (m/-comp str first) %)
         wrap #(str "\"" % "\"")
         label (fn [k v] (str "\"{" k "|"
                              (or (some->> (m/entries v) (map (fn [[k s]] (str k " " (esc (m/form (m/deref s)))))) (str/join "\\l"))
                                  (esc (m/form v)))
                              "\\l}\""))
         > #(apply println %&)
         >> #(apply > " " %&)]
     (with-out-str
       (> "digraph {")
       (>> "node" (props {:shape "record", :style "filled", :color "#000000"}))
       (>> "edge" (props {:dir "back", :arrowtail "none"}))
       (>>)
       (doseq [[k v] (sorted registry)]
         (>> (wrap k) (props {:label #(label k v), :fillcolor (if (entity? k) "#e6caab" "#fff0cd")})))
       (>>)
       (doseq [[from tos] (sorted (-get-links registry)), to tos]
         (>> (wrap from) "->" (wrap to) (props {:arrowtail (if (entity? to) "diamond" "odiamond")})))
       (> "}")))))
