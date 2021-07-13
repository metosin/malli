(ns malli.instrument
  (:require [malli.core :as m]
            [malli.clj-kondo :as clj-kondo]))

(defn -find-var [n s] (find-var (symbol (str n "/" s))))

(defn -filter-all [] (constantly true))
(defn -filter-ns [& ns] (fn [n _ _] ((set ns) n)))
(defn -filter-var [f] (fn [n s _] (f (-find-var n s))))
(defn -filter-schema [f] (fn [_ _ {:keys [schema]}] (f schema)))

(defn -strument!
  ([] (-strument! nil))
  ([{:keys [mode data filters gen] :or {mode :instrument, data (m/function-schemas)} :as options}]
   (doseq [[n d] data, [s d] d]
     (if (or (not filters) (some #(% n s d) filters))
       (if-let [v (-find-var n s)]
         (case mode
           :instrument (let [original-fn (or (::original-fn (meta v)) (deref v))
                             dgen (as-> (merge (select-keys options [:scope :report :gen]) d) $
                                        (cond (and gen (true? (:gen d))) (assoc $ :gen gen)
                                              (true? (:gen d)) (dissoc $ :gen)
                                              :else $))]
                         (alter-meta! v assoc ::original-fn original-fn)
                         (alter-var-root v (constantly (m/-instrument dgen original-fn)))
                         (println "..instrumented" v))
           :unstrument (when-let [original-fn (::original-fn (meta v))]
                         (alter-meta! v dissoc ::original-fn)
                         (alter-var-root v (constantly original-fn))
                         (println "..unstrumented" v))))))))

;;
;; public api
;;

(defn collect!
  ([] (collect! {:ns *ns*}))
  ([{:keys [ns]}]
   (not-empty
     (reduce
       (fn [acc v]
         (let [{:keys [ns name malli/schema] :as meta} (meta v)
               v' (when schema (m/-register-function-schema! (-> ns str symbol) name (m/-unlift-keys meta "malli")))]
           (cond-> acc v' (conj v)))) #{} (vals (ns-publics ns))))))

(defn instrument!
  ([] (instrument! nil))
  ([options] (-strument! (assoc options :mode :instrument))))

(defn unstrument!
  ([] (unstrument! nil))
  ([options] (-strument! (assoc options :mode :unstrument))))

(defn stop! []
  (remove-watch @#'m/-function-schemas* ::watch)
  (unstrument!)
  (clj-kondo/emit!)
  (println "stopped instrumentation"))

(defn start!
  ([] (start! nil))
  ([options]
   (with-out-str (stop!))
   (let [watch (fn [_ _ old new]
                 (instrument! (assoc options :data (->> (for [[n d] new
                                                              :let [no (get old n)]
                                                              [s d] d
                                                              :when (not= d (get no s))]
                                                          [[n s] d])
                                                        (into {})
                                                        (reduce-kv assoc-in {}))))
                 (clj-kondo/emit!))]
     (add-watch @#'m/-function-schemas* ::watch watch))
   (instrument! options)
   (clj-kondo/emit!)
   (println "started instrumentation")))
