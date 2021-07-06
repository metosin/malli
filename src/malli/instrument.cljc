(ns malli.instrument
  (:require [malli.core :as m]
            [malli.clj-kondo :as clj-kondo]
            [clojure.data :as data]))

(def ^:private instrumented* (atom nil))

(defn -find-var [n s] (find-var (symbol (str n "/" s))))

(defn -filter-all [] (constantly true))
(defn -filter-ns [& ns] (fn [n _ _] ((set ns) n)))
(defn -filter-var [f] (fn [n s _] (f (-find-var n s))))
(defn -filter-schema [f] (fn [_ _ {:keys [schema]}] (f schema)))

(defn -strument!
  ([] (-strument! nil))
  ([{:keys [mode data filters] :or {mode :instrument, data (m/function-schemas), filters [(-filter-all)]}}]
   (doseq [[n d] data, [s d] d]
     (if (some #(% n s d) filters)
       (if-let [v (-find-var n s)]
         (case mode
           :instrument (let [original-fn (or (::original-fn (meta v)) (deref v))]
                         (swap! instrumented* (fnil assoc {}) v d)
                         (alter-meta! v assoc ::original-fn original-fn)
                         (alter-var-root v (constantly (m/-instrument d original-fn)))
                         (println "..instrumented" v))
           :unstrument (when-let [original-fn (and (contains? @instrumented* v) (::original-fn (meta v)))]
                         (swap! instrumented* (fn [s] (some-> s (dissoc v) (seq) (->> (into {})))))
                         (alter-meta! v dissoc ::original-fn)
                         (alter-var-root v (constantly original-fn))
                         (println "..unstrumented" v))))))))

;;
;; public api
;;

(defn instrument!
  ([] (instrument! nil))
  ([options] (-strument! (assoc options :mode :instrument))))

(defn unstrument!
  ([] (unstrument! nil))
  ([options] (-strument! (assoc options :mode :unstrument))))

(defn stop! []
  (remove-watch @#'m/-function-schemas* ::watch)
  (unstrument!)
  (clj-kondo/emit!))

(defn start!
  ([] (start! nil))
  ([options]
   (stop!)
   (let [watch (fn [_ _ old new]
                 (instrument! (assoc options :data (second (data/diff old new))))
                 (clj-kondo/emit!))]
     (add-watch @#'m/-function-schemas* ::watch watch))
   (instrument! options)))
