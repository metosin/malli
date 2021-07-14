(ns malli.dev
  (:require [malli.core :as m]
            [malli.instrument :as mi]
            [malli.clj-kondo :as clj-kondo]))

(defn stop!
  "Stops instrumentation for all functions vars and removes clj-kondo type annotations."
  []
  (remove-watch @#'m/-function-schemas* ::watch)
  (mi/unstrument!)
  (clj-kondo/emit!)
  (println "stopped instrumentation"))

(defn start!
  "Starts instrumentation for a filtered set of function Vars (e.g. `defn`s).
   See [[malli.core/-instrument]] for possible options. Re-instruments if the
   function schemas change. Also emits clj-kondo type annotations."
  ([] (start! nil))
  ([options]
   (with-out-str (stop!))
   (let [watch (fn [_ _ old new]
                 (mi/instrument! (assoc options :data (->> (for [[n d] new
                                                                 :let [no (get old n)]
                                                                 [s d] d
                                                                 :when (not= d (get no s))]
                                                             [[n s] d])
                                                           (into {})
                                                           (reduce-kv assoc-in {}))))
                 (clj-kondo/emit!))]
     (add-watch @#'m/-function-schemas* ::watch watch))
   (mi/instrument! options)
   (clj-kondo/emit!)
   (println "started instrumentation")))
