(ns malli.dev.cljs
  (:require
    [malli.clj-kondo :as clj-kondo]
    [malli.instrument.cljs :as mi]))

(defmacro stop!
  "Stops instrumentation for all functions vars and removes clj-kondo type annotations."
  []
  `(do
     ~(mi/-unstrument nil)
     ~(do (clj-kondo/save! {}) nil)))

(defmacro start!
  "Collects defn schemas from all loaded namespaces and starts instrumentation for
   a filtered set of function Vars (e.g. `defn`s). See [[malli.core/-instrument]] for possible options.
   Also emits clj-kondo type annotations.
   This does NOT re-instrument functions if the function schemas change - use hot reloading to get a similar effect."
  [options]
  `(do
     ~(mi/-unstrument nil)
     ~(do (clj-kondo/save! {}) nil)
     ;; malli.dev/stop ^^

     ;; register all function schemas and instrument them based on the options
     ~(mi/-collect-all-ns)
     ~(mi/-instrument options)
     ~(do (clj-kondo/emit!) nil)))
