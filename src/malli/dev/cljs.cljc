(ns malli.dev.cljs
  #?(:cljs (:require-macros [malli.dev.cljs]))
  #?(:cljs (:require [malli.core :as m]
                     [malli.dev.pretty :as pretty]
                     [malli.instrument]))
  #?(:clj (:require [cljs.analyzer.api :as ana-api]
                    [malli.clj-kondo :as clj-kondo]
                    [malli.core :as m]
                    [malli.instrument])))

#?(:clj (defmacro stop!
          "Stops instrumentation for all functions vars and removes clj-kondo type annotations."
          []
          `(do (malli.instrument/unstrument! nil)
               ~(do (clj-kondo/save! {}) nil))))

#?(:clj (defmacro collect-all! [] (malli.instrument/collect! {:ns (ana-api/all-ns)})))

#?(:clj
   (defmacro start!
     "Collects defn schemas from all loaded namespaces and starts instrumentation for
      a filtered set of function Vars (e.g. `defn`s). See [[malli.core/-instrument]] for possible options.
      Differences from Clojure `malli.dev/start!`:
      - The :ns option must be a compile-time literal as the namespace symbol(s) must be available at compile time, not runtime.
      - Does not unstrument functions - this is handled by hot reloading.
      - Does not emit clj-kondo type annotations. See `malli.clj-kondo/print-cljs!` to print clj-kondo config.
      - Does not re-instrument functions if the function schemas change - use hot reloading to get a similar effect."
     ([] `(start! {:report (malli.dev.pretty/thrower) :skip-instrumented? true}))
     ([options]
      ;; register all function schemas and instrument them based on the options
      ;; first clear out all metadata schemas to support dev-time removal of metadata schemas on functions - they should not be instrumented
      `(do
         (m/-deregister-metadata-function-schemas! :cljs)
         (malli.instrument/collect! {:ns ~(if (:ns options)
                                            (:ns options)
                                            (vec (ana-api/all-ns)))})
         (malli.instrument/instrument! (assoc ~options :data (m/function-schemas :cljs)))
         (js/console.groupCollapsed "Instrumentation done")
         (js/console.groupEnd)))))

;; only used by deprecated malli.instrument.cljs implementation
#?(:clj (defmacro deregister-function-schemas! []
          (m/-deregister-function-schemas! :cljs)
          nil))
