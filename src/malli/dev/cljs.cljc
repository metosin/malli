(ns malli.dev.cljs
  #?(:cljs (:require-macros [malli.dev.cljs]))
  #?(:cljs (:require [malli.instrument :as mi]
                     [malli.core :as m]
                     ;; include kondo ns so the client build has it present to be called by a shadow-cljs hook
                     [malli.clj-kondo :as clj-kondo]
                     [malli.dev.pretty :as pretty]))
  #?(:clj (:require [cljs.analyzer.api :as ana-api]
                    [malli.clj-kondo :as clj-kondo]
                    [malli.core :as m]
                    [malli.instrument])))

#?(:clj (defmacro stop!
          "Stops instrumentation for all functions vars and removes clj-kondo type annotations."
          []
          `(do
             (malli.instrument/unstrument! nil)
             ~(do (clj-kondo/save! {}) nil))))

#?(:clj (defmacro collect-all! [] (malli.instrument/collect! {:ns (ana-api/all-ns)})))

;; This is used to track when compilation is finished and is read by the shadow-cljs hook that is used to fetch
;; clj-kondo config data from the client.
#?(:cljs (defonce build-number_ (volatile! 0)))
#?(:cljs (defn get-build-number [] @build-number_))

#?(:clj
   (defmacro start!
     "Collects defn schemas from all loaded namespaces and starts instrumentation for
      a filtered set of function Vars (e.g. `defn`s). See [[malli.core/-instrument]] for possible options.
      Differences from Clojure `malli.dev/start!`:
      - The :ns option must be a compile-time literal as the namespace symbol(s) must be available at compile time, not runtime.
      - Does not unstrument functions - this is handled by hot reloading.
      - Does not emit clj-kondo type annotations. See `malli.clj-kondo/print-cljs!` to print clj-kondo config.
      - Does not re-instrument functions if the function schemas change - use hot reloading to get a similar effect."
     ([] `(start! {:report (malli.dev.pretty/thrower) :skip-instrumented? false}))
     ([options]
      ;; register all function schemas and instrument them based on the options
      ;; first clear out all metadata schemas to support dev-time removal of metadata schemas on functions - they should not be instrumented
      `(do
         (m/-deregister-metadata-function-schemas! :cljs)
         (malli.instrument/collect! {:ns ~(if (:ns options)
                                            (:ns options)
                                            (vec (ana-api/all-ns)))})
         (js/console.groupCollapsed "Instrumentation done")
         (malli.instrument/instrument! (assoc ~options :data (m/function-schemas :cljs)))
         (js/console.groupEnd)
         (vswap! build-number_ inc)))))

;; only used by deprecated malli.instrument.cljs implementation
#?(:clj (defmacro deregister-function-schemas! []
          (m/-deregister-function-schemas! :cljs)
          nil))
