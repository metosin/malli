(ns malli.dev.cljs
  #?(:cljs (:require-macros [malli.dev.cljs]))
  #?(:cljs (:require [malli.instrument.cljs]
                     [malli.dev.pretty :as pretty]))
  #?(:clj (:require [cljs.analyzer.api :as ana-api]
                    [malli.clj-kondo :as clj-kondo]
                    [malli.dev.pretty :as pretty]
                    [malli.instrument.cljs :as mi]
                    [malli.core :as m])))

#?(:clj (defonce
         ^{:doc
           "Used during development to hold only the namespaces that were updated by a user with code edits and thus need
           to be re-collected and instrumented."}
         updated-namespaces
         (atom #{})))

#?(:clj (defmacro stop!
          "Stops instrumentation for all functions vars and removes clj-kondo type annotations."
          []
          `(do
             ~(mi/-unstrument &env nil)
             ~(do (clj-kondo/save! {}) nil))))

#?(:clj
   (defn start!* [env options]
     `(do
        (js/console.groupCollapsed "Instrumentation done")
        ;; register all function schemas and instrument them based on the options
        ;; first clear out all metadata schemas to support dev-time removal of metadata schemas on functions - they should not be instrumented
        ~(m/-deregister-metadata-function-schemas! nil :cljs)
        ~(mi/-collect-all-ns)
        ~(mi/-instrument env options)
        (js/console.groupEnd))))

#?(:clj
   (defn refresh!* [env options]
     `(do
        (js/console.groupCollapsed "Instrumentation done")
        ;; register all function schemas and instrument them based on the options
        ;; first clear out all metadata schemas to support dev-time removal of metadata schemas on functions - they should not be instrumented
        ~(m/-deregister-metadata-function-schemas! @updated-namespaces :cljs)
        ~(mi/-collect!* {:ns @updated-namespaces})
        ~(mi/-instrument env options)
        (js/console.groupEnd))))

#?(:clj (defmacro start!
          "Collects defn schemas from all loaded namespaces and starts instrumentation for
           a filtered set of function Vars (e.g. `defn`s). See [[malli.core/-instrument]] for possible options.
           Differences from Clojure `malli.dev/start!`:

           - Does not unstrument functions - this is handled by hot reloading.
           - Does not emit clj-kondo type annotations. See `malli.clj-kondo/print-cljs!` to print clj-kondo config.
           - Does not re-instrument functions if the function schemas change - use hot reloading to get a similar effect."
          ([] (start!* &env {:report `(pretty/thrower)}))
          ([options] (start!* &env options))))

#?(:clj (defmacro refresh!
          "Intended for use only during development. Collects defn schemas from only those namespaces which were updated since the
          last build and starts instrumentation for a filtered set of function Vars (e.g. `defn`s).
          See [[malli.core/-instrument]] for possible options. Differences from Clojure `malli.dev/start!`:

          - Does not unstrument functions - this is handled by hot reloading.
          - Does not emit clj-kondo type annotations. See `malli.clj-kondo/print-cljs!` to print clj-kondo config.
          - Does not re-instrument functions if the function schemas change - use hot reloading to get a similar effect."
          ([] (refresh!* &env {:report `(pretty/thrower)}))
          ([options] (refresh!* &env options))))

#?(:clj (defmacro collect-all! [] (mi/-collect-all-ns)))

#?(:clj (defmacro deregister-function-schemas! []
          (m/-deregister-function-schemas! :cljs)
          nil))
