(ns malli.dev.cljs
  #?(:cljs (:require-macros [malli.dev.cljs]))
  #?(:cljs (:require [malli.instrument.cljs]
                     [malli.dev.pretty :as pretty]))

  #?(:clj (:require [malli.clj-kondo :as clj-kondo]
                    [malli.dev.pretty :as pretty]
                    [malli.instrument.cljs :as mi])))

#?(:clj (defmacro stop!
          "Stops instrumentation for all functions vars and removes clj-kondo type annotations."
          []
          `(do
             ~(mi/-unstrument &env nil)
             ~(do (clj-kondo/save! {}) nil))))
#?(:clj
   (defn start!* [env options]
     `(do
        ~(mi/-unstrument env nil)
        ~(do (clj-kondo/save! {}) nil)
        ;; malli.dev/stop ^^

        ;; register all function schemas and instrument them based on the options
        ~(mi/-collect-all-ns)
        ~(mi/-instrument env options)
        ~(do (clj-kondo/emit-cljs!*) nil)) ) )

#?(:clj (defmacro start!
          "Collects defn schemas from all loaded namespaces and starts instrumentation for
           a filtered set of function Vars (e.g. `defn`s). See [[malli.core/-instrument]] for possible options.
           Also emits clj-kondo type annotations.
           This does NOT re-instrument functions if the function schemas change - use hot reloading to get a similar effect."
          ([] (start!* &env {:report `(pretty/reporter)}))
          ([options] (start!* &env options))))

#?(:clj (defmacro collect-all! [] (mi/-collect-all-ns)))
