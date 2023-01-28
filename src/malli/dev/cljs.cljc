(ns malli.dev.cljs
  #?(:cljs (:require-macros [malli.dev.cljs]))
  #?(:cljs (:require [clojure.string :as str]
                     [goog.object :as g]
                     [malli.instrument :as mi]
                     [malli.core :as m]
                     [malli.clj-kondo :as clj-kondo]
                     [malli.dev.pretty :as pretty]))
  #?(:clj (:require [cljs.analyzer.api :as ana-api]
                    [clojure.string :as str]
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

#?(:cljs
   (defn send-kondo-config-to-shadow!
     "During development sends the clj-kondo config data for all collected functions with malli schemas to the
     shadow-cljs build hook which writes it to disk.
     If shadow-cljs is not present on the client this is a no-op."
     []
     (when (exists? js/shadow)
       (let [ns-js-path         (fn [ns] (into-array (map munge (str/split (str ns) #"\."))))
             shadow-runtime     (g/getValueByKeys goog/global (ns-js-path 'shadow.remote.runtime.shared))
             shadow-call        (if shadow-runtime (g/get shadow-runtime "call") identity)

             shadow-client      (g/getValueByKeys goog/global (ns-js-path 'shadow.cljs.devtools.client.shared))
             shadow-runtime-ref (when shadow-client (g/get shadow-client "runtime_ref"))

             shadow-client-env  (g/getValueByKeys goog/global (ns-js-path 'shadow.cljs.devtools.client.env))
             runtime-state      (some-> (deref shadow-runtime-ref) :state-ref deref)]
         ;; This is to wait until the shadow runtime is initialized, config will not be written until the second
         ;; save during hot-reload.
         (when (< 0 (:call-id-seq runtime-state))
           (shadow-call
             (deref shadow-runtime-ref)
             {:op       ::clj-kondo/write-config
              :to       (some-> shadow-client-env (g/get "worker_client_id"))
              :build-id (some-> shadow-client-env (g/get "build_id") keyword)
              :data     (clj-kondo/get-kondo-config)}
             {}))))))

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
         (send-kondo-config-to-shadow!)))))

;; only used by deprecated malli.instrument.cljs implementation
#?(:clj (defmacro deregister-function-schemas! []
          (m/-deregister-function-schemas! :cljs)
          nil))
