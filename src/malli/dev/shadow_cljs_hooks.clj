(ns malli.dev.shadow-cljs-hooks
  (:require [malli.clj-kondo :as clj-kondo]
            [shadow.cljs.devtools.api :as sh]
            [shadow.cljs.devtools.config :as config]
            [shadow.cljs.devtools.server :as server]
            [shadow.cljs.devtools.server.worker.impl :as worker]))

(def build-id_ (atom nil))

(defmethod worker/do-relay-msg ::clj-kondo/write-config
  [worker-state msg]
  (when (= @build-id_ (:build-id msg))
    (future
      (malli.clj-kondo/save! (:data msg))))
  worker-state)

(defn kondo-hook
  "This hook is a no-op used to setup a method handler for kondo config messages from the CLJS frontend app."
  {:shadow.build/stage :flush}
  [build-state & [build-id]]
  (when-not build-id (throw (Exception. "Missing build-id in clj-kondo hook")))
  (reset! build-id_ build-id)
  build-state)
