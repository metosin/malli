(ns malli.dev.shadow-cljs-instrument-hooks
  "Used during development with shadow-cljs for ClojureScript function instrumentation."
  (:require
    [clojure.core.async :as async]
    [malli.dev.cljs :as md]
    [shadow.cljs.devtools.server.supervisor :as supervisor]
    [shadow.cljs.devtools.server.runtime :as runtime]
    [shadow.cljs.util :as s.util]))

(defn add-ns-to-set! [ns] (swap! md/updated-namespaces conj ns))
(defonce first-compile?_ (atom false))

(defn configure-hook
  {:shadow.build/stage :configure}
  [build-state & [build-id]]
  (let [output-chan (async/chan)
        worker      (supervisor/get-worker (:supervisor (runtime/get-instance!)) build-id)]
    (async/tap (:output-mult worker) output-chan)
    (async/go-loop [msg (async/<! output-chan)]
      (when
        (and
          (not @first-compile?_)
          (= :build-log (:type msg))
          (= :compile-cljs (:type (:event msg)))
          (= :enter (-> msg :event :timing)))
        (add-ns-to-set! (s.util/filename->ns (:resource-name (:event msg)))))
      (recur (async/<! output-chan)))
    build-state))

(defn compile-flush-hook
  {:shadow.build/stage :flush}
  [build-state]
  (reset! first-compile?_ false)
  (reset! md/updated-namespaces #{})
  build-state)
