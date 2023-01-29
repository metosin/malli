(ns malli.dev.shadow-cljs-hooks
  (:require [clojure.edn :as edn]
            [malli.clj-kondo :as clj-kondo]
            [shadow.cljs.devtools.api :as sh]))

(def build-number_ (atom 0))
(def running?_ (atom false))

(defn fetch-build-number [build-id]
  (when-let [num-str (-> (sh/cljs-eval build-id "(malli.dev.cljs/get-build-number)" {}) :results first)]
    (parse-long num-str)))

(defn fetch-clj-kondo-config [build-id]
  (-> build-id
    (sh/cljs-eval "(malli.clj-kondo/get-kondo-config)" {})
    :results first edn/read-string))

(defn fetch-and-save-kondo-config* [build-id]
  (when-let [config (fetch-clj-kondo-config build-id)]
    (clj-kondo/save! config)))

(def max-retries 10)

(defn fetch-and-save-kondo-config [build-id]
  (if (zero? @build-number_)
    (do (fetch-and-save-kondo-config* build-id)
        (swap! build-number_ inc))
    (loop [client-build-number (fetch-build-number build-id)
           retries             0]
      (when client-build-number
        (cond (> client-build-number @build-number_)
              (do
                (fetch-and-save-kondo-config* build-id)
                (reset! build-number_ client-build-number))

              ;; If the client state is reset this will return to 0
              (< client-build-number @build-number_)
              (reset! build-number_ client-build-number)

              ;; if the build numbers are equal, wait for the client to update
              :else
              (when (< retries max-retries)
                (Thread/sleep 500)
                (recur (fetch-build-number build-id) (inc retries))))))))

(defn kondo-hook
  "After each ClojureScript compilation will fetch the clj-kondo config from the client application and persist to disk."
  {:shadow.build/stage :flush}
  [build-state & [build-id]]
  (when-not build-id (throw (Exception. "Missing build-id in clj-kondo hook")))
  (when (and (= (:build-id build-state) build-id) (not @running?_) (compare-and-set! running?_ false true))
    (future
      (fetch-and-save-kondo-config build-id)
      (reset! running?_ false)))
  build-state)
