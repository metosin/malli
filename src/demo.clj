(ns demo
  (:require [malli.experimental :as mx]
            [malli.provider :as mp]))

;; pull schema from example
(def Config
  (mp/provide
   [{:port 8080
     :host "127.0.0.1"
     :db {:adapter "postgresql"
          :username "example"
          :password "example"
          :server-name "localhost"
          :port-number 5432
          :database-name "example"}}]))

;; fail fast
(mx/def config :- Config
  {:port "8080"
   :host "127.0.0.1"
   :db {:adapter "postgresql"
        :username "example"
        :password "example"
        :server-name "localhost"
        :port-number "5432"
        :database-name :example}})
