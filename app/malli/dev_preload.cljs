(ns malli.dev-preload
  {:dev/always true}
  (:require
   [malli.instrument-app]
   [malli.dev.cljs :as dev]))

(dev/start!)
