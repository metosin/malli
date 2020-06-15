(ns malli.mutable
  (:require [malli.core :as m]
            [malli.registry :as mr]))

(defonce ^:private state (atom {}))

(defn registry [schemas]
  (reify
    mr/Registry
    (-get-schema [_ name] (or (get schemas name) (get @state name)))
    (-get-schemas [_] (merge @state schemas))))

(defn default-registry [] (registry (m/default-schemas)))

(defn register! [type ?schema]
  (if ?schema
    (swap! state assoc type (m/schema ?schema {:registry (m/registry)}))
    (swap! state dissoc type)))
