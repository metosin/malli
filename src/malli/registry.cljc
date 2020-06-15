(ns malli.registry)

(defprotocol Registry
  (-get-schema [this name] "returns the schema from a registry")
  (-get-schemas [this] "returns all schemas from a registry"))

(defn registry? [x] (satisfies? Registry x))

(defn simple-registry [schemas]
  (reify
    Registry
    (-get-schema [_ name] (get schemas name))
    (-get-schemas [_] schemas)))
