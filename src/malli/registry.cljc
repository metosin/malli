(ns malli.registry)

(defprotocol Registry
  (-get-schema [this name] "returns the schema from a registry")
  (-get-schemas [this] "returns all schemas from a registry"))

(defn simple-registry [schemas]
  (reify
    Registry
    (-get-schema [_ name] (get schemas name))
    (-get-schemas [_] schemas)))

(defn registry [x default]
  (cond (satisfies? Registry x) x
        (map? x) (simple-registry x)
        :else default))

#?(:cljs (goog-define REGISTRY ""))

#?(:clj
   (defmacro default-registry [default]
     (if-let [registry (some-> (System/getProperty "malli.registry") symbol requiring-resolve)]
       `(~registry)
       `(simple-registry ~default))))
