(ns malli.registry
  (:refer-clojure :exclude [type]))

(defprotocol Registry
  (-schema [this name] "returns the schema from a registry")
  (-schemas [this] "returns all schemas from a registry"))

(defn simple-registry [schemas]
  (reify
    Registry
    (-schema [_ name] (get schemas name))
    (-schemas [_] schemas)))

(defn registry
  ([x] (registry x nil))
  ([x default] (cond (satisfies? Registry x) x
                     (map? x) (simple-registry x)
                     :else default)))

#?(:cljs (goog-define type "default")
   :clj  (def type (or (System/getProperty "malli.registry/type") "default")))

(defn default-registry [schemas]
  (if (identical? type "default")
    (registry schemas nil)
    (or (some-> type symbol requiring-resolve (apply nil) (registry))
        (throw (ex-info (str "invalid registry malli.registry/type " type) {})))))
