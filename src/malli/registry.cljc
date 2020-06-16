(ns malli.registry
  (:refer-clojure :exclude [type]))

#?(:cljs (goog-define type "default")
   :clj  (def type (.intern (or (System/getProperty "malli.registry/type") "default"))))

(defprotocol Registry
  (-schema [this name] "returns the schema from a registry")
  (-schemas [this] "returns all schemas from a registry"))

(defn simple-registry [schemas]
  (reify
    Registry
    (-schema [_ name] (get schemas name))
    (-schemas [_] schemas)))

(defn registry [?registry]
  (cond (satisfies? Registry ?registry) ?registry
        (map? ?registry) (simple-registry ?registry)))

;;
;; managed
;;

(def ^:private state (atom (registry {})))

(defn set-default-registry! [?registry]
  (reset! state (registry ?registry)))

(defn managed-registry []
  (reify
    Registry
    (-schema [_ name] (-schema @state name))
    (-schemas [_] (-schemas @state))))

;;
;; mutable
;;

(defn mutable-registry [?registry db]
  (let [registry (registry ?registry)]
    (reify
      Registry
      (-schema [_ name] (or (-schema registry name) (get @db name)))
      (-schemas [_] (merge @db (-schema registry name))))))
