(ns malli.registry
  (:refer-clojure :exclude [type]))

#?(:cljs (goog-define type "default")
   :clj  (def type (.intern (or (System/getProperty "malli.registry/type") "default"))))

(declare simple-registry)

(defprotocol Registry
  (-schema [this name] "returns the schema from a registry")
  (-schemas [this] "returns all schemas from a registry"))

(defn registry [?registry]
  (cond (satisfies? Registry ?registry) ?registry
        (map? ?registry) (simple-registry ?registry)))

;;
;; simple
;;

(defn simple-registry [schemas]
  (reify
    Registry
    (-schema [_ name] (get schemas name))
    (-schemas [_] schemas)))

;;
;; managed
;;

(let [registry* (atom (registry {}))]

  (defn set! [?registry]
    (reset! registry* (registry ?registry)))

  (defn managed-registry []
    (reify
      Registry
      (-schema [_ name] (-schema @registry* name))
      (-schemas [_] (-schemas @registry*)))))
