(ns malli.registry
  (:refer-clojure :exclude [type]))

#?(:cljs (goog-define type "default")
   :clj  (def type (as-> (or (System/getProperty "malli.registry/type") "default") $ (.intern $))))

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
  (if (identical? type "managed")
    (reset! state (registry ?registry))
    (throw (ex-info "invalid registry type" {:type type}))))

(defn managed-registry []
  (reify
    Registry
    (-schema [_ name] (-schema @state name))
    (-schemas [_] (-schemas @state))))

(defn composite-registry [& ?registries]
    (let [registries (mapv registry ?registries)]
      (reify
        Registry
        (-schema [_ name] (some #(-schema % name) registries))
        (-schemas [_] (reduce merge (map -schemas (reverse registries)))))))

(defn mutable-registry [db]
  (reify
    Registry
    (-schema [_ name] (get @db name))
    (-schemas [_] @db)))

(def ^:dynamic *registry* {})

(defn dynamic-registry []
  (reify
    Registry
    (-schema [_ name] (get *registry* name))
    (-schemas [_] *registry*)))
