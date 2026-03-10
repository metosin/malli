(ns malli.registry
  "Conventions:
   - ?registry: may be nil, Registry, or a map.
   - ?registries: a collection of ?registry"
  (:refer-clojure :exclude [type])
  #?(:clj (:import (java.util HashMap Map))))

#?(:cljs (goog-define mode "default")
   :clj  (def mode (or (System/getProperty "malli.registry/mode") "default")))

#?(:cljs (goog-define type "default")
   :clj  (def type (or (System/getProperty "malli.registry/type") "default")))

(defprotocol Registry
  (-schema [this type] "returns the schema from a registry")
  (-schemas [this] "returns all schemas from a registry"))

(defn registry? [x] (#?(:clj instance?, :cljs implements?) malli.registry.Registry x))

(defn fast-registry
  "Create a Registry from a map."
  [m]
  (let [fm #?(:clj (doto (HashMap. 1024 0.25) (.putAll ^Map m)), :cljs m)]
    (reify
      Registry
      (-schema [_ type] (.get fm type))
      (-schemas [_] m))))

(defn simple-registry
  "Create a Registry from a map."
  [m]
  (reify
    Registry
    (-schema [_ type] (m type))
    (-schemas [_] m)))

(defn registry
  "Converts input to a Registry or nil"
  [?registry]
  (cond (nil? ?registry) nil
        (registry? ?registry) ?registry
        (map? ?registry) (simple-registry ?registry)
        (satisfies? Registry ?registry) ?registry))

;;
;; custom
;;

(def ^:private registry* (atom (simple-registry {})))

(defn set-default-registry!
  "Set the default registry"
  [?registry]
  (if-not #?(:cljs (identical? mode "strict")
             :default (= mode "strict"))
    (reset! registry* (registry ?registry))
    (throw (ex-info "can't set default registry, invalid mode" {:mode mode, :type type}))))

(defn ^:no-doc custom-default-registry []
  (reify
    Registry
    (-schema [_ type] (-schema @registry* type))
    (-schemas [_] (-schemas @registry*))))

(defn composite-registry
  "Return a Registry backed by the ?registries. Registries further left take precedence."
  [& ?registries]
  (let [registries (mapv registry ?registries)]
    (reify
      Registry
      (-schema [_ type] (some #(-schema % type) registries))
      (-schemas [_] (reduce merge (map -schemas (reverse registries)))))))

(defn mutable-registry
  "Takes a deref'able db containing a ?registry, returning
  a Registry that forwards to db's ?registry."
  [db]
  (reify
    Registry
    (-schema [_ type] (-schema (registry @db) type))
    (-schemas [_] (-schemas (registry @db)))))

(defn var-registry
  "Adds support for Clojure vars."
  []
  (reify
    Registry
    (-schema [_ type] (if (var? type) @type))
    (-schemas [_])))

(def ^:dynamic *registry* {})

(defn dynamic-registry []
  (reify
    Registry
    (-schema [_ type] (-schema (registry *registry*) type))
    (-schemas [_] (-schemas (registry *registry*)))))

(defn lazy-registry [default-registry provider]
  (let [cache* (atom {})
        registry* (atom default-registry)]
    (reset!
     registry*
     (composite-registry
      default-registry
      (reify
        Registry
        (-schema [_ name]
          (or (@cache* name)
              (when-let [schema (provider name @registry*)]
                (swap! cache* assoc name schema)
                schema)))
        (-schemas [_] @cache*))))))

(defn schema
  "finds a schema from a registry"
  [registry type]
  (-schema registry type))

(defn schemas
  "finds all schemas from a registry"
  [registry]
  (-schemas registry))
