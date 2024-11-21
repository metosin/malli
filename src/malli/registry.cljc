(ns malli.registry
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

(defn fast-registry [m]
  (let [fm #?(:clj (doto (HashMap. 1024 0.25) (.putAll ^Map m)), :cljs m)]
    (reify
      Registry
      (-schema [_ type] (.get fm type))
      (-schemas [_] m))))

(defn simple-registry [m]
  (reify
    Registry
    (-schema [_ type] (m type))
    (-schemas [_] m)))

(defn registry [?registry]
  (cond (nil? ?registry) nil
        (registry? ?registry) ?registry
        (map? ?registry) (simple-registry ?registry)
        (satisfies? Registry ?registry) ?registry))

;;
;; custom
;;

(def ^:private registry* (atom (simple-registry {})))

(defn set-default-registry! [?registry]
  (if-not #?(:cljs (identical? mode "strict")
             :default (= mode "strict"))
    (reset! registry* (registry ?registry))
    (throw (ex-info "can't set default registry, invalid mode" {:mode mode, :type type}))))

(defn ^:no-doc custom-default-registry []
  (reify
    Registry
    (-schema [_ type] (-schema @registry* type))
    (-schemas [_] (-schemas @registry*))))

(defn composite-registry [& ?registries]
  (let [registries (mapv registry ?registries)]
    (reify
      Registry
      (-schema [_ type] (some #(-schema % type) registries))
      (-schemas [_] (reduce merge (map -schemas (reverse registries)))))))

(defn mutable-registry [db]
  (reify
    Registry
    (-schema [_ type] (-schema (registry @db) type))
    (-schemas [_] (-schemas (registry @db)))))

(defn var-registry []
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
