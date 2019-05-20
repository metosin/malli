(ns malli.core
  "Plain data schemas.")

(defprotocol SchemaFunctor
  ""
  (-map-children [self f]))

(extend-protocol SchemaFunctor
  clojure.lang.APersistentVector
  (-map-children [self f]
    (if (seq self)
      (into [(first self)] (map f) (rest self))
      self))

  clojure.lang.Keyword
  (-map-children [self _] self))

(defn map-children [f schema] (-map-children schema f))

;;;;

(defn- expand-node-once [registry schema]
  (cond
    (vector? schema) (if-let [expander (and (seq schema) (get registry (first schema)))]
                       (apply expander (rest schema))
                       schema)
    (keyword? schema) (get registry schema schema)
    :else schema))

(defn expand-node [registry schema]
  (let [schema* (expand-node-once registry schema)]
    (if (identical? schema* schema)
      schema*
      (recur registry schema*))))

(defn expand
  "Recursively expand `schema` and its children using the abbreviations and expanders in `registry`."
  [registry schema]
  (map-children (partial expand registry)
                (expand-node registry schema)))

;;;;

(deftype IntSchema []
  SchemaFunctor
  (-map-children [self _] self))

(def Int
  "Schema for integers."
  (->IntSchema))

(declare ->SetSchema)

(deftype SetSchema [elem-schema]
  SchemaFunctor
  (-map-children [_ f] (->SetSchema (f elem-schema))))

(def Set
  "Schema constructor for sets."
  ->SetSchema)

;;;;

(def default-registry
  {::int Int
   ::set Set})
