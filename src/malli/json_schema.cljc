(ns malli.json-schema
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [malli.core :as m]))

(declare -transform)

(defprotocol JsonSchema
  (-accept [this children options] "transforms schema to JSON Schema"))

(defn -join-ref [prefix suffix]
  ;; kludge to make :foo.bar/quux and :foo/bar.quux not collide
  (str prefix
       (if (str/includes? (str suffix) ".") ".." ".")
       suffix))

(defn -ref [schema {::keys [transform definitions definitions-path]
                    :or {definitions-path "#/definitions/"}
                    :as options}]
  (let [ref (as-> (m/-ref schema) $
              (cond (var? $) (let [{:keys [ns name]} (meta $)]
                               (-join-ref ns name))
                    (qualified-ident? $) (-join-ref (namespace $) (name $))
                    :else (str $)))]
    (when-not (contains? @definitions ref)
      (let [child (m/deref schema)]
        (swap! definitions assoc ref ::recursion-stopper)
        (swap! definitions assoc ref (transform child options))))
    ;; '/' must be encoded as '~1' in JSON Schema - https://www.rfc-editor.org/rfc/rfc6901
    ;; However, tools like openapi-schema-validator disallow ~1, so we use "." as the separator above.
    ;; This str/replace is left here in case a user has managed to smuggle a "/" in to the type name.
    {:$ref (apply str definitions-path (str/replace ref #"/" "~1"))}))

(defn -schema [schema {::keys [transform] :as options}]
  (if (m/-ref schema)
    (-ref schema options)
    (transform (m/deref schema) options)))

(defn select [m] (select-keys m [:title :description :default]))

(defmulti accept (fn [name _schema _children _options] name) :default ::default)

(defmethod accept ::default [_ _ _ _] {})
(defmethod accept 'any? [_ _ _ _] {})
(defmethod accept 'some? [_ _ _ _] {})
(defmethod accept 'number? [_ _ _ _] {:type "number"})
(defmethod accept 'integer? [_ _ _ _] {:type "integer"})
(defmethod accept 'int? [_ _ _ _] {:type "integer"})
(defmethod accept 'pos-int? [_ _ _ _] {:type "integer", :minimum 1})
(defmethod accept 'neg-int? [_ _ _ _] {:type "integer", :maximum -1})
(defmethod accept 'nat-int? [_ _ _ _] {:type "integer", :minimum 0})
(defmethod accept 'float? [_ _ _ _] {:type "number"})
(defmethod accept 'double? [_ _ _ _] {:type "number"})
(defmethod accept 'float? [_ _ _ _] {:type "number"})
(defmethod accept 'pos? [_ _ _ _] {:type "number" :exclusiveMinimum 0})
(defmethod accept 'neg? [_ _ _ _] {:type "number" :exclusiveMaximum 0})
(defmethod accept 'boolean? [_ _ _ _] {:type "boolean"})
(defmethod accept 'string? [_ _ _ _] {:type "string"})
(defmethod accept 'ident? [_ _ _ _] {:type "string"})
(defmethod accept 'simple-ident? [_ _ _ _] {:type "string"})
(defmethod accept 'qualified-ident? [_ _ _ _] {:type "string"})
(defmethod accept 'keyword? [_ _ _ _] {:type "string"})
(defmethod accept 'simple-keyword? [_ _ _ _] {:type "string"})
(defmethod accept 'qualified-keyword? [_ _ _ _] {:type "string"})
(defmethod accept 'symbol? [_ _ _ _] {:type "string"})
(defmethod accept 'simple-symbol? [_ _ _ _] {:type "string"})
(defmethod accept 'qualified-symbol? [_ _ _ _] {:type "string"})
(defmethod accept 'uuid? [_ _ _ _] {:type "string" :format "uuid"})
(defmethod accept 'uri? [_ _ _ _] {:type "string" :format "uri"})
(defmethod accept 'decimal? [_ _ _ _] {:type "number"})
(defmethod accept 'inst? [_ _ _ _] {:type "string" :format "date-time"})
(defmethod accept 'seqable? [_ _ _ _] {:type "array"})
(defmethod accept 'indexed? [_ _ _ _] {:type "array"})
(defmethod accept 'map? [_ _ _ _] {:type "object"})
(defmethod accept 'vector? [_ _ _ _] {:type "array"})
(defmethod accept 'list? [_ _ _ _] {:type "array"})
(defmethod accept 'seq? [_ _ _ _] {:type "array"})
(defmethod accept 'char? [_ _ _ _] {:type "string"})
(defmethod accept 'set? [_ _ _ _] {:type "array" :uniqueItems true})
(defmethod accept 'nil? [_ _ _ _] {:type "null"})
(defmethod accept 'false? [_ _ _ _] {:type "boolean"})
(defmethod accept 'true? [_ _ _ _] {:type "boolean"})
(defmethod accept 'zero? [_ _ _ _] {:type "integer"})
#?(:clj (defmethod accept 'rational? [_ _ _ _] {:type "number"}))
(defmethod accept 'coll? [_ _ _ _] {:type "object"})
(defmethod accept 'empty? [_ _ _ _] {:type "array" :maxItems 0 :minItems 0})
(defmethod accept 'associative? [_ _ _ _] {:type "object"})
(defmethod accept 'sequential? [_ _ _ _] {:type "array"})
#?(:clj (defmethod accept 'ratio? [_ _ _ _] {:type "number"}))
(defmethod accept 'bytes? [_ _ _ _] {:type "string" :format "byte"})
(defmethod accept 'ifn? [_ _ _ _] {})
(defmethod accept 'fn? [_ _ _ _] {})

(defmethod accept :> [_ _ [value] _] {:type "number" :exclusiveMinimum value})
(defmethod accept :>= [_ _ [value] _] {:type "number" :minimum value})
(defmethod accept :< [_ _ [value] _] {:type "number" :exclusiveMaximum value})
(defmethod accept :<= [_ _ [value] _] {:type "number" :maximum value})
(defmethod accept := [_ _ [value] _] {:const value})
(defmethod accept :not= [_ _ _ _] {})

(defmethod accept :not [_ _ children _] {:not (last children)})
(defmethod accept :and [_ _ children _] {:allOf children})
(defmethod accept :or [_ _ children _] {:anyOf children})
(defmethod accept :orn [_ _ children _] {:anyOf (map last children)})

(defmethod accept ::m/val [_ _ children _] (first children))

(defmethod accept :map [_ schema children _]
  (let [ks (set (m/explicit-keys schema))
        default (some->> children (remove (m/-comp ks first)) first last)
        {additionalProperties' :additionalProperties properties' :properties required' :required} default
        children (filter (m/-comp ks first) children)
        required (->> children (filter (m/-comp not :optional second)) (mapv first))
        closed (:closed (m/properties schema))
        object {:type "object"
                :properties (apply array-map (mapcat (fn [[k _ s]] [k s]) children))}]
    (cond-> (merge default object)
      (seq required) (assoc :required required)
      closed (assoc :additionalProperties false)
      default (cond->
                additionalProperties' (assoc :additionalProperties additionalProperties')
                properties' (update :properties merge properties')
                required' (update :required (comp vec distinct into) required')))))

(defmethod accept :multi [_ _ children _] {:oneOf (mapv last children)})

(defn- minmax-properties [m schema kmin kmax]
  (merge m (-> schema (m/properties) (select-keys [:min :max]) (set/rename-keys {:min kmin, :max kmax}))))

(defmethod accept :map-of [_ schema children _]
  (minmax-properties
   {:type "object",
    :additionalProperties (second children)}
   schema
   :minProperties
   :maxProperties))

(defmethod accept :vector [_ schema children _]
  (minmax-properties
   {:type "array", :items (first children)}
   schema
   :minItems
   :maxItems))

(defmethod accept :sequential [_ schema children _]
  (minmax-properties
   {:type "array", :items (first children)}
   schema
   :minItems
   :maxItems))

(defmethod accept :set [_ schema children _]
  (minmax-properties
   {:type "array", :items (first children), :uniqueItems true}
   schema
   :minItems
   :maxItems))

(defmethod accept :enum [_ _ children options] (merge (some-> (m/-infer children) (-transform options)) {:enum children}))
(defmethod accept :maybe [_ _ children _] {:oneOf (conj children {:type "null"})})
(defmethod accept :tuple [_ _ children _] {:type "array", :prefixItems children, :items false})
(defmethod accept :re [_ schema _ options] {:type "string", :pattern (first (m/children schema options))})
(defmethod accept :fn [_ _ _ _] {})

(defmethod accept :any [_ _ _ _] {})
(defmethod accept :some [_ _ _ _] {})
(defmethod accept :nil [_ _ _ _] {:type "null"})

(defmethod accept :string [_ schema _ _]
  (merge {:type "string"} (-> schema m/properties (select-keys [:min :max]) (set/rename-keys {:min :minLength, :max :maxLength}))))

(defmethod accept :int [_ schema _ _]
  (merge {:type "integer"} (-> schema m/properties (select-keys [:min :max]) (set/rename-keys {:min :minimum, :max :maximum}))))

(defmethod accept :float [_ schema _ _]
  (merge {:type "number"}
         (-> schema m/properties (select-keys [:min :max]) (set/rename-keys {:min :minimum, :max :maximum}))))

(defmethod accept :double [_ schema _ _]
  (merge {:type "number"}
         (-> schema m/properties (select-keys [:min :max]) (set/rename-keys {:min :minimum, :max :maximum}))))

(defmethod accept :boolean [_ _ _ _] {:type "boolean"})
(defmethod accept :keyword [_ _ _ _] {:type "string"})
(defmethod accept :qualified-keyword [_ _ _ _] {:type "string"})
(defmethod accept :symbol [_ _ _ _] {:type "string"})
(defmethod accept :qualified-symbol [_ _ _ _] {:type "string"})
(defmethod accept :uuid [_ _ _ _] {:type "string" :format "uuid"})

(defmethod accept :=> [_ _ _ _] {})
(defmethod accept :function [_ _ _ _] {})
(defmethod accept :ref [_ schema _ options] (-ref schema options))
(defmethod accept :schema [_ schema _ options] (-schema schema options))
(defmethod accept ::m/schema [_ schema _ options] (-schema schema options))

(defmethod accept :merge [_ schema _ {::keys [transform] :as options}] (transform (m/deref schema) options))
(defmethod accept :union [_ schema _ {::keys [transform] :as options}] (transform (m/deref schema) options))
(defmethod accept :select-keys [_ schema _ {::keys [transform] :as options}] (transform (m/deref schema) options))

(defn- -json-schema-walker [schema _ children options]
  (let [p (merge (m/type-properties schema) (m/properties schema))]
    (or (get p :json-schema)
        (merge (select p)
               (if (satisfies? JsonSchema schema)
                 (-accept schema children options)
                 (accept (m/type schema) schema children options))
               (m/-unlift-keys p :json-schema)))))

(defn -transform [?schema options] (m/walk ?schema -json-schema-walker options))

;;
;; public api
;;

(defn transform
  ([?schema]
   (transform ?schema nil))
  ([?schema options]
   (let [definitions (atom {})
         options (merge options {::m/walk-entry-vals true, ::definitions definitions, ::transform -transform})]
     (cond-> (-transform ?schema options) (seq @definitions) (assoc :definitions @definitions)))))
