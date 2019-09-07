(ns malli.json-schema
  (:require [malli.core :as m]))

(defn unlift-keys [m ns-str]
  (reduce-kv #(if (= ns-str (namespace %2)) (assoc %1 (keyword (name %2)) %3) %1) {} m))

(defn json-schema-props [schema prefix]
  (as-> (m/properties schema) $ (merge (select-keys $ [:title :description :default]) (unlift-keys $ prefix))))

(defmulti accept (fn [name _schema _childs _opts] name) :default ::default)

(defmethod accept ::default [_ _ _ _] {})
(defmethod accept 'any? [_ _ _ _] {})
(defmethod accept 'some? [_ _ _ _] {})
(defmethod accept 'number? [_ _ _ _] {:type "number" :format "double"})
(defmethod accept 'integer? [_ _ _ _] {:type "integer"})
(defmethod accept 'int? [_ _ _ _] {:type "integer" :format "int64"})
(defmethod accept 'pos-int? [_ _ _ _] {:type "integer", :format "int64", :minimum 1})
(defmethod accept 'neg-int? [_ _ _ _] {:type "integer", :format "int64", :maximum -1})
(defmethod accept 'nat-int? [_ _ _ _] {:type "integer", :format "int64" :minimum 0})
(defmethod accept 'float? [_ _ _ _] {:type "number"})
(defmethod accept 'double? [_ _ _ _] {:type "number"})
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
(defmethod accept 'decimal? [_ _ _ _] {:type "number" :format "double"})
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
#?(:clj (defmethod accept 'rational? [_ _ _ _] {:type "double"}))
(defmethod accept 'coll? [_ _ _ _] {:type "object"})
(defmethod accept 'empty? [_ _ _ _] {:type "array" :maxItems 0 :minItems 0})
(defmethod accept 'associative? [_ _ _ _] {:type "object"})
(defmethod accept 'sequential? [_ _ _ _] {:type "array"})
(defmethod accept 'ratio? [_ _ _ _] {:type "integer"})
(defmethod accept 'bytes? [_ _ _ _] {:type "string" :format "byte"})

(defmethod accept :> [_ _ [value] _] {:type "number" :format "double" :exclusiveMinimum value})
(defmethod accept :>= [_ _ [value] _] {:type "number" :format "double" :minimum value})
(defmethod accept :< [_ _ [value] _] {:type "number" :format "double" :exclusiveMaximum value})
(defmethod accept :<= [_ _ [value] _] {:type "number" :format "double" :maximum value})
(defmethod accept := [_ _ _ _] {})
(defmethod accept :not= [_ _ _ _] {})

(defmethod accept :and [_ _ children _] {:allOf children})
(defmethod accept :or [_ _ children _] {:anyOf children})

(defmethod accept :map [_ schema children opts]
  (let [{:keys [required keys]} (m/-parse-keys (m/childs schema opts) opts)]
    (merge
      {:type "object"
       :properties (apply array-map (interleave keys children))
       :required required}
      (json-schema-props schema "json-schema"))))

(defmethod accept :map-of [_ _ children _] {:type "object", :additionalProperties (second children)})
(defmethod accept :vector [_ _ children _] {:type "array", :items children})
(defmethod accept :list [_ _ children _] {:type "array", :items children})
(defmethod accept :set [_ _ children _] {:type "array", :items children, :uniqueItems true})
(defmethod accept :enum [_ _ children _] {:enum children})
(defmethod accept :maybe [_ _ children _] {:oneOf (conj children {:type "null"})})
(defmethod accept :tuple [_ _ children _] {:type "array", :items children, :additionalItems false})
(defmethod accept :fn [_ _ _ _] {})

(defn- -json-schema-visitor [schema childs opts]
  (merge (accept (m/name schema) schema childs opts) (json-schema-props schema "json-schema")))

;;
;; public api
;;

(defn transform
  ([?schema]
   (transform ?schema nil))
  ([?schema opts]
   (m/accept ?schema -json-schema-visitor opts)))
