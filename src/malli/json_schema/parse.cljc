(ns malli.json-schema.parse
  (:require [malli.core :as m]
            [clojure.string :as str]))

;; Utility Functions
(defn- map-values
  ([-fn] (map (fn [[k v]] [k (-fn v)])))
  ([-fn coll] (sequence (map-values -fn) coll)))

;; Parsing
(defmulti type->malli :type)

(defn $ref [v]
  ;; TODO to be improved
  (keyword (last (str/split v #"/"))))

(defn schema->malli [js-schema]
  (let [-keys (set (keys js-schema))]
    (cond
      (-keys :type) (type->malli js-schema)

      ;; Aggregates
      (-keys :oneOf)
      (into
        ;; TODO Figure out how to make it exclusively select o schema
        [:or]
        (map schema->malli)
        (:oneOf js-schema))

      (-keys :anyOf)
      (into
        [:or]
        (map schema->malli)
        (:anyOf js-schema))

      (-keys :allOf)
      (into
        [:and]
        (map schema->malli)
        (:allOf js-schema))

      (-keys :not) [:not (schema->malli (:not js-schema))]

      (-keys :$ref) ($ref (:$ref js-schema))

      :else
      (throw (ex-info "Not supported" {:js-schema js-schema})))))

(defn properties->malli [{:keys [required]} [k v]]
  (cond-> [k]
    (nil? (required k)) (conj {:optional true})
    true (conj (schema->malli v))))

(defn object->malli [v]
  (let [required (into #{}
                       (map keyword)
                       (:required v))
        closed? (false? (:additionalProperties v))]
    (m/schema (cond-> [:map]
                closed? (conj {:closed :true})
                true (into
                       (map (partial properties->malli {:required required}))
                       (:properties v))))))

(defmethod type->malli "string" [p] string?)
(defmethod type->malli "integer" [p] int?)
(defmethod type->malli "number" [p]
  ;; TODO support decimal/double
  number?)
(defmethod type->malli "boolean" [p] boolean?)
(defmethod type->malli "null" [p] nil?)
(defmethod type->malli "object" [p] (object->malli p))
(defmethod type->malli "array" [p] (let [items (:items p)]
                                     (cond
                                       (vector? items) (into [:tuple]
                                                        (map schema->malli)
                                                        items)
                                       (map? items) [:vector (schema->malli items)]
                                       :else (throw (ex-info "Can't produce malli schema" {:p p})))))

(defn json-schema-document->malli [obj]
  [:schema {:registry (into {}
                            (map-values schema->malli)
                            (:definitions obj))}
   (schema->malli obj)])
