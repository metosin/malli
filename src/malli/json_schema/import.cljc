(ns malli.json-schema.import
  (:require [malli.core :as m]))

(defmulti type->malli :type)

(defn properties->malli [{:keys [required]} [k v]]
  (cond-> [k]
    (nil? (required k)) (conj {:optional true})
    true (conj (type->malli v))))

(defn $ref [v]
  ;; TODO to be improved
  (keyword (last (str/split v #"/"))))

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

(defmethod type->malli nil [p]
  (when-let [-ref (:$ref v)]
    ($ref -ref))
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
                                                        (map type->malli)
                                                        items)
                                       (map? items) [:vector (type->malli items)]
                                       :else (throw (ex-info "Can't produce malli schema" {:p p})))))

(defn- parse-top-level [js-schema]
  (let [keys- (set (keys js-schema))]
    (cond
      (keys- :oneOf) (into [:or]
                           (map type->malli)
                           (:oneOf js-schema)))))

(defn- map-values
  ([-fn] (map (fn [[k v]] [k (-fn v)])))
  ([-fn coll] (sequence (map-values -fn) coll)))

(defn json-schema-document->malli [obj]
  [:schema {:registry (into {}
                            (map-values type->malli)
                            (:definitions obj))}
   (parse-top-level (dissoc obj :definitions))])
