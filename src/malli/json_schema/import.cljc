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

(defn v->malli [v]
  (if-let [ref- (:$ref v)]
    ($ref ref-)
    (let [required (into #{}
                       (map keyword)
                       (:required v))
        closed? (false? (:additionalProperties v))]
    (m/schema (cond-> [:map]
                closed? (conj {:closed :true})
                true (into
                       (map (partial properties->malli {:required required}))
                       (:properties v)))))))

(defn kv->malli [[k v]]
  [k (v->malli v)])

(defmethod type->malli "string" [p] string?)
(defmethod type->malli "integer" [p] int?)
(defmethod type->malli "number" [p] number?)
(defmethod type->malli "boolean" [p] boolean?)
(defmethod type->malli "null" [p] nil?)
(defmethod type->malli "object" [p] (v->malli p))
(defmethod type->malli "array" [p] (let [items (:items p)]
                                     (cond
                                       (vector? items) (into [:tuple]
                                                        (map type->malli)
                                                        items)
                                       (map? items) [:vector (type->malli items)]
                                       :else (throw (ex-info "Can't produce malli schema" {:p p})))))

(defn parse-top-level [js-schema]
  (let [keys- (set (keys js-schema))]
    (cond
      (keys- :oneOf) (into [:or]
                           (map v->malli)
                           (:oneOf js-schema)))))

(defn ->malli [obj]
  [:schema {:registry (into {}
                            (map kv->malli)
                            (:definitions obj))}
   (parse-top-level (dissoc obj :definitions))])

(defn properties->malli [{:keys [required]} [k v]]
  (cond-> [k]
    (nil? (required k)) (conj {:optional true})
    true (conj (type->malli v))))

(defn $ref [v]
  ;; TODO to be improved
  (keyword (last (str/split v #"/"))))

(defn v->malli [v]
  (if-let [ref- (:$ref v)]
    ($ref ref-)
    (let [required (into #{}
                       (map keyword)
                       (:required v))
        closed? (false? (:additionalProperties v))]
    (m/schema (cond-> [:map]
                closed? (conj {:closed :true})
                true (into
                       (map (partial properties->malli {:required required}))
                       (:properties v)))))))

(defn kv->malli [[k v]]
  [k (v->malli v)])

(defmethod type->malli "string" [p] string?)
(defmethod type->malli "integer" [p] int?)
(defmethod type->malli "number" [p]
  ;; TODO support decimal/double
  number?)
(defmethod type->malli "boolean" [p] boolean?)
(defmethod type->malli "null" [p] nil?)
(defmethod type->malli "object" [p] (v->malli p))
(defmethod type->malli "array" [p] (let [items (:items p)]
                                     (cond
                                       (vector? items) (into [:tuple]
                                                        (map type->malli)
                                                        items)
                                       (map? items) [:vector (type->malli items)]
                                       :else (throw (ex-info "Can't produce malli schema" {:p p})))))

(defn parse-top-level [js-schema]
  (let [keys- (set (keys js-schema))]
    (cond
      (keys- :oneOf) (into [:or]
                           (map v->malli)
                           (:oneOf js-schema)))))

(defn ->malli [obj]
  [:schema {:registry (into {}
                            (map kv->malli)
                            (:definitions obj))}
   (parse-top-level (dissoc obj :definitions))])
