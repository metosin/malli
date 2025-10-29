(ns malli.json-schema.parse
  (:require [malli.core :as m]
            [malli.util :as mu]
            [clojure.set :as set]
            [clojure.string :as str]))

(def annotations #{:title :description :default :examples :example})

(defn annotations->properties [js-schema]
  (-> js-schema
      (select-keys annotations)
      (set/rename-keys {:examples    :json-schema/examples
                        :example     :json-schema/example
                        :title       :json-schema/title
                        :description :json-schema/description
                        :default     :json-schema/default})))

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
    (mu/update-properties
     (cond
       (-keys :type) (type->malli js-schema)

       (-keys :enum) (into [:enum]
                           (:enum js-schema))

       (-keys :const) [:= (:const js-schema)]

       ;; Aggregates
       (-keys :oneOf) (into
                       ;; TODO Figure out how to make it exclusively select o schema
                       ;; how about `m/multi`?
                       [:or]
                       (map schema->malli)
                       (:oneOf js-schema))

       (-keys :anyOf) (into
                       [:or]
                       (map schema->malli)
                       (:anyOf js-schema))

       (-keys :allOf) (into
                       [:and]
                       (map schema->malli)
                       (:allOf js-schema))

       (-keys :not) [:not (schema->malli (:not js-schema))]

       (-keys :$ref) ($ref (:$ref js-schema))

       (empty -keys) :any

       :else (throw (ex-info "Not supported" {:json-schema js-schema
                                              :reason ::schema-type})))
     merge
     (annotations->properties js-schema))))

(defn properties->malli [required [k v]]
  (cond-> [k]
    (nil? (required k)) (conj {:optional true})
    true (conj (schema->malli v))))

(defn- prop-size [pred?] (fn [-map] (pred? (count (keys -map)))))
(defn- min-properties [-min] (prop-size (partial <= -min)))
(defn- max-properties [-max] (prop-size (partial >= -max)))

(defn with-min-max-properties-size [malli v]
  (let [predicates [(some->> v
                             (:minProperties)
                             (min-properties)
                             (conj [:fn]))
                    (some->> v
                             (:maxProperties)
                             (max-properties)
                             (conj [:fn]))]]
    (cond->> malli
      (some some? predicates)
      (conj (into [:and]
                  (filter some?)
                  predicates)))))

(defn object->malli [{:keys [additionalProperties] :as v}]
  (let [required (into #{}
                       ;; TODO Should use the same fn as $ref
                       (map keyword)
                       (:required v))
        closed? (false? additionalProperties)]
    (m/schema (-> (if (:type additionalProperties)
                    (let [va (schema->malli additionalProperties)] [:map-of va va])
                    [:map])
                  (cond-> closed? (conj {:closed :true}))
                  (into
                   (map (partial properties->malli required))
                   (:properties v))
                  (with-min-max-properties-size v)))))

(defmethod type->malli "string" [{:keys [pattern minLength maxLength enum format]}]
  ;; `format` metadata is deliberately not considered.
  ;; String enums are stricter, so they're also implemented here.
  (cond
    pattern [:re pattern]
    enum (into [:enum] enum)
    (= format "uuid") :uuid
    :else (let [attrs (cond-> nil
                        minLength (assoc :min minLength)
                        maxLength (assoc :max maxLength))]
            (if attrs
              [:string attrs]
              :string))))

(defn- number->malli [{:keys [minimum maximum exclusiveMinimum exclusiveMaximum
                              multipleOf enum type]
                       :as schema}]
  (let [integer (= type "integer")
        implicit-double (or minimum maximum integer enum
                            (number? exclusiveMaximum) (number? exclusiveMinimum))
        maximum (if (number? exclusiveMaximum) exclusiveMaximum maximum)
        minimum (if (number? exclusiveMinimum) exclusiveMinimum minimum)]
    (cond-> (if integer [:int] [])
      (or minimum maximum) identity
      enum    (into [(into [:enum] enum)])
      maximum (into [[(if exclusiveMaximum :< :<=) maximum]])
      minimum (into [[(if exclusiveMinimum :> :>=) minimum]])
      (not implicit-double) (into [[:double]]))))

(defmethod type->malli "integer" [p]
  ;; TODO Implement multipleOf support
  (let [ranges-logic (number->malli p)]
    (if (> (count ranges-logic) 1)
      (into [:and] ranges-logic)
      (first ranges-logic))))

(defmethod type->malli "number" [{:keys [exclusiveMinimum exclusiveMaximum minimum maximum] :as p}]
  (let [ranges-logic (number->malli p)]
    (if (> (count ranges-logic) 1)
      (into [:and] ranges-logic)
      (first ranges-logic))))

(defmethod type->malli "boolean" [p] boolean?)
(defmethod type->malli "null" [p] :nil)
(defmethod type->malli "object" [p] (object->malli p))
(defmethod type->malli "array" [p] (let [items (:items p)]
                                     (cond
                                       (vector? items) (into [:tuple]
                                                             (map schema->malli)
                                                             items)
                                       (:uniqueItems p) [:set (schema->malli items)]
                                       (map? items) [:vector (schema->malli items)]
                                       :else (throw (ex-info "Not Supported" {:json-schema p
                                                                              :reason ::array-items})))))

(defmethod type->malli "file" [p]
  [:map {:json-schema {:type "file"}} [:file :any]])

(defmethod type->malli :default [{:keys [type] :as p}]
  (cond
    (vector? type) (into [:or] (map #(type->malli {:type %}) type))
    (and type (= 1 (count (keys p)))) {:json-schema/type type}
    :else
    (throw (ex-info "Not Supported" {:json-schema p
                                     :reason ::unparseable-type}))))

(defn json-schema-document->malli [obj]
  [:schema {:registry (into {}
                            (map-values schema->malli)
                            (:definitions obj))}
   (schema->malli obj)])
