(ns malli.json-schema.parse
  (:require [malli.core :as m]
            [malli.util :as mu]
            [clojure.set :as set]
            [clojure.string :as str]))

(def annotations #{:title :description :default :examples})

(defn annotations->properties [js-schema]
  (-> js-schema
      (select-keys annotations)
      (set/rename-keys {:examples :json-schema/examples})))

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

        (-keys :const) [:enum (:const js-schema)]

        ;; Aggregates
        (-keys :oneOf) (into
                         ;; TODO Figure out how to make it exclusively select o schema
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

(defn with-min-max-poperties-size [malli v]
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

(defn object->malli [v]
  (let [required (into #{}
                       ;; TODO Should use the same fn as $ref
                       (map keyword)
                       (:required v))
        closed? (false? (:additionalProperties v))]
    (m/schema (-> [:map]
                  (cond-> closed? (conj {:closed :true}))
                  (into
                    (map (partial properties->malli required))
                    (:properties v))
                  (with-min-max-poperties-size v)))))

(defmethod type->malli "string" [{:keys [pattern minLength maxLength enum]}]
  ;; `format` metadata is deliberately not considered.
  ;; String enums are stricter, so they're also implemented here.
  (cond
    pattern [:re pattern]
    enum [:and
          :string
          (into [:enum] enum)]
    :else [:string (cond-> {}
               minLength (assoc :min minLength)
               maxLength (assoc :max maxLength))]))

(defmethod type->malli "integer" [{:keys [minimum maximum exclusiveMinimum exclusiveMaximum multipleOf]
                                   :or {minimum Integer/MIN_VALUE
                                        maximum Integer/MAX_VALUE}}]
  ;; On draft 4, exclusive{Minimum,Maximum} is a boolean.
  ;; TODO Decide on whether draft 4 will be supported
  ;; TODO Implement exclusive{Minimum,Maximum} support
  ;; TODO Implement multipleOf support
  ;; TODO Wrap, when it makes sense, the values below with range checkers, i.e. [:< maximum]
  ;; TODO extract ranges logic and reuse with number
  (cond
    (pos? minimum) pos-int?
    (neg? maximum) neg-int?
    :else int?))

(defmethod type->malli "number" [p] number?)
(defmethod type->malli "boolean" [p] boolean?)
(defmethod type->malli "null" [p] nil?)
(defmethod type->malli "object" [p] (object->malli p))
(defmethod type->malli "array" [p] (let [items (:items p)]
                                     (cond
                                       (vector? items) (into [:tuple]
                                                        (map schema->malli)
                                                        items)
                                       (map? items) [:vector (schema->malli items)]
                                       :else (throw (ex-info "Not Supported" {:json-schema p
                                                                              :reason ::array-items})))))

(defn json-schema-document->malli [obj]
  [:schema {:registry (into {}
                            (map-values schema->malli)
                            (:definitions obj))}
   (schema->malli obj)])
