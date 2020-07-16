(ns malli.swagger
  (:require [malli.json-schema :as json-schema]
            [malli.core :as m]))

(defprotocol SwaggerSchema
  (-accept [this children options] "transforms schema to Swagger Schema"))

(defmulti accept (fn [name _schema _children _options] name) :default ::default)

(defmethod accept ::default [name schema children options] (json-schema/accept name schema children options))
(defmethod accept 'float? [_ _ _ _] {:type "number" :format "float"})
(defmethod accept 'double? [_ _ _ _] {:type "number" :format "double"})
(defmethod accept 'nil? [_ _ _ _] {})

(defmethod accept :and [_ _ children _] (assoc (first children) :x-allOf children))
(defmethod accept :or [_ _ children _] (assoc (first children) :x-anyOf children))
(defmethod accept :multi [_ _ children _] (let [cs (mapv last children)] (assoc (first cs) :x-anyOf cs)))

(defmethod accept :maybe [_ _ children {:keys [type in]}]
  (let [k (if (and (= type :parameter) (not= in :body)) :allowEmptyValue :x-nullable)]
    (assoc (first children) k true)))

(defmethod accept :tuple [_ _ children _] {:type "array" :items {} :x-items children})

(defn- -swagger-walker [schema _ children options]
  (let [p (m/properties schema)]
    (or (json-schema/unlift p :swagger)
        (json-schema/unlift p :json-schema)
        (merge (json-schema/select p)
               (if (satisfies? SwaggerSchema schema)
                 (-accept schema children options)
                 (accept (m/type schema) schema children options))
               (json-schema/unlift-keys p :json-schema)
               (json-schema/unlift-keys p :swagger)))))

;;
;; public api
;;

(defn transform
  ([?schema]
   (transform ?schema nil))
  ([?schema options]
   (m/walk ?schema -swagger-walker (assoc options ::m/walk-map-entries true))))
