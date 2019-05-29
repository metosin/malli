(ns malli.design-draft
  (:import (clojure.lang IPersistentMap)))


;;; malli schemas

;; Core schemas are not namespace qualified keywords to make using them user
;; friendly.

;; Schemas are written using Hiccup style vectors, where order is significant.

;; Every schema definition can have additional options-map after schema name.
;; Options-map can have arbitrary amount of data, with some keys having special
;; meaning, like below, which describes a closed map.

[:map {:closed true, :description "My closed map"}]

;; Schema definitions live in registry. Malli provides a core registry for
;; all the essential schemas. Registry can be extended on the runtime and it's
;; possible to have as many registries one needs.

;; The actual registry definitions and format is wip, but could be something
;; like below.

;; Compiles given definition to Schema
(declare compile)
(declare Schema)

(def -map (compile {:id :map
                    :type IPersistentMap
                    :title "Map"
                    :json-schema/type "object"
                    :valid-fn (fn [data])}))

(def -and (compile {:id :and
                    ;; Does this make any sense?
                    :json-schema/op "allOf"}))

;; or create Schema directly
(def -map (reify Schema '...))

(def core-registry
  {:map -map
   :and -and})

;; We could expose core schemas as vars as well for having development time
;; documentation etc.
(def ^{:doc "A map"} map :map)


;;; TBD List core schemas
;;; TBD List main functions operating on schemas
;;; TBD Examples of validation and validation error messages
;;; TBD Examples of coercion
;;; TBD Examples of schema format transformation eg. to Json schema

;;; Larger schema example

;; Creating an inlined model inspired from schema.org Person
(def example1
  [:schema
   [:map {:id :my/Thing
          :title "A Thing"
          :description
          :schema.org/type "http://schema.org/ImageObject"}
    :identifier [:or :uuid :url [:and :string :not-empty]]]
   [:and {:id :my/ImageObject
          :schema.org/type "http://schema.org/ImageObject"
          :json-schema/id "http://my.org/ImageObject"}
    [:map {:closed true
           ;; Use keys from another map schemas?
           :merge-keys [:my/Thing]
           ;; Optional keys as map option?
           :opt-keys [:caption :thumbnail]}
           ;; All keys optional shortcut?
           ;; `:opt-keys :all`}
     :caption :string
     :thumbnail [:coll-of :my/ImageObject]
     :contentUrl :url
     :uploadDate :date]
    [:map-of
     [[:and :keyword :camel-case] :any]]]
   [:enum {:id :my/gender
           :schema.org/type "http://schema.org/GenderType"}
    [:male :female]]
   [:map {:id :my/Person
          :merge-keys [:my/Thing]
          :json-schema/id "http://my.org/Person"
          :description "A schema.org Person"
          :schema.org/type "Person"}
    ;; Optional key as key operator
    [:opt :email] :string
    ;; Optional key as key opt-map
    :gender {:opt true} [:or :my/gender [:and :string :not-empty]]
    :parent [:coll-of :my/Person]
    :height {:opt true} :number]])
