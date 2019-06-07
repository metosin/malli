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

(defprotocol Schema)

;; Expands the given schema data to instances of Schema
(defn expand [registry schema])

;; Core schemas are defined as types of Schema
(def -map (reify Schema '...))
(def -and (reify Schema '...))

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
;;; TBD Examples of schema format transformation to Json schema
;;  - It should be possible to generate Json-schema metaschema from our core

;;; Larger schema example

;; Creating an inlined model inspired from schema.org Person
(def Person
  [:schema
   ;; Optional base URI
   {:json-schema/id "urn:"}

   [:map {:id :my.org/Person ;; Json-schema URI would be: 'urn:my.org/Person'
          ;; Merges referenced schemas
          :ref [:my.org/Thing]
          :description "A schema.org Person"
          :schema.org/type "Person"}
    ;; Optional key - sugar for `:map :opt-keys`
    [[:opt :email] :string]
    [:gender [:or :my/gender [:and :string :not-empty]]]
    ;; References another schema (should it be wrapped on `:ref`?)
    [:parent [:coll-of :my.org/Person]]
    [:height :number]]

   [:defs ;; Defines inline schemas which are only referenced

    ;; Thing
    [:map {:id :my.org/Thing
           :title "Thing"
           :description "A root of all schema.org entities"
           :schema.org/type "http://schema.org/Thing"
           ;; Can override json-schema generation from :id?
           :json-schema/id "my.org/Thingy"}
     [:identifier [:or :uuid :url [:and :string :not-empty]]]]

    ;; ImageObject
    [:and {:id :my.org/ImageObject
           :schema.org/type "http://schema.org/ImageObject"
           :json-schema/id "http://my.org/ImageObject"}
     [:map {:closed true
            :ref [:my.org/Thing]
            :opt-keys [:caption :thumbnail]}
            ;; All keys optional shortcut?
            ;; `:opt-keys :all`}
      [:caption :string]
      [:thumbnail [:coll-of :my/ImageObject]]
      [:contentUrl :url]
      [:uploadDate :date]]
     ;; Generic kv-schema
     [:map-of
      [[:and :keyword :camel-case] :any]]]

    ;; Gender enum
    [:enum {:id :my.org/gender
            :schema.org/type "http://schema.org/GenderType"}
     [:male :female]]]])

