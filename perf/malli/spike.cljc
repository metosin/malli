(ns malli.spike
  (:require [malli.core :as m]))

;;
;; spike
;;

(m/validate int? 1)

(m/form [:and int?])
(m/schema [:and int? pos-int?])
(m/validate [:and int? [:and pos-int?]] 2)
(m/validate [:and int? pos-int?] 2)

(m/validate [:and int? [:or pos-int? neg-int?]] 0)

(m/schema [:map [:x int?]])

(m/schema [:map
           [:x boolean?]
           [[:opt :y] int?]
           [:z string?]])

(m/schema [:map
           [:x boolean?]
           [:y {:required false} int?]
           [:z string?]])

(def Age
  [:schema
   {:title "Age"
    :description "Age of a user"
    :json-schema/example 20}
   [:and int? [:not= 18]]])

(m/properties Age)

(m/validate Age 18)

;; schema-style
[:map
 {:closed true}
 [:x boolean?]
 [[:opt :y] int?]
 [[:req :z] string?]]

;; tuples
[:map
 [:x boolean?]
 [:opt :y int?]
 [:req :z string?]]

;; attrs
[:map
 [:x int?]
 [:y {:required false} int?]
 [:z {:required true} string?]]

;; varargs
[:map
 [:x int?]
 [:y int? :optional]
 [:z string? :required]]
