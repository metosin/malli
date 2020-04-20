(ns malli.mermaid-test
  (:require [clojure.test :refer [deftest testing is are]]
            [malli.mermaid :as mm]
            [clojure.string :as str]))

(def Country
  [:map {:id "Country"}
   [:name [:enum :FI :PO]]
   [:neighbors any?]])

(def Burger
  [:map {:id "Burger"}
   [:name string?]
   [:description {:optional true} string?]
   [:origin [:maybe [:maybe Country]]]
   [:price pos-int?]])

(def OrderLine
  [:map {:id "OrderLine"}
   [:burger Burger]
   [:amount int?]])

(def Order
  [:map {:id "Order"}
   [:lines [:vector OrderLine]]
   [:delivery [:map
               [:delivered boolean?]
               [:address [:map
                          [:street string?]
                          [:zip int?]
                          [:country Country]]]]]])

(defn trimmed= [s1 s2]
  (letfn [(trim [x] (str/trim (str/replace x #"\s+" " ")))]
    (= (trim s1) (trim s2))))

(deftest mermaid-test
  (is (trimmed=
        "classDiagram
           class Country {
             + :name [:enum :FI :PO]
             + :neighbors any?
           }
           class Burger {
             + :name string?
             + :description string?
             + :origin Country
             + :price pos-int?
           }
           Burger o-- Country
           class OrderLine {
             + :burger Burger
             + :amount int?
           }
           OrderLine o-- Burger
           class OrderDeliveryAddress {
             <<embedded>>
             + :street string?
             + :zip int?
             + :country Country
           }
           OrderDeliveryAddress o-- Country
           class OrderDelivery {
             <<embedded>>
             + :delivered boolean?
             + :address OrderDeliveryAddress
           }
           OrderDelivery *-- OrderDeliveryAddress
           class Order {
             + :lines OrderLine
             + :delivery OrderDelivery
           }
           Order o-- OrderLine
           Order *-- OrderDelivery"
        (mm/class-diagram Order))))
