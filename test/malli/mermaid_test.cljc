(ns malli.mermaid-test
  (:require [clojure.test :refer [deftest testing is are]]
            [malli.mermaid :as mm]
            [clojure.string :as str]))

(def Order
  [:schema
   {:registry {"Country" [:map
                          [:name [:enum :FI :PO]]
                          [:neighbors [:vector [:ref "Country"]]]]
               "Burger" [:map
                         [:name string?]
                         [:description {:optional true} string?]
                         [:origin [:maybe "Country"]]
                         [:price pos-int?]]
               "OrderLine" [:map
                            [:burger "Burger"]
                            [:amount int?]]
               "Order" [:map
                        [:lines [:vector "OrderLine"]]
                        [:delivery [:map
                                    [:delivered boolean?]
                                    [:address [:map
                                               [:street string?]
                                               [:zip int?]
                                               [:country "Country"]]]]]]}}
   "Order"])

(defn trimmed= [s1 s2]
  (letfn [(trim [x] (str/trim (str/replace x #"\s+" " ")))]
    (= (trim s1) (trim s2))))

(deftest mermaid-test
  (is (trimmed=
        "classDiagram
           class Country {
             :name [:enum :FI :PO]
             :neighbors [:vector [:ref Country]]
           }
           class Burger {
             :name string?
             :description string?
             :origin [:maybe Country]
             :price pos-int?
           }
           class OrderLine {
             :burger Burger
             :amount int?
           }
           class Order {
             :lines [:vector OrderLine]
             :delivery Order_Delivery
           }
           class Order_Delivery_Address {
             :street string?
             :zip int?
             :country Country
           }
           class Order_Delivery {
             :delivered boolean?
             :address Order_Delivery_Address
           }
           Country o-- Country
           Burger o-- Country
           OrderLine o-- Burger
           Order o-- OrderLine
           Order *-- Order_Delivery
           Order_Delivery_Address o-- Country
           Order_Delivery *-- Order_Delivery_Address"
        (mm/class-diagram Order))))
