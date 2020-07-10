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
           class Schema {
              :x :string
           }"
        (mm/class-diagram
          [:map {:x 1}
           [:x {:x 1} :string]])))

  (is (trimmed=
        "classDiagram
           class Burger {
             :description string?
             :name string?
             :origin [:maybe Country]
             :price pos-int?
           }
           class Country {
             :name [:enum :FI :PO]
             :neighbors [:vector [:ref Country]]
           }
           class Order {
             :delivery Order_Delivery
             :lines [:vector OrderLine]
           }
           class OrderLine {
             :amount int?
             :burger Burger
           }
           class Order_Delivery {
             :address Order_Delivery_Address
             :delivered boolean?
           }
           class Order_Delivery_Address {
             :country Country
             :street string?
             :zip int?
           }
           Burger o-- Country
           Country o-- Country
           Order o-- OrderLine
           Order *-- Order_Delivery
           OrderLine o-- Burger
           Order_Delivery *-- Order_Delivery_Address
           Order_Delivery_Address o-- Country"
        (mm/class-diagram Order))))
