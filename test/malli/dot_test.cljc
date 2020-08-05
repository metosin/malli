(ns malli.dot-test
  (:require [clojure.test :refer [deftest testing is]]
            [clojure.string :as str]
            [malli.dot :as md]))

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

(deftest transform-test

  (is (trimmed=
        "digraph {
           node [shape=\"record\", style=\"filled\", color=\"#000000\"]
           edge [dir=\"back\", arrowtail=\"none\"]

           \":malli.dot/schema\" [label=\"{:malli.dot/schema|[:enum \\{:title \\\"enum\\\"\\}  \\\"S\\\" \\\"M\\\" \\\"L\\\"]\\l}\", fillcolor=\"#fff0cd\"]
         }"
        (md/transform
          [:enum {:title "enum"} "S" "M" "L"])))

  (is (trimmed=
        "digraph {
           node [shape=\"record\", style=\"filled\", color=\"#000000\"]
           edge [dir=\"back\", arrowtail=\"none\"]

           \":malli.dot/schema\" [label=\"{:malli.dot/schema|:x :string\\l}\", fillcolor=\"#fff0cd\"]
         }"
        (md/transform
          [:map {:x 1}
           [:x {:x 1} :string]])))

  (is (trimmed=
        "digraph {
           node [shape=\"record\", style=\"filled\", color=\"#000000\"]
           edge [dir=\"back\", arrowtail=\"none\"]

           \"Burger\" [label=\"{Burger|:name string?\\l:description string?\\l:origin [:maybe \\\"Country\\\"]\\l:price pos-int?\\l}\", fillcolor=\"#fff0cd\"]
           \"Country\" [label=\"{Country|:name [:enum :FI :PO]\\l:neighbors [:vector [:ref \\\"Country\\\"]]\\l}\", fillcolor=\"#fff0cd\"]
           \"Order\" [label=\"{Order|:lines [:vector \\\"OrderLine\\\"]\\l:delivery Order$Delivery\\l}\", fillcolor=\"#fff0cd\"]
           \"Order$Delivery\" [label=\"{Order$Delivery|:delivered boolean?\\l:address Order$Delivery$Address\\l}\", fillcolor=\"#e6caab\"]
           \"Order$Delivery$Address\" [label=\"{Order$Delivery$Address|:street string?\\l:zip int?\\l:country Country\\l}\", fillcolor=\"#e6caab\"]
           \"OrderLine\" [label=\"{OrderLine|:burger Burger\\l:amount int?\\l}\", fillcolor=\"#fff0cd\"]

           \"Burger\" -> \"Country\" [arrowtail=\"odiamond\"]
           \"Country\" -> \"Country\" [arrowtail=\"odiamond\"]
           \"Order\" -> \"OrderLine\" [arrowtail=\"odiamond\"]
           \"Order\" -> \"Order$Delivery\" [arrowtail=\"diamond\"]
           \"Order$Delivery\" -> \"Order$Delivery$Address\" [arrowtail=\"diamond\"]
           \"Order$Delivery$Address\" -> \"Country\" [arrowtail=\"odiamond\"]
           \"OrderLine\" -> \"Burger\" [arrowtail=\"odiamond\"]
         }"
        (md/transform Order))))
