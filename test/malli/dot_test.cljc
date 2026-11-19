(ns malli.dot-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [malli.core :as m]
            [malli.dot :as md]
            [malli.registry :as mr]))

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
       (md/transform Order)))

  (is (trimmed=
       "digraph {
          node [shape=\"record\", style=\"filled\", color=\"#000000\"]
          edge [dir=\"back\", arrowtail=\"none\"]

          \":malli.dot/schema\" [label=\"{:malli.dot/schema|[:and int? [:\\< 100]]\\l}\", fillcolor=\"#fff0cd\"]
        }"
       (md/transform [:and int? [:< 100]]))))

(deftest options-registry-test
  (let [registry (mr/composite-registry
                  m/default-registry
                  (mr/registry {"Curve" [:enum :curve25519 :p256]
                                "Key" [:map [:curve "Curve"] [:value [:map [:x int?]]]]}))]

    (testing "external schemas referenced from the options registry are shown as entities"
      (is (trimmed=
           "digraph {
              node [shape=\"record\", style=\"filled\", color=\"#000000\"]
              edge [dir=\"back\", arrowtail=\"none\"]

              \":malli.dot/schema\" [label=\"{:malli.dot/schema|Curve\\l}\", fillcolor=\"#fff0cd\"]
              \"Curve\" [label=\"{Curve|[:enum :curve25519 :p256]\\l}\", fillcolor=\"#fff0cd\"]

              \":malli.dot/schema\" -> \"Curve\" [arrowtail=\"odiamond\"]
            }"
           (md/transform "Curve" {:registry registry}))))

    (testing "external schemas are normalized and their references followed"
      (is (trimmed=
           "digraph {
              node [shape=\"record\", style=\"filled\", color=\"#000000\"]
              edge [dir=\"back\", arrowtail=\"none\"]

              \":malli.dot/schema\" [label=\"{:malli.dot/schema|:key Key\\l}\", fillcolor=\"#fff0cd\"]
              \"Curve\" [label=\"{Curve|[:enum :curve25519 :p256]\\l}\", fillcolor=\"#fff0cd\"]
              \"Key\" [label=\"{Key|:curve Curve\\l:value Key$Value\\l}\", fillcolor=\"#fff0cd\"]
              \"Key$Value\" [label=\"{Key$Value|:x int?\\l}\", fillcolor=\"#e6caab\"]

              \":malli.dot/schema\" -> \"Key\" [arrowtail=\"odiamond\"]
              \"Key\" -> \"Curve\" [arrowtail=\"odiamond\"]
              \"Key\" -> \"Key$Value\" [arrowtail=\"diamond\"]
            }"
           (md/transform [:map [:key "Key"]] {:registry registry}))))

    (testing "recursive external schemas terminate"
      (let [registry (mr/composite-registry
                      m/default-registry
                      (mr/registry {"Node" [:map [:next [:maybe [:ref "Node"]]]]}))
            dot (md/transform [:map [:node "Node"]] {:registry registry})]
        (is (str/includes? dot "\"Node\" [label=\"{Node|:next [:maybe [:ref"))
        (is (str/includes? dot "\"Node\" -> \"Node\""))))))
