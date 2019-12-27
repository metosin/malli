(ns malli.util-test
  (:require [clojure.test :refer [deftest testing is are]]
            [malli.core :as m]
            [malli.util :as mu]))

(deftest merge-test
  (are [?s1 ?s2 expected]
    (= true (m/equals expected (mu/merge ?s1 ?s2)))

    int? int? int?
    int? pos-int? pos-int?
    int? nil int?
    nil pos-int? pos-int?

    [:map [:x int?]]
    [:map [:x {:optional true} pos-int?]]
    [:map [:x {:optional true} pos-int?]]

    [:map [:x {:optional true} int?]]
    [:map [:x pos-int?]]
    [:map [:x pos-int?]]

    [:map [:x {:optional false} int?]]
    [:map [:x {:optional true} pos-int?]]
    [:map [:x {:optional true} pos-int?]]

    [:map {:title "parameters"}
     [:parameters
      [:map
       [:query-params {:title "query1", :description "first"}
        [:map [:x int?]]]]]]
    [:map {:description "description"}
     [:parameters
      [:map
       [:query-params {:title "query2", :summary "second"}
        [:map [:x string?] [:y int?]]]
       [:body-params
        [:map [:z int?]]]]]]
    [:map {:title "parameters", :description "description"}
     [:parameters
      [:map
       [:query-params {:title "query2", :description "first", :summary "second"}
        [:map [:x string?] [:y int?]]]
       [:body-params
        [:map [:z int?]]]]]]))

(deftest union-test
  (are [?s1 ?s2 expected]
    (= true (m/equals expected (mu/union ?s1 ?s2)))

    int? int? int?
    int? pos-int? [:or int? pos-int?]
    int? nil int?
    nil pos-int? pos-int?

    [:map [:x int?]]
    [:map [:x {:optional true} pos-int?]]
    [:map [:x [:or int? pos-int?]]]

    [:map [:x int?]]
    [:map [:x {:optional true} pos-int?]]
    [:map [:x [:or int? pos-int?]]]

    [:map [:x {:optional true} int?]]
    [:map [:x {:optional true} pos-int?]]
    [:map [:x {:optional true} [:or int? pos-int?]]]

    [:map [:x {:optional false} int?]]
    [:map [:x {:optional true} pos-int?]]
    [:map [:x [:or int? pos-int?]]]

    [:map {:title "parameters"}
     [:parameters
      [:map
       [:query-params {:title "query1", :description "first"}
        [:map [:x int?]]]]]]
    [:map {:description "description"}
     [:parameters
      [:map
       [:query-params {:title "query2", :summary "second"}
        [:map [:x string?] [:y int?]]]
       [:body-params
        [:map [:z int?]]]]]]
    [:map {:title "parameters", :description "description"}
     [:parameters
      [:map
       [:query-params {:title "query2", :description "first", :summary "second"}
        [:map [:x [:or int? string?]] [:y int?]]]
       [:body-params
        [:map [:z int?]]]]]]))

(mu/merge
  [:map
   [:name string?]
   [:description string?]
   [:address
    [:map
     [:street string?]
     [:country [:enum "finland" "poland"]]]]]
  [:map
   [:description {:optional true} string?]
   [:address
    [:map
     [:country string?]]]])
;[:map
; [:name string?]
; [:description {:optional true} string?]
; [:address [:map
;            [:street string?]
;            [:country string?]]]]

(mu/union
  [:map
   [:name string?]
   [:description string?]
   [:address
    [:map
     [:street string?]
     [:country [:enum "finland" "poland"]]]]]
  [:map
   [:description {:optional true} string?]
   [:address
    [:map
     [:country string?]]]])
;[:map
; [:name string?]
; [:description string?]
; [:address [:map
;            [:street string?]
;            [:country [:or [:enum "finland" "poland"] string?]]]]]
