(ns malli.provider-test
  (:require [clojure.test :refer [deftest testing is]]
            [malli.provider :as mp]
            [malli.core :as m])
  #?(:clj (:import (java.util UUID Date))))

(def expectations
  [[int? [1 2 3]]
   [keyword? [:kikka :kukka]]
   [qualified-keyword? [::kikka ::kukka]]
   [uuid? [#?(:clj (UUID/randomUUID) :cljs (random-uuid))]]
   [inst? [#?(:clj (Date.) :cljs (js/Date.))]]
   [any? []]

   [[:vector keyword?] [[:kikka] [:kukka :kakka]]]
   [[:sequential symbol?] [(seq ['kikka]) (seq ['kikka 'kakka])]]
   [[:set string?] [#{"a" "b"} #{"c"}]]
   [[:vector [:sequential [:set int?]]] [[(list #{1})]]]
   [[:vector any?] [[]]]

   [[:maybe int?] [1 nil 2 3]]
   [[:maybe some?] [1 nil 2 "some"]]
   [[:maybe [:map [:x int?]]] [{:x 1} nil]]
   [[:maybe [:or [:map [:x int?]] string?]] [{:x 1} nil "1"]]

   ;; :maybe and optional keys with :map-of
   [[:map-of keyword? [:maybe [:map [:b int?] [:c {:optional true} int?]]]]
    [{:a {:b 1, :c 2}
      :b {:b 2, :c 1}
      :c {:b 3}
      :d nil}]]
   [[:map [:a [:map
               [:b int?]
               [:c int?]]]
     [:b [:map
          [:b int?]
          [:c int?]]]
     [:c [:map
          [:b int?]]]
     [:d string?]]
    [{:a {:b 1, :c 2}
           :b {:b 2, :c 1}
           :c {:b 3}
           :d "whatever"}]]

   [[:map
     ["kikka" [:map [:name string?]]]
     ["kakka" [:map [:name string?]]]]
    [{"kikka" {:name "kikka"}
      "kakka" {:name "kakka"}}]]
   [[:map-of string? [:map [:name string?]]]
    [{"kikka" {:name "kikka"}
      "kukka" {:name "kukka"}}]
    {::mp/map-of-threshold 2}]
   [[:map-of string? [:map [:name string?]]]
    [{"kikka" {:name "kikka"}
      "kukka" {:name "kukka"}
      "kakka" {:name "kakka"}}]]

   [[:map
     [:id string?]
     [:tags [:set keyword?]]
     [:address
      [:map
       [:street string?]
       [:city string?]
       [:zip int?]
       [:lonlat [:vector double?]]]]
     [:description {:optional true} string?]] [{:id "Lillan"
                                                :tags #{:artesan :coffee :hotel}
                                                :address {:street "Ahlmanintie 29"
                                                          :city "Tampere"
                                                          :zip 33100
                                                          :lonlat [61.4858322, 23.7854658]}}
                                               {:id "Huber",
                                                :description "Beefy place"
                                                :tags #{:beef :wine :beer}
                                                :address {:street "Aleksis Kiven katu 13"
                                                          :city "Tampere"
                                                          :zip 33200
                                                          :lonlat [61.4963599 23.7604916]}}]]])

(deftest provider-test
  (doseq [[schema samples options] expectations]
    (is (= (m/form schema) (m/form (mp/provide samples options))))))
