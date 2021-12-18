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

   ;; normal maps without type-hint
   [[:map
     [:a [:map
          [:b int?]
          [:c int?]]]
     [:b [:map
          [:b int?]
          [:c int?]]]
     [:c [:map
          [:b int?]]]
     [:d :nil]]
    [{:a {:b 1, :c 2}
      :b {:b 2, :c 1}
      :c {:b 3}
      :d nil}]]

   ;; :map-of type-hint
   [[:map-of keyword? [:maybe [:map
                               [:b int?]
                               [:c {:optional true} int?]]]]
    [^{::mp/hint :map-of}
     {:a {:b 1, :c 2}
      :b {:b 2, :c 1}
      :c {:b 3}
      :d nil}]]

   ;; too few samples for :map-of
   [[:map
     ["1" [:map [:name string?]]]
     ["2" [:map [:name string?]]]]
    [{"1" {:name "1"}
      "2" {:name "2"}}]]

   ;; explicit sample count for :map-of
   [[:map-of string? [:map [:name string?]]]
    [{"1" {:name "1"}
      "2" {:name "2"}}]
    {::mp/map-of-threshold 2}]

   ;; implicit sample count for :map-of
   [[:map-of string? [:map [:name string?]]]
    [{"1" {:name "1"}
      "2" {:name "2"}
      "3" {:name "3"}}]]

   ;; tuple-like with too few elements
   [[:vector some?]
    [[1 "kikka" true]
     [2 "kukka" true]]]

   ;; tuple-like with enough samples
   [[:tuple int? string? boolean?]
    [[1 "kikka" true]
     [2 "kukka" true]]
    {::mp/tuple-threshold 2}]

   ;; tuple-like with enough samples
   [[:tuple int? string? boolean?]
    [[1 "kikka" true]
     [2 "kukka" true]
     [3 "kakka" true]]]

   ;; tuple-like with non-coherent data
   [[:vector some?]
    [[1 "kikka" true]
     [2 "kukka" true]
     [3 "kakka" "true"]]]

   ;; a homogenous hinted tuple
   [[:tuple int? string? boolean?]
    [^{::mp/hint :tuple} [1 "kikka" true]
     [2 "kukka" true]]]

   ;; a hererogenous hinted tuple
   [[:tuple int? string? some?]
    [^{::mp/hint :tuple} [1 "kikka" true]
     [2 "kukka" "true"]]]

   ;; invalid hinted tuple
   [[:vector some?]
    [^{::mp/hint :tuple} [1 "kikka" true]
     [2 "kukka" true "invalid tuple"]]]

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
