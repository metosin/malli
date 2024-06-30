(ns malli.provider-test
  (:require [clojure.test :refer [deftest is]]
            [malli.core :as m]
            [malli.provider :as mp]
            [malli.transform :as mt])
  #?(:clj (:import (java.util UUID Date))))

(def expectations
  [[:int [1 2 3]]
   [:keyword [:kikka :kukka]]
   [:qualified-keyword [::kikka ::kukka]]
   [:uuid [#?(:clj (UUID/randomUUID) :cljs (random-uuid))]]
   [inst? [#?(:clj (Date.) :cljs (js/Date.))]]
   [:any []]

   [[:vector :keyword] [[:kikka] [:kukka :kakka]]]
   [[:sequential :symbol] [(seq ['kikka]) (seq ['kikka 'kakka])]]
   [[:set :string] [#{"a" "b"} #{"c"}]]
   [[:vector [:sequential [:set :int]]] [[(list #{1})]]]
   [[:vector :any] [[]]]

   [[:maybe :int] [1 nil 2 3]]
   [[:maybe :some] [1 nil 2 "some"]]
   [[:maybe [:map [:x :int]]] [{:x 1} nil]]
   [[:maybe [:or [:map [:x :int]] :string]] [{:x 1} nil "1"]]

   ;; normal maps without type-hint
   [[:map
     [:a [:map
          [:b :int]
          [:c :int]]]
     [:b [:map
          [:b :int]
          [:c :int]]]
     [:c [:map
          [:b :int]]]
     [:d :nil]]
    [{:a {:b 1, :c 2}
      :b {:b 2, :c 1}
      :c {:b 3}
      :d nil}]]

   ;; :map-of type-hint
   [[:map-of :keyword [:maybe [:map
                               [:b :int]
                               [:c {:optional true} :int]]]]
    [^{::mp/hint :map-of}
     {:a {:b 1, :c 2}
      :b {:b 2, :c 1}
      :c {:b 3}
      :d nil}]]

   ;; too few samples for :map-of
   [[:map
     ["1" [:map [:name :string]]]
     ["2" [:map [:name :string]]]]
    [{"1" {:name "1"}
      "2" {:name "2"}}]]

   ;; explicit sample count for :map-of
   [[:map-of :string [:map [:name :string]]]
    [{"1" {:name "1"}}
     {"2" {:name "2"}}]
    {::mp/map-of-threshold 2}]

   ;; tuple-like without options
   [[:vector :some]
    [[1 "kikka" true]
     [2 "kukka" true]
     [3 "kakka" false]]]

   ;; tuple-like with threshold not reached
   [[:vector :some]
    [[1 "kikka" true]
     [2 "kukka" true]
     [3 "kakka" false]]
    {::mp/tuple-threshold 4}]

   ;; tuple-like with threshold reached
   [[:tuple :int :string :boolean]
    [[1 "kikka" true]
     [2 "kukka" true]
     [3 "kakka" false]]
    {::mp/tuple-threshold 3}]

   ;; tuple-like with non-coherent data
   [[:vector :some]
    [[1 "kikka" true]
     [2 "kukka" true]
     [3 "kakka" "true"]]]

   ;; a homogenous hinted tuple
   [[:tuple :int :string :boolean]
    [^{::mp/hint :tuple} [1 "kikka" true]
     [2 "kukka" true]]]

   ;; a hererogenous hinted tuple
   [[:tuple :int :string :some]
    [^{::mp/hint :tuple} [1 "kikka" true]
     [2 "kukka" "true"]]]

   ;; invalid hinted tuple
   [[:vector :some]
    [^{::mp/hint :tuple} [1 "kikka" true]
     [2 "kukka" true "invalid tuple"]]]

   ;; value-decoders
   [[:map [:id :string]]
    [{:id "caa71a26-5fe1-11ec-bf63-0242ac130002"}
     {:id "8aadbf5e-5fe3-11ec-bf63-0242ac130002"}]]
   [[:map [:id :uuid]]
    [{:id "caa71a26-5fe1-11ec-bf63-0242ac130002"}
     {:id "8aadbf5e-5fe3-11ec-bf63-0242ac130002"}]
    {::mp/value-decoders {:string {:uuid mt/-string->uuid}}}]
   [[:map-of :uuid [:map [:id :uuid]]]
    [{"0423191a-5fee-11ec-bf63-0242ac130002" {:id "0423191a-5fee-11ec-bf63-0242ac130002"}}
     {"09e59de6-5fee-11ec-bf63-0242ac130002" {:id "09e59de6-5fee-11ec-bf63-0242ac130002"}}
     {"15511020-5fee-11ec-bf63-0242ac130002" {:id "15511020-5fee-11ec-bf63-0242ac130002"}}]
    {::mp/value-decoders {:string {:uuid mt/-string->uuid}}
     ::mp/map-of-threshold 3}]
   [[:map-of inst? :string]
    [{"1901-03-02T22:20:11.000Z" "123"}
     {"1902-04-03T22:20:11.000Z" "234"}
     {"1904-06-05T22:20:11.000Z" "456"}]
    {::mp/value-decoders {:string {'inst? mt/-string->date}}
     ::mp/map-of-threshold 3}]
   ;; value-hints
   [[:map [:name :string] [:gender [:enum "male" "female"]]]
    [{:name "Tommi", :gender (mp/-hinted "male" :enum)}
     {:name (mp/-hinted "Tiina" :string), :gender "female"}]]

   [[:map
     [:speed #?(:clj :float, :cljs :double)]
     [:position [:vector #?(:clj :float, :cljs :double)]]]
    [{:speed (float 1.5)
      :position [(float 300.33) (float 663.66)]}]]

   [[:map
     [:id :string]
     [:tags [:set :keyword]]
     [:address
      [:map
       [:street :string]
       [:city :string]
       [:zip :int]
       [:lonlat [:vector :double]]]]
     [:description {:optional true} :string]]
    [{:id "Lillan"
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
