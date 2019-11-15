(ns malli.provider-test
  (:require [clojure.test :refer [deftest testing is]]
            [malli.provider :as mp]
            [malli.core :as m]))

(def expectations
  [[int? [1 2 3]]
   [keyword? [:kikka :kukka]]
   [qualified-keyword? [::kikka ::kukka]]
   [uuid? [#?(:clj (java.util.UUID/randomUUID) :cljs (random-uuid))]]
   [inst? [#?(:clj (java.util.Date.) :cljs (js/Date.))]]

   [[:vector keyword?] [[:kikka] [:kukka :kakka]]]
   [[:list symbol?] [(list 'kikka) (list 'kukka 'kakka)]]
   [[:sequential symbol?] [(seq ['kikka]) (seq ['kikka 'kakka])]]
   [[:set string?] [#{"a" "b"} #{"c"}]]
   [[:vector [:list [:set int?]]] [[(list #{1})]]]

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
  (doseq [[schema samples] expectations]
    (is (= (m/form schema) (m/form (mp/provide samples))))))
