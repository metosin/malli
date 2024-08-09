(ns malli.distributive-test
  (:require [clojure.test :refer [are deftest is testing]]
            [malli.core :as m]
            [malli.impl.util :as miu]
            [malli.registry :as mr]
            [malli.transform :as mt]
            [malli.util :as mu]))

(def options {:registry (merge (mu/schemas) (m/default-schemas))})

(defn dist [s]
  (m/form (m/deref s options)))

(deftest distributive-test
  (is (= (dist
           [:merge
            [:map [:x :int]]
            [:multi {:dispatch :y}
             [1 [:map [:y [:= 1]]]]
             [2 [:map [:y [:= 2]]]]]])
         [:multi {:dispatch :y}
          [1 [:map [:x :int] [:y [:= 1]]]]
          [2 [:map [:x :int] [:y [:= 2]]]]]))
  (is (= (dist
           [:merge
            [:multi {:dispatch :y}
             [1 [:map [:y [:= 1]]]]
             [2 [:map [:y [:= 2]]]]]
            [:map [:x :int]]])
         [:multi {:dispatch :y}
          [1 [:map [:y [:= 1]] [:x :int]]]
          [2 [:map [:y [:= 2]] [:x :int]]]]))

  (is (= (dist
           [:merge
            [:map [:x :int]]
            [:orn
             [1 [:map [:y [:= 1]]]]
             [2 [:map [:y [:= 2]]]]]])
         [:orn
          [1 [:map [:x :int] [:y [:= 1]]]]
          [2 [:map [:x :int] [:y [:= 2]]]]]))
  (is (= (dist
           [:merge
            [:orn
             [1 [:map [:y [:= 1]]]]
             [2 [:map [:y [:= 2]]]]]
            [:map [:x :int]]])
         [:orn
          [1 [:map [:y [:= 1]] [:x :int]]]
          [2 [:map [:y [:= 2]] [:x :int]]]]))
  (is (= (dist
           [:merge
            [:map [:x :int]]
            [:or
             [:map [:y [:= 1]]]
             [:map [:y [:= 2]]]]])
         [:or
          [:map [:x :int] [:y [:= 1]]]
          [:map [:x :int] [:y [:= 2]]]]))
  (is (= (dist
           [:merge
            [:or
             [:map [:y [:= 1]]]
             [:map [:y [:= 2]]]]
            [:map [:x :int]]])
         [:or
          [:map [:y [:= 1]] [:x :int]]
          [:map [:y [:= 2]] [:x :int]]])))
