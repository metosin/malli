(ns malli.distributive-test
  (:require [clojure.test :refer [are deftest is testing]]
            [malli.core :as m]
            [malli.impl.util :as miu]
            [malli.registry :as mr]
            [malli.transform :as mt]
            [malli.util :as mu]))
(comment
  (m/type
    [:schema
     [:multi {:dispatch :y}
      [1 [:map [:y [:= 1]]]]
      [2 [:map [:y [:= 2]]]]]]))

(def options {:registry (merge (mu/schemas) (m/default-schemas))})

(defn dist [s]
  (m/form (m/deref s options)))

(deftest distributive-multi-test
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
            [:map [:x :int]]
            [:schema
             [:multi {:dispatch :y}
              [1 [:map [:y [:= 1]]]]
              [2 [:map [:y [:= 2]]]]]]])
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
            [:multi {:dispatch :y}
             [1 [:map [:y [:= 1]]]]
             [2 [:map [:y [:= 2]]]]]
            [:multi {:dispatch :y}
             [3 [:map [:y [:= 3]]]]
             [4 [:map [:y [:= 4]]]]]])
         [:multi {:dispatch :y}
          [1 [:multi {:dispatch :y}
              [3 [:map [:y [:= 3]]]]
              [4 [:map [:y [:= 4]]]]]]
          [2 [:multi {:dispatch :y}
              [3 [:map [:y [:= 3]]]]
              [4 [:map [:y [:= 4]]]]]]])))

;;FIXME
(deftest distribute-registry-test
  (is (= (dist
           [:merge
            [:map {:registry {::y boolean?}}
             [:y :int]]
            [:multi {:dispatch :z
                     :registry {::y :int}}
             [1 [:map [:z :int]]]
             [2 [:map [:z :int]]]]])
         [:multi {:dispatch :z
                  ;;problem!! shadowed by maps
                  :registry {::y :int}}
          [1 [:map {:registry {::y 'boolean?}}
              [:y :int]
              [:z :int]]]
          [2 [:map {:registry {::y 'boolean?}}
              [:y :int] [:z :int]]]])))
