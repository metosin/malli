(ns malli.distributive-test
  (:require [clojure.test :refer [are deftest is testing]]
            [malli.core :as m]
            [malli.impl.util :as miu]
            [malli.registry :as mr]
            [malli.transform :as mt]
            [malli.util :as mu]))

(def options {:registry (merge (mu/schemas) (m/default-schemas))})

(deftest distributive-test
  (is (= (m/form
           (m/deref
             [:merge
              [:map [:x :int]]
              [:multi {:dispatch :y}
               [1 [:map [:y [:= 1]]]]
               [2 [:map [:y [:= 2]]]]]]
             options))
         [:multi {:dispatch :y}
          [1 [:map [:x :int] [:y [:= 1]]]]
          [2 [:map [:x :int] [:y [:= 2]]]]]))
  (is (= (m/form
           (m/deref
             [:merge
              [:multi {:dispatch :y}
               [1 [:map [:y [:= 1]]]]
               [2 [:map [:y [:= 2]]]]]
              [:map [:x :int]]]
             options))
         [:multi {:dispatch :y}
          [1 [:map [:y [:= 1]] [:x :int]]]
          [2 [:map [:y [:= 2]] [:x :int]]]]))

  (is (= (m/form
           (m/deref
             [:merge
              [:map [:x :int]]
              [:orn
               [1 [:map [:y [:= 1]]]]
               [2 [:map [:y [:= 2]]]]]]
             options))
         [:orn
          [1 [:map [:x :int] [:y [:= 1]]]]
          [2 [:map [:x :int] [:y [:= 2]]]]]))
  (is (= (m/form
           (m/deref
             [:merge
              [:orn
               [1 [:map [:y [:= 1]]]]
               [2 [:map [:y [:= 2]]]]]
              [:map [:x :int]]]
             options))
         [:orn
          [1 [:map [:y [:= 1]] [:x :int]]]
          [2 [:map [:y [:= 2]] [:x :int]]]]))

  (is (= (m/form
           (m/deref
             [:merge
              [:map [:x :int]]
              [:or
               [:map [:y [:= 1]]]
               [:map [:y [:= 2]]]]]
             options))
         [:or
          [:map [:x :int] [:y [:= 1]]]
          [:map [:x :int] [:y [:= 2]]]]))
  (is (= (m/form
           (m/deref
             [:merge
              [:or
               [:map [:y [:= 1]]]
               [:map [:y [:= 2]]]]
              [:map [:x :int]]]
             options))
         [:or
          [:map [:y [:= 1]] [:x :int]]
          [:map [:y [:= 2]] [:x :int]]])))
