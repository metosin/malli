(ns malli.distributive-test
  (:require [clojure.test :refer [deftest is]]
            [malli.core :as m]
            [malli.generator :as mg]
            [malli.util :as mu]))

(def options {:registry (merge (mu/schemas) (m/default-schemas))})

(defn dist [s]
  (m/form (m/deref s options)))

(defn valid? [?schema value] (m/validate ?schema value options))

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
            [:map [:x :int]]
            [:map [:z :int]]])
         (dist
           [:merge
            [:merge
             [:multi {:dispatch :y}
              [1 [:map [:y [:= 1]]]]
              [2 [:map [:y [:= 2]]]]]
             [:map [:x :int]]]
            [:map [:z :int]]])
         [:multi {:dispatch :y}
          [1 [:map [:y [:= 1]] [:x :int] [:z :int]]]
          [2 [:map [:y [:= 2]] [:x :int] [:z :int]]]]))
  (is (= (dist
           [:merge
            [:multi {:dispatch :y}
             [1 [:map [:y [:= 1]]]]
             [2 [:map [:y [:= 2]]]]]
            [:map [:x :int]]
            [:map [:z :int]]
            [:multi {:dispatch :y}
             [3 [:map [:y [:= 3]]]]
             [4 [:map [:y [:= 4]]]]]])
         (dist
           [:merge
            [:merge
             [:merge
              [:multi {:dispatch :y}
               [1 [:map [:y [:= 1]]]]
               [2 [:map [:y [:= 2]]]]]
              [:map [:x :int]]]
             [:map [:z :int]]]
             [:multi {:dispatch :y}
              [3 [:map [:y [:= 3]]]]
              [4 [:map [:y [:= 4]]]]]])
         [:multi {:dispatch :y}
          [1 [:multi {:dispatch :y}
              [3 [:map [:y [:= 3]] [:x :int] [:z :int]]]
              [4 [:map [:y [:= 4]] [:x :int] [:z :int]]]]]
          [2 [:multi {:dispatch :y}
              [3 [:map [:y [:= 3]] [:x :int] [:z :int]]]
              [4 [:map [:y [:= 4]] [:x :int] [:z :int]]]]]]))
  (is (= (dist
           [:merge
            [:multi {:dispatch :y}
             [1 [:map [:y [:= 1]]]]
             [2 [:map [:y [:= 2]]]]]
            [:map [:x :int]]
            [:map [:z :int]]
            [:multi {:dispatch :a}
             [3 [:map [:a [:= 3]]]]
             [4 [:map [:a [:= 4]]]]]])
         [:multi {:dispatch :y}
          [1 [:multi {:dispatch :a}
              [3 [:map [:y [:= 1]] [:x :int] [:z :int] [:a [:= 3]]]]
              [4 [:map [:y [:= 1]] [:x :int] [:z :int] [:a [:= 4]]]]]]
          [2 [:multi {:dispatch :a}
              [3 [:map [:y [:= 2]] [:x :int] [:z :int] [:a [:= 3]]]]
              [4 [:map [:y [:= 2]] [:x :int] [:z :int] [:a [:= 4]]]]]]]))
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
              [4 [:map [:y [:= 4]]]]]]]))
  (is (= (dist
           [:merge
            [:multi {:dispatch :y}
             [1 [:map [:y [:= 1]]]]
             [2 [:map [:y [:= 2]]]]]
            [:multi {:dispatch :z}
             [3 [:map [:z [:= 3]]]]
             [4 [:map [:z [:= 4]]]]]])
         [:multi {:dispatch :y}
          [1 [:multi {:dispatch :z}
              [3 [:map [:y [:= 1]] [:z [:= 3]]]]
              [4 [:map [:y [:= 1]] [:z [:= 4]]]]]]
          [2 [:multi {:dispatch :z}
              [3 [:map [:y [:= 2]] [:z [:= 3]]]]
              [4 [:map [:y [:= 2]] [:z [:= 4]]]]]]])))

(deftest parse-distributive-multi-test
  (is (= (m/tag 1 (m/tag 3 {:y 1, :z 3}))
         (m/parse
           [:merge
            [:multi {:dispatch :y}
             [1 [:map [:y [:= 1]]]]
             [2 [:map [:y [:= 2]]]]]
            [:multi {:dispatch :z}
             [3 [:map [:z [:= 3]]]]
             [4 [:map [:z [:= 4]]]]]]
           {:y 1 :z 3}
           options))))

(deftest gen-distributive-multi-test
  (is (= [{:y 1, :z 3} {:y 2, :z 4} {:y 2, :z 3} {:y 2, :z 3} {:y 1, :z 4}
          {:y 1, :z 3} {:y 1, :z 3} {:y 1, :z 3} {:y 1, :z 3} {:y 2, :z 4}]
         (mg/sample
           [:merge
            [:multi {:dispatch :y}
             [1 [:map [:y [:= 1]]]]
             [2 [:map [:y [:= 2]]]]]
            [:multi {:dispatch :z}
             [3 [:map [:z [:= 3]]]]
             [4 [:map [:z [:= 4]]]]]]
           (assoc options :seed 0)))))
