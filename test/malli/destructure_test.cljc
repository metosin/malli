(ns malli.destructure-test
  (:require [clojure.test :refer [deftest testing is]]
            [malli.destructure :as md]))

(def expectations
  [{:name "empty"
    :bind '[]
    :schema :cat}
   {:name "1 arg"
    :bind '[a]
    :schema [:cat :any]}
   {:name "2 args"
    :bind '[a b]
    :schema [:cat :any :any]}
   {:name "2 + varargs"
    :bind '[a b & cs]
    :schema [:cat :any :any [:* :any]]}
   {:name "sequence destructuring"
    :bind '[a [b1 [b2] & bs :as bss] & [c1 c2 & cs :as css]]
    :schema [:cat
             :any
             [:maybe
              [:cat
               [:? :any]
               [:maybe
                [:cat
                 [:? :any]
                 [:* :any]]]
               [:* :any]]]
             [:maybe
              [:cat
               [:? :any]
               [:? :any]
               [:* :any]]]]}
   {:name "map destructuring"
    :bind '[a {:keys [b c]
               :strs [e f]
               :syms [g h]
               :or {b 0, e 0, g 0} :as bc}]
    :schema [:cat
             :any
             [:map
              [:b {:optional true} :any]
              [:c {:optional true} :any]
              ["e" {:optional true} :any]
              ["f" {:optional true} :any]
              ['g {:optional true} :any]
              ['h {:optional true} :any]]]}
   {:name "Keyword argument functions now also accept maps"
    :bind '[& {:keys [a b], :strs [c d], :syms [e f] :as opts}]
    :schema [:cat
             [:altn
              [:map [:map
                     [:a {:optional true} :any]
                     [:b {:optional true} :any]
                     ["c" {:optional true} :any]
                     ["d" {:optional true} :any]
                     ['e {:optional true} :any]
                     ['f {:optional true} :any]]]
              [:args [:*
                      [:alt
                       [:cat [:= :a] :any]
                       [:cat [:= :b] :any]
                       [:cat [:= "c"] :any]
                       [:cat [:= "d"] :any]
                       [:cat [:= 'e] :any]
                       [:cat [:= 'f] :any]]]]]]}
   {:name "Nested Keyword argument"
    :bind '[[& {:keys [a b] :as opts}]
            & {:keys [a b] :as opts}]
    :schema [:cat
             [:maybe
              [:altn
               [:map [:map
                      [:a {:optional true} :any]
                      [:b {:optional true} :any]]]
               [:args [:* [:alt
                           [:cat [:= :a] :any]
                           [:cat [:= :b] :any]]]]]]
             [:altn
              [:map [:map
                     [:a {:optional true} :any]
                     [:b {:optional true} :any]]]
              [:args [:* [:alt
                          [:cat [:= :a] :any]
                          [:cat [:= :b] :any]]]]]]}])

(deftest parse-test
  (doseq [{:keys [name bind schema]} expectations]
    (testing name
      (is (= schema (:schema (md/parse bind)))))))
