(ns malli.constraint.solver-test
  (:require [clojure.test :refer [deftest is]]
            [malli.constraint.solver :as mcs]))

(deftest -constraint-solutions-test
  (is (= (mcs/-constraint-solutions
           [:and :a :b] :map nil)
         '({:order [:a :b], :present {:a true, :b true}})))
  (is (= (mcs/-constraint-solutions
           [:and [:xor :a :c] :b] :map nil)
         '({:order [:a :c :b], :present {:a true, :c false, :b true}}
           {:order [:a :c :b], :present {:a false, :c true, :b true}})))
  (is (= (mcs/-constraint-solutions
           [:or :a :b]
           :map nil)
         '({:order [:a], :present {:a true}}
           {:order [:b], :present {:b true}}
           {:order [:a :b], :present {:a true, :b true}})))
  (is (= (mcs/-constraint-solutions
           [:or :a :b :c]
           :map nil)
         '({:order [:a], :present {:a true}}
           {:order [:b], :present {:b true}}
           {:order [:c], :present {:c true}}
           {:order [:b :c], :present {:b true, :c true}}
           {:order [:a :c], :present {:a true, :c true}}
           {:order [:a :b], :present {:a true, :b true}}
           {:order [:a :b :c], :present {:a true, :b true, :c true}})))
  (is (= (mcs/-constraint-solutions
               [:xor :a :b]
               :map nil)
             '({:order [:a :b], :present {:a true, :b false}}
               {:order [:a :b], :present {:a false, :b true}})))
  (is (= (mcs/-constraint-solutions
               [:disjoint [:a] [:b]]
               :map nil)
             '({:order [:a :b], :present {:a false, :b false}}
               {:order [:a :b], :present {:a true, :b false}}
               {:order [:a :b], :present {:a false, :b true}})))
  (is (= (mcs/-constraint-solutions
               [:iff :a :b]
               :map nil)
             '({:order [:a :b], :present {:a false, :b false}}
               {:order [:a :b], :present {:a true, :b true}})))
  (is (= (mcs/-constraint-solutions
           [:implies :a :b]
           :map
           nil)
         '({:order [:a], :present {:a false}}
           {:order [:a :b], :present {:a true, :b true}}
           {:order [:b], :present {:b true}}
           {:order [:a :b], :present {:a false, :b true}})))

  (is (= [{:order [:a :hint1 :hint2], :present {:a true, :hint1 false, :hint2 false}}
          {:order [:a :hint1 :hint2], :present {:a true, :hint1 false, :hint2 true}}
          {:order [:a :hint1 :hint2], :present {:a true, :hint1 true, :hint2 false}}
          {:order [:a :hint1 :hint2], :present {:a true, :hint1 true, :hint2 true}}
          {:order [:b :hint1 :hint2], :present {:b true, :hint1 false, :hint2 false}}
          {:order [:b :hint1 :hint2], :present {:b true, :hint1 false, :hint2 true}}
          {:order [:b :hint1 :hint2], :present {:b true, :hint1 true, :hint2 false}}
          {:order [:b :hint1 :hint2], :present {:b true, :hint1 true, :hint2 true}}
          {:order [:a :b :hint1 :hint2], :present {:a true, :b true, :hint1 false, :hint2 false}}
          {:order [:a :b :hint1 :hint2], :present {:a true, :b true, :hint1 false, :hint2 true}}
          {:order [:a :b :hint1 :hint2], :present {:a true, :b true, :hint1 true, :hint2 false}}
          {:order [:a :b :hint1 :hint2], :present {:a true, :b true, :hint1 true, :hint2 true}}]
         (mcs/-constraint-solutions
           [:and
            [:or :a :b]
            ;[:or :hint3 [:not :hint3]]
            [:disjoint [:hint1 :hint2]]]
           :map
           nil)))
  (is (= [{:order [:a], :present {:a true}}
          {:order [:b], :present {:b true}}
          {:order [:a :b], :present {:a true, :b true}}]
         (mcs/-constraint-solutions
           [:or :a :b]
           :map
           nil)))
  (is (mcs/-constraint-solutions [:and
                                  [:or :a :b]
                                  [:or :hint3 [:not :hint3]]
                                  [:disjoint [:hint1 :hint2 :hint3]]]
                                 :map
                                 nil))
  ;;FIXME
  #_
  (is (seq (mcs/-constraint-solutions
             [:and
              :a
              :hint3
              [:disjoint [:hint3]]
              ]
             :map
             nil)))
  (is (= [{:order [:hint1 :hint2 :hint3], :present {:hint1 false, :hint2 false, :hint3 false}}
          {:order [:hint1 :hint2 :hint3], :present {:hint1 false, :hint2 false, :hint3 true}}
          {:order [:hint1 :hint2 :hint3], :present {:hint1 false, :hint2 true, :hint3 false}}
          {:order [:hint1 :hint2 :hint3], :present {:hint1 false, :hint2 true, :hint3 true}}
          {:order [:hint1 :hint2 :hint3], :present {:hint1 true, :hint2 false, :hint3 false}}
          {:order [:hint1 :hint2 :hint3], :present {:hint1 true, :hint2 false, :hint3 true}}
          {:order [:hint1 :hint2 :hint3], :present {:hint1 true, :hint2 true, :hint3 false}}
          {:order [:hint1 :hint2 :hint3], :present {:hint1 true, :hint2 true, :hint3 true}}]
         (mcs/-constraint-solutions [:disjoint [:hint1 :hint2 :hint3]]
                                    :map
                                    nil)
         (mcs/-constraint-solutions [:and [:disjoint [:hint1 :hint2 :hint3]]]
                                    :map
                                    nil))))
