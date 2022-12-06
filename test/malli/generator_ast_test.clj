(ns malli.generator-ast-test
  (:require [clojure.pprint :refer [pprint]]
            [clojure.test :refer [are deftest is testing]]
            [malli.generator-ast :as ast]))

(deftest generator-ast-test
  (is (= '{:op :recursive-gen,
           :target :recur0
           :rec-gen
           {:op :one-of,
            :generators
            [{:op :boolean}
             {:op :tuple,
              :generators [{:op :return, :value :not} {:op :boolean}]}
             {:op :tuple,
              :generators
              [{:op :return, :value :and}
               {:op :vector, :generator {:op :recur :target :recur0}}]}
             {:op :tuple,
              :generators
              [{:op :return, :value :or}
               {:op :vector, :generator {:op :recur :target :recur0}}]}]},
           :scalar-gen
           {:op :one-of,
            :generators
            [{:op :boolean}
             {:op :tuple,
              :generators [{:op :return, :value :not} {:op :boolean}]}
             {:op :tuple,
              :generators
              [{:op :return, :value :and} {:op :return, :value ()}]}
             {:op :tuple,
              :generators
              [{:op :return, :value :or} {:op :return, :value ()}]}]}}
         (ast/generator-ast
           [:schema
            {:registry
             {::formula
              [:or
               :boolean
               [:tuple [:enum :not] :boolean]
               [:tuple [:enum :and] [:* [:ref ::formula]]]
               [:tuple [:enum :or]  [:* [:ref ::formula]]]]}}
            [:ref ::formula]])))
  (is (= '{:op :recursive-gen,
           :target :recur0
           :rec-gen
           {:op :one-of,
            :generators
            [{:op :boolean}
             {:op :tuple,
              :generators [{:op :return, :value :not} {:op :boolean}]}
             {:op :tuple,
              :generators
              [{:op :return, :value :and}
               {:op :vector-min
                :generator {:op :recur :target :recur0}
                :min 1}]}
             {:op :tuple,
              :generators
              [{:op :return, :value :or}
               {:op :vector-min
                :generator {:op :recur :target :recur0}
                :min 1}]}]},
           :scalar-gen
           {:op :one-of,
            :generators
            [{:op :boolean}
             {:op :tuple,
              :generators [{:op :return, :value :not} {:op :boolean}]}]}}
         (ast/generator-ast
           [:schema
            {:registry
             {::formula
              [:or
               :boolean
               [:tuple [:enum :not] :boolean]
               [:tuple [:enum :and] [:+ [:ref ::formula]]]
               [:tuple [:enum :or]  [:+ [:ref ::formula]]]]}}
            [:ref ::formula]])))
  #_
  (is (= nil
         (ast/generator-ast
           [:schema
            {:registry {::A [:tuple [:= "A"] [:maybe [:or [:ref ::B] [:ref ::C]]]]
                        ::B [:tuple [:= "B"] [:maybe [:or [:ref ::C] [:ref ::A]]]]
                        ::C [:tuple [:= "C"] [:maybe [:or [:ref ::A] [:ref ::B]]]]}}
            [:ref ::A]]))))

(deftest maybe-ast-test
  (is (ast/generator-ast [:maybe :boolean])))
