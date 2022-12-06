(ns malli.generator-ast-test
  (:require [clojure.pprint :refer [pprint]]
            [clojure.test :refer [are deftest is testing]]
            [malli.generator-ast :as ast]))

(deftest generator-ast-test
  (is (= '{:op :recursive-gen,
           :rec-gen
           {:op :one-of,
            :generators
            [{:op :boolean}
             {:op :tuple,
              :generators [{:op :return, :value :not} {:op :boolean}]}
             {:op :tuple,
              :generators
              [{:op :return, :value :and}
               {:op :vector, :generator {:op :recur}}]}
             {:op :tuple,
              :generators
              [{:op :return, :value :or}
               {:op :vector, :generator {:op :recur}}]}]},
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
           :rec-gen
           {:op :one-of,
            :generators
            [{:op :boolean}
             {:op :tuple,
              :generators [{:op :return, :value :not} {:op :boolean}]}
             {:op :tuple,
              :generators
              [{:op :return, :value :and}
               {:op :not-empty, :gen {:op :vector, :generator {:op :recur}}}]}
             {:op :tuple,
              :generators
              [{:op :return, :value :or}
               {:op :not-empty, :gen {:op :vector, :generator {:op :recur}}}]}]},
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
            [:ref ::formula]]))))
