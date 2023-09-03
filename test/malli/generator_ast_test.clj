(ns malli.generator-ast-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.test.check.generators :as tcgen]
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
              [:tuple [:enum :or] [:* [:ref ::formula]]]]}}
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
              [:tuple [:enum :or] [:+ [:ref ::formula]]]]}}
           [:ref ::formula]])))
  (is (= '{:op :recursive-gen,
           :target :recur0,
           :rec-gen
           {:op :tuple,
            :generators
            [{:op :return, :value "A"}
             {:op :one-of,
              :generators
              [{:op :return, :value nil}
               {:op :one-of,
                :generators
                [{:op :recursive-gen,
                  :target :recur1,
                  :rec-gen
                  {:op :tuple,
                   :generators
                   [{:op :return, :value "B"}
                    {:op :one-of,
                     :generators
                     [{:op :return, :value nil}
                      {:op :one-of,
                       :generators
                       [{:op :tuple,
                         :generators
                         [{:op :return, :value "C"}
                          {:op :one-of,
                           :generators
                           [{:op :return, :value nil}
                            {:op :one-of,
                             :generators
                             [{:op :recur, :target :recur0}
                              {:op :recur, :target :recur1}]}]}]}
                        {:op :recur, :target :recur0}]}]}]},
                  :scalar-gen
                  {:op :tuple,
                   :generators
                   [{:op :return, :value "B"}
                    {:op :one-of,
                     :generators
                     [{:op :return, :value nil}
                      {:op :one-of,
                       :generators
                       [{:op :tuple,
                         :generators
                         [{:op :return, :value "C"}
                          {:op :one-of,
                           :generators
                           [{:op :return, :value nil}
                            {:op :recur, :target :recur0}]}]}
                        {:op :recur, :target :recur0}]}]}]}}
                 {:op :recursive-gen,
                  :target :recur1,
                  :rec-gen
                  {:op :tuple,
                   :generators
                   [{:op :return, :value "C"}
                    {:op :one-of,
                     :generators
                     [{:op :return, :value nil}
                      {:op :one-of,
                       :generators
                       [{:op :recur, :target :recur0}
                        {:op :tuple,
                         :generators
                         [{:op :return, :value "B"}
                          {:op :one-of,
                           :generators
                           [{:op :return, :value nil}
                            {:op :one-of,
                             :generators
                             [{:op :recur, :target :recur1}
                              {:op :recur, :target :recur0}]}]}]}]}]}]},
                  :scalar-gen
                  {:op :tuple,
                   :generators
                   [{:op :return, :value "C"}
                    {:op :one-of,
                     :generators
                     [{:op :return, :value nil}
                      {:op :one-of,
                       :generators
                       [{:op :recur, :target :recur0}
                        {:op :tuple,
                         :generators
                         [{:op :return, :value "B"}
                          {:op :one-of,
                           :generators
                           [{:op :return, :value nil}
                            {:op :recur, :target :recur0}]}]}]}]}]}}]}]}]},
           :scalar-gen
           {:op :tuple,
            :generators
            [{:op :return, :value "A"}
             {:op :one-of,
              :generators
              [{:op :return, :value nil}
               {:op :one-of,
                :generators
                [{:op :recursive-gen,
                  :target :recur0,
                  :rec-gen
                  {:op :tuple,
                   :generators
                   [{:op :return, :value "B"}
                    {:op :one-of,
                     :generators
                     [{:op :return, :value nil}
                      {:op :tuple,
                       :generators
                       [{:op :return, :value "C"}
                        {:op :one-of,
                         :generators
                         [{:op :return, :value nil}
                          {:op :recur, :target :recur0}]}]}]}]},
                  :scalar-gen
                  {:op :tuple,
                   :generators
                   [{:op :return, :value "B"}
                    {:op :one-of,
                     :generators
                     [{:op :return, :value nil}
                      {:op :tuple,
                       :generators
                       [{:op :return, :value "C"}
                        {:op :return, :value nil}]}]}]}}
                 {:op :recursive-gen,
                  :target :recur0,
                  :rec-gen
                  {:op :tuple,
                   :generators
                   [{:op :return, :value "C"}
                    {:op :one-of,
                     :generators
                     [{:op :return, :value nil}
                      {:op :tuple,
                       :generators
                       [{:op :return, :value "B"}
                        {:op :one-of,
                         :generators
                         [{:op :return, :value nil}
                          {:op :recur, :target :recur0}]}]}]}]},
                  :scalar-gen
                  {:op :tuple,
                   :generators
                   [{:op :return, :value "C"}
                    {:op :one-of,
                     :generators
                     [{:op :return, :value nil}
                      {:op :tuple,
                       :generators
                       [{:op :return, :value "B"}
                        {:op :return, :value nil}]}]}]}}]}]}]}}
         (ast/generator-ast
          [:schema
           {:registry {::A [:tuple [:= "A"] [:maybe [:or [:ref ::B] [:ref ::C]]]]
                       ::B [:tuple [:= "B"] [:maybe [:or [:ref ::C] [:ref ::A]]]]
                       ::C [:tuple [:= "C"] [:maybe [:or [:ref ::A] [:ref ::B]]]]}}
           [:ref ::A]]))))

(def this-ns *ns*)

(deftest generator-code-test
  (is (= '(tcgen/recursive-gen
           (fn [recur0]
             (tcgen/tuple (tcgen/return "A")
                          (tcgen/one-of
                           [(tcgen/return nil)
                            (tcgen/one-of
                             [(tcgen/recursive-gen
                               (fn [recur1]
                                 (tcgen/tuple (tcgen/return "B")
                                              (tcgen/one-of
                                               [(tcgen/return nil)
                                                (tcgen/one-of
                                                 [(tcgen/tuple
                                                   (tcgen/return "C")
                                                   (tcgen/one-of [(tcgen/return nil)
                                                                  (tcgen/one-of [recur0 recur1])]))
                                                  recur0])])))
                               (tcgen/tuple (tcgen/return "B")
                                            (tcgen/one-of [(tcgen/return nil)
                                                           (tcgen/one-of
                                                            [(tcgen/tuple (tcgen/return "C")
                                                                          (tcgen/one-of [(tcgen/return nil)
                                                                                         recur0]))
                                                             recur0])])))
                              (tcgen/recursive-gen
                               (fn [recur1]
                                 (tcgen/tuple (tcgen/return "C")
                                              (tcgen/one-of [(tcgen/return nil)
                                                             (tcgen/one-of
                                                              [recur0
                                                               (tcgen/tuple
                                                                (tcgen/return "B")
                                                                (tcgen/one-of [(tcgen/return nil)
                                                                               (tcgen/one-of [recur1 recur0])]))])])))
                               (tcgen/tuple (tcgen/return "C")
                                            (tcgen/one-of [(tcgen/return nil)
                                                           (tcgen/one-of
                                                            [recur0
                                                             (tcgen/tuple (tcgen/return "B")
                                                                          (tcgen/one-of [(tcgen/return nil)
                                                                                         recur0]))])])))])])))
           (tcgen/tuple (tcgen/return "A")
                        (tcgen/one-of [(tcgen/return nil)
                                       (tcgen/one-of
                                        [(tcgen/recursive-gen
                                          (fn [recur0]
                                            (tcgen/tuple (tcgen/return "B")
                                                         (tcgen/one-of [(tcgen/return nil)
                                                                        (tcgen/tuple
                                                                         (tcgen/return "C")
                                                                         (tcgen/one-of [(tcgen/return nil)
                                                                                        recur0]))])))
                                          (tcgen/tuple (tcgen/return "B")
                                                       (tcgen/one-of [(tcgen/return nil)
                                                                      (tcgen/tuple (tcgen/return "C")
                                                                                   (tcgen/return nil))])))
                                         (tcgen/recursive-gen
                                          (fn [recur0]
                                            (tcgen/tuple (tcgen/return "C")
                                                         (tcgen/one-of [(tcgen/return nil)
                                                                        (tcgen/tuple (tcgen/return "B")
                                                                                     (tcgen/one-of [(tcgen/return nil)
                                                                                                    recur0]))])))
                                          (tcgen/tuple (tcgen/return "C")
                                                       (tcgen/one-of [(tcgen/return nil)
                                                                      (tcgen/tuple (tcgen/return "B")
                                                                                   (tcgen/return nil))])))])])))
         (binding [*ns* this-ns]
           (ast/generator-code
            [:schema
             {:registry {::A [:tuple [:= "A"] [:maybe [:or [:ref ::B] [:ref ::C]]]]
                         ::B [:tuple [:= "B"] [:maybe [:or [:ref ::C] [:ref ::A]]]]
                         ::C [:tuple [:= "C"] [:maybe [:or [:ref ::A] [:ref ::B]]]]}}
             [:ref ::A]])))))

(deftest maybe-ast-test
  (is (ast/generator-ast [:maybe :boolean])))
