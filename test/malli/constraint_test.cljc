(ns malli.constraint-test
  (:require [clojure.string :as str]
            [clojure.test :refer [are deftest is testing]]
            [clojure.test.check.generators :as gen]
            [clojure.walk :as walk]
            [malli.core :as m]
            [malli.constraint.string.util :refer [code-point-offset-seq]]
            [malli.edn :as edn]
            [malli.generator :as mg]
            [malli.error :as me]
            [malli.impl.util :as miu]
            [malli.registry :as mr]
            [malli.test-utils :refer [with-schema-forms]]
            [malli.transform :as mt]
            [malli.util :as mu]
            #?(:clj [malli.test-macros :refer [when-env]]))
  #?(:clj  (:import (clojure.lang IFn PersistentArrayMap PersistentHashMap))
     :cljs (:require-macros [malli.test-macros :refer [when-env]])))

;; not sure this makes sense without objects [:is schema object path]. we could default to "this"
;; but need to keep forwards compat if we add explicit objects.
#_
(deftest is-prop-test
  (is (m/validate [:map
                   {:and [[:is [:= 1] [:get :x]]]}
                   [:x :int]
                   [:y :int]]
                  {:x 1 :y 2}))
  (is (not (m/validate [:map
                        {:and [[:is [:= 1] [:get :x]]]}
                        [:x :int]
                        [:y :int]]
                       {:x 2 :y 2})))
  (is (m/validate [:map
                   {:is [[:get :x] [:= 1]]}
                   [:x :int]
                   [:y :int]]
                  {:x 1 :y 2}))
  (is (not (m/validate [:map
                        {:and [[:is [:get :x] [:= 1]]]}
                        [:x :int]
                        [:y :int]]
                       {:x 2 :y 2})))
  (is (m/validate [:map
                   {:and [[:is [:get :x] [:= 1]]]}
                   [:x :int]
                   [:y :int]
                   ]
                  {:x 1 :y 2}))
  #_
  (is (m/validate [:map
                   {:and [[:is [:< [:get :x] [:get :y]]]]}
                   [:x :int]
                   [:y :int]
                   ]
                  {}))
  )

;; TODO
[:map
 {:> [:x :y]}
 [:x int?]
 [:y int?]
 [:z int?]]
[:not :int [:key :x] [:count]]
[:map
 {:and [:< [:is :int [:get :x]] [:in :y]]}
 [:x int?]
 [:y int?]
 [:z int?]]
