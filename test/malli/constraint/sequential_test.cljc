(ns malli.constraint.sequential-test
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

(deftest vector-constraint-test
  (is (m/validate [:sequential {:distinct true} :any] [1 2 3]))
  (is (not (m/explain [:sequential {:distinct true} :any] [1 2 3])))
  (is (not (m/validate [:sequential {:distinct true} :any] [1 3 3])))
  (is (= ["should be distinct: 3 provided 2 times"]
         (me/humanize (m/explain [:sequential {:distinct true} :any] [1 3 3]))))

  (is (m/validate [:sequential {:sort true} :int] [1 2 3]))
  (is (not (m/explain [:sequential {:sorted true} :int] [1 2 3])))
  (is (not (m/validate [:sequential {:sorted true} :int] [3 2 1])))
  (is (= ["should have sorted elements"]
         (me/humanize (m/explain [:sequential {:sorted true} :int] [3 2 1]))))

  (is (= ["should be distinct: 2 provided 2 times"]
         (me/humanize (m/explain [:sequential {:sorted true :distinct true} :int] [1 2 2 3]))))

  (is (= [:and
          "should be distinct: 3 provided 2 times"
          "should have sorted elements"]
         (me/humanize (m/explain
                        [:sequential {:sorted true
                                      :distinct true} :int]
                        [1 3 3 2])))))
