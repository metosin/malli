(ns malli.constraint.number-test
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


(deftest number-constraint-test
  (is (m/validate [:int {:> 5 :< 10}] 8))
  (is (not (m/validate [:int {:> 5 :< 10}] 2)))
  (is (not (m/validate [:int {:> 5 :< 10}] 5)))
  (is (m/validate [:int {:=> 5 :< 10}] 5))
  (is (not (m/validate [:int {:> 5 :< 10}] 10)))
  (is (m/validate [:int {:> 5 :<= 10}] 10))
  (testing "nested constraints"
    (is (m/validate [:int {:and [[:> 5] [:< 10]]}] 8))
    (is (not (m/validate [:int {:and [[:> 5] [:< 10]]}] 11)))
    (is (m/validate [:int {:not [:and [:> 5] [:< 10]]}] 4))
    (is (m/validate [:int {:not [:and [:> 5] [:< 10]]}] 11))
    (is (not (m/validate [:int {:not [:> 5]}] 6)))
    (is (m/validate [:int {:not [:> 5]}] 5))
    (is (m/validate [:int {:not [:> 5]}] 4)))
  (testing ":gen ignored"
    (is (m/validate [:int {:gen/> 5 :< 10}] 3))
    (is (not (m/validate [:int {:gen/> 5 :< 10}] 11)))
    (is (m/validate [:int {:and [[:gen/> 5] [:< 10]]}] 8))
    (is (not (m/validate [:int {:and [[:gen/> 5] [:< 10]]}] 11)))
    (is (m/validate [:int {:and [[:gen/> 5] [:< 10]]}] 4))))
