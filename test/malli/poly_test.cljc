(ns malli.poly-test
  (:require [clojure.string :as str]
            [clojure.test :refer [are deftest is testing]]
            [clojure.test.check.generators :as gen]
            [clojure.walk :as walk]
            [malli.core :as m]
            [malli.edn :as edn]
            [malli.generator :as mg]
            [malli.error :as me]
            [malli.impl.util :as miu]
            [malli.registry :as mr]
            [malli.transform :as mt]
            [malli.util :as mu]
            [malli.poly :as poly]
            #?(:clj [malli.test-macros :refer [when-env]]))
  #?(:clj  (:import (clojure.lang IFn PersistentArrayMap PersistentHashMap))
     :cljs (:require-macros [malli.test-macros :refer [when-env]])))

(def options {:registry (mr/composite-registry m/default-registry (poly/schemas) (mu/schemas))})

(deftest all-test
  ;; no alpha-renaming needed
  (is (= [:all [:x] [:=> [:cat :x] :x]]
         (m/form (poly/all [x] [:=> [:cat x] x]) options)))
  (is (= [:all [:x] [:-> :x :x]]
         (m/form (poly/all [x] [:-> x x]) options)))
  ;; alpha-rename binder if clashing keyword in body form
  (is (= [:all [:x0] [:=> [:x :x0] :x0]]
         (m/form (poly/all [x] [:=> [:x x] x]) options)))
  (is (= [:all [:x] [:=> [:cat [:all [:y] :y]] :x]]
         (m/form (poly/all [x] [:=> [:cat (poly/all [y] y)] x]) options)))
  ;; alpha-rename outer binder if clashing :all inside (actually just 
  ;; a naive keyword occurrence check on the form of the body).
  (is (= [:all [:x0] [:=> [:cat [:all [:x] :x]] :x0]]
         (m/form (poly/all [x] [:=> [:cat (poly/all [x] x)] x]) options)))
  (is (= [:all [:x0] [:-> [:all [:x] :x] :x0]]
         (m/form (poly/all [x] [:-> (poly/all [x] x) x]) options)))
  (is (= [:=> [:cat [:schema :any]] [:schema :any]]
         (m/form (poly/inst (poly/all [x] [:=> [:cat x] x]) [:any] options))))
  (is (= [:->
          [:schema [:all [:x] [:-> :x :x]]]
          [:schema [:all [:x] [:-> :x :x]]]]
         (m/form (poly/inst (poly/all [x] [:-> x x])
                            [(poly/all [x] [:-> x x])]
                            options)))) ;;FIXME
  (is (= [:all [:y0] [:schema [:all [:y] :y]]]
         (m/form (poly/inst (poly/all [x] (poly/all [y] x))
                            [(poly/all [y] y)]
                            options))))
  ;;TODO could be smarter here since no substitution occurs
  (is (= [:all [:x1] :x1]
         (m/form (poly/inst (poly/all [x] (poly/all [x] x))
                            [(poly/all [x] x)]
                            options))))
  (is (= [:=> [:cat [:schema :any]] [:schema :any]]
         (m/form (m/deref (poly/all [a] [:=> [:cat a] a]) options))))
  (is (= [:-> [:schema :any] [:schema :any]]
         (m/form (m/deref (poly/all [a] [:-> a a]) options))))
  (is (= [:=> [:cat [:schema [:maybe :map]] [:schema :any]]
          [:merge [:schema [:maybe :map]] [:map [:x [:schema :any]]]]]
         (-> (poly/all [[M [:maybe :map]] X] [:=> [:cat M X] [:merge M [:map [:x X]]]])
             (m/schema options)
             m/deref
             m/form)))
  (is (= [:->
          [:schema [:maybe :map]]
          [:schema :any]
          [:merge
           [:schema [:maybe :map]]
           [:map [:x [:schema :any]]]]]
         (-> (poly/all [[M [:maybe :map]] X] [:-> M X [:merge M [:map [:x X]]]])
             (m/schema options)
             m/deref
             m/form))))
