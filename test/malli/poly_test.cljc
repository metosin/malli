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

(deftest f-in-registry-test
  (is (= [:-> [:schema {:registry {::a [:schema :any]}} [:schema :any]] [:schema :any]]
         (m/form (m/deref (poly/all [a] [:-> [:schema {:registry {::a a}} a] a])
                          options)))))

(deftest poly-generator-test
  ;;TODO :P
  (is (thrown-with-msg?
        #?(:clj Exception, :cljs js/Error)
        #":malli.generator/no-generator"
        (mg/generate (poly/all [X] [:=> [:cat X] X]) options)))
  ;;via deref
  (is (= {} ((mg/generate (m/deref (poly/all [X] [:=> [:cat X] X]) options) {:seed 1 :size 2}) 1))))

(defn is-all-good [schema vs]
  (testing "good"
    (doseq [[i f] (map-indexed vector vs)]
      (testing i
        (is (nil? (mg/check schema f options)))))))

(defn is-all-bad  [schema vs]
  (testing "bad"
    (doseq [[i f] (map-indexed vector vs)]
      (testing i
        (try (let [res (mg/check schema f (assoc options ::mg/all-iterations 1000))]
               (is res))
             (catch #?(:clj Exception, :cljs js/Error) e
               (is (= ::m/invalid-input (:type (ex-data e))))))))))

(def good-identities [identity
                      (fn [a] a)
                      (fn [a] (identity a))])
(def bad-identities [(fn [_] nil)
                     (fn [a] (when (uuid? a) a))])

(def identity-specs [(poly/all [a] [:=> [:cat a] a])
                     (poly/all [a] [:-> a a])])

(deftest identity-test
  (doseq [identity-spec identity-specs]
    (testing (pr-str identity-spec)
      (is-all-good identity-spec good-identities)
      (is-all-bad identity-spec bad-identities))))

(def good-maps [map
                (fn [f c] (map f c))
                (fn [f c] (mapv f c))])
(def bad-maps [(comp #(map str %) map)
               (fn [f c] (map (comp f str) c))
               (fn [f c] (map (comp str f) c))])

(def map-specs [(poly/all [a b] [:=> [:cat [:=> [:cat a] b] [:sequential a]] [:sequential b]])
                (poly/all [a b] [:-> [:-> a b] [:sequential a] [:sequential b]])])

;; TODO catch higher-order failures and shrink them.
(deftest map-test
  (doseq [map-spec map-specs]
    (testing (pr-str map-spec)
      (is-all-good map-spec good-maps)
      (is-all-bad map-spec bad-maps))))
