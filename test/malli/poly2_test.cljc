(ns malli.poly2-test
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
            [malli.poly2 :as poly]
            #?(:clj [malli.test-macros :refer [when-env]]))
  #?(:clj  (:import (clojure.lang IFn PersistentArrayMap PersistentHashMap))
     :cljs (:require-macros [malli.test-macros :refer [when-env]])))

(def options {:registry (mr/composite-registry m/default-registry (poly/schemas) (mu/schemas))})

(deftest all-test
  (is (= [:all [:x] [:=> [:cat :x] :x]]
         (m/form [:all [:x] [:=> [:cat :x] :x]] options)))
  (is (= [:all [:x] [:-> :x :x]]
         (m/form [:all [:x] [:-> :x :x]] options)))
  (is (= [:all [:x] [:=> [:cat [:all [:y] :y]] :x]]
         (m/form [:all [:x] [:=> [:cat [:all [:y] :y]] :x]] options)))
  ;; alpha-rename outer binder if clashing :all inside (actually just 
  ;; a naive keyword occurrence check on the form of the body).
  (is (= [:all [:x] [:-> [:all [:x] :x] :x]]
         (m/form [:all [:x] [:-> [:all [:x] :x] :x]] options)))
  (is (= [:=> [:cat [:schema :any]] [:schema :any]]
         (m/form (poly/inst (poly/all [x] [:=> [:cat x] x]) [:any] options))))
  (is (= [:->
          [:schema [:all [:x] [:-> :x :x]]]
          [:schema [:all [:x] [:-> :x :x]]]]
         (m/form (poly/inst [:all [:x] [:-> :x :x]]
                            [[:all [:x] [:-> :x :x]]]
                            options))))
  (is (= [:all [:y] :y]
         (m/form (poly/inst [:all [:x] [:all [:y] :x]]
                            [[:all [:y] :y]]
                            options))))
  (is (= [:all [:x] :x]
         (m/form (poly/inst [:all [:x] [:all [:x] :x]]
                            [[:all [:x] :x]]
                            options))))
  (is (= [:=> [:cat [:schema :any]] [:schema :any]]
         (m/form (m/deref [:all [:a] [:=> [:cat :a] :a]] options))))
  (is (= [:-> [:schema :any] [:schema :any]]
         (m/form (m/deref [:all [:a] [:-> :a :a]] options))))
  (is (= [:=> [:cat [:schema [:maybe :map]] [:schema :any]]
          [:merge [:schema [:maybe :map]] [:map [:x [:schema :any]]]]]
         (-> [:all [[:M [:maybe :map]] :X]
              [:=> [:cat :M :X] [:merge :M [:map [:x :X]]]]]
             (m/schema options)
             m/deref
             m/form)))
  (is (= [:->
          [:schema [:maybe :map]]
          [:schema :any]
          [:merge
           [:schema [:maybe :map]]
           [:map [:x [:schema :any]]]]]
         (-> [:all [[:M [:maybe :map]] :X]
              [:-> :M :X [:merge :M [:map [:x :X]]]]]
             (m/schema options)
             m/deref
             m/form))))

(deftest f-in-registry-test
  (is (= [:-> [:schema {:registry {::a :any}} :any] :any]
         (m/form (m/deref [:all [:a] [:-> [:schema {:registry {::a :a}} :a] :a]]
                          options)))))

(deftest poly-generator-test
  ;;TODO :P
  (is (thrown-with-msg?
        #?(:clj Exception, :cljs js/Error)
        #":malli.generator/no-generator"
        (mg/generate [:all [:X] [:=> [:cat :X] :X]] options)))
  ;;via deref
  (is (= {} ((mg/generate (m/deref [:all [:X] [:=> [:cat :X] :X]] options) {:seed 1 :size 2}) 1))))

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

(def identity-specs [[:all [:a] [:=> [:cat :a] :a]]
                     [:all [:a] [:-> :a :a]]])

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

(def map-specs [[:all [:a :b] [:=> [:cat [:=> [:cat :a] :b] [:sequential :a]] [:sequential :b]]]
                [:all [:a :b] [:-> [:-> :a :b] [:sequential :a] [:sequential :b]]]])

;; TODO catch higher-order failures and shrink them.
(deftest map-test
  (doseq [map-spec map-specs]
    (testing (pr-str map-spec)
      (is-all-good map-spec good-maps)
      (is-all-bad map-spec bad-maps))))

(deftest -abstract-test
  (is (= [:schema {::poly/scope true} [::poly/b 0]]
         (m/form (poly/-abstract [::poly/f :a] :a options))))
  (is (= [:schema {::poly/scope true} [:schema {:registry {::a [::poly/b 0]}} ::a]]
         (m/form (poly/-abstract [:schema {:registry {::a [::poly/f :a]}} ::a] :a options))))
  (is (= [:schema {::poly/scope true} [:schema {::poly/scope true} [::poly/b 1]]]
         (m/form (poly/-abstract [:schema {::poly/scope true} [::poly/f :a]] :a options))))
  (is (= [:schema {::poly/scope true}
          [:schema {::poly/scope true}
           [:schema {::poly/scope true}
            [::poly/b 2]]]]
         (m/form (poly/-abstract [:schema {::poly/scope true}
                                  [:schema {::poly/scope true}
                                   [::poly/f :a]]]
                                 :a
                                 options))))
  (is (= [:schema {::poly/scope true}
          [:schema {::poly/scope true}
           [:schema {::poly/scope true}
            [:-> [::poly/b 1] [::poly/b 2]]]]]
         (m/form (poly/-abstract [:schema {::poly/scope true}
                                  [:schema {::poly/scope true}
                                   [:-> [::poly/b 1] [::poly/f :a]]]]
                                 :a
                                 options)))))

(deftest -instantiate-test
  (is (= :any
         (m/form
           (poly/-instantiate [:schema {::poly/scope true} [::poly/b 0]]
                              :any
                              options))))
  (is (= [:schema {:registry {::a :a}} ::a]
         (m/form
           (poly/-instantiate [:schema {::poly/scope true} [:schema {:registry {::a [::poly/b 0]}} ::a]]
                              [::poly/f :a]
                              options))))
  (is (= [:schema {::poly/scope true} :int]
         (m/form
           (poly/-instantiate
             [:schema {::poly/scope true} [:schema {::poly/scope true} [::poly/b 1]]]
             :int
             options))))
  (is (= [:schema {::poly/scope true}
          [:schema {::poly/scope true}
           :int]]
         (m/form
           (poly/-instantiate
             [:schema {::poly/scope true}
              [:schema {::poly/scope true}
               [:schema {::poly/scope true}
                [::poly/b 2]]]]
             :int
             options))))
  (is (= [:schema {::poly/scope true}
          [:schema {::poly/scope true}
           [:-> [::poly/b 1] :int]]]
         (m/form
           (poly/-instantiate
             [:schema {::poly/scope true}
              [:schema {::poly/scope true}
               [:schema {::poly/scope true}
                [:-> [::poly/b 1] [::poly/b 2]]]]]
             :int
             options)))))

(deftest all-smart-constructor-destructor-test
  (is (= [:all [:x] [:-> :x :x]]
         (m/form [:all [:x] [:-> :x :x]] options)))
  (is (= [:all [:x :y] [:-> :x :y]]
         (m/form [:all [:x :y] [:-> :x :y]] options)))
  (is (= [:-> :int :boolean]
         (m/form
           (poly/inst (m/schema [:all [:x :y] [:-> :x :y]] options)
                      [:int :boolean]
                      options))))
  (is (thrown-with-msg?
        #?(:clj Exception, :cljs js/Error)
        #"regex-not-kind-schema"
        (poly/inst (m/schema [:all [:x :y] [:-> :x :y]] options)
                   [[:? :int] [:* :boolean]]
                   options)))
  (is (= [:-> :x :y]
         (m/form
           (poly/inst (m/schema (poly/all [x y] [:-> x y]) options)
                      [[::poly/f :x]
                       [::poly/f :y]]
                      options))))
  )

(comment
  (m/form
    (m/schema
      [:schema {:registry {::Reducer (m/tfn [a b] [:=> b a b])
                           ::Transducer (m/tfn [in out]
                                               (m/all [r]
                                                      [:=> [::Reducer out r] [::Reducer in r]]))}}
       (m/all [in out] [:=> [:=> in out] [::Transducer in out]])]))
  )
