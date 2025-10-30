(ns malli.parser-test
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
            #?(:clj [malli.test-macros :refer [when-env]]))
  #?(:clj  (:import (clojure.lang IFn PersistentArrayMap PersistentHashMap))
     :cljs (:require-macros [malli.test-macros :refer [when-env]])))

(defn simple-parser? [s] (boolean (:simple-parser (m/-parser-info (m/schema s) nil))))

(def inheriting-parser-templates
  "Schema templates which have simple parsers iff ::HOLE has a simple parser.
  Should also be generatable for any ::HOLE and have high likelihood of (un)parsing
  to a different value than its input if transforming."
  [::HOLE
   [:maybe ::HOLE]
   [:schema ::HOLE]
   [:schema {:registry {::a ::HOLE}} ::a]
   [:schema {:registry {::a ::HOLE}} [:ref ::a]]
   [:schema {:registry {::a [:ref ::b] ::b ::HOLE}} [:ref ::a]]
   [:tuple ::HOLE]
   [:tuple ::HOLE :any]
   [:vector ::HOLE]
   [:set ::HOLE]
   [:seqable ::HOLE]
   [:map [:foo ::HOLE]]
   [:map [:foo {:optional true} ::HOLE]]
   [:map [:foo ::HOLE] [:bar :int]]
   [:and ::HOLE] ;; generator will fail if :any is first
   [:and ::HOLE :any]
   [:and ::HOLE :any :any]
   [:or ::HOLE] ;; parser will always be identical if :any is first
   [:or ::HOLE :any]
   [:map-of ::HOLE :any]
   [:map-of :any ::HOLE]
   [:map-of ::HOLE ::HOLE]])

(def simple-parser-templates
  "Schema templates which have simple parsers for any value of ::HOLE."
  [[:and {:parse/transforming-child 1} ::HOLE :any]
   [:and {:parse/transforming-child :none} ::HOLE :any]
   [:every ::HOLE]
   [:-> ::HOLE]
   [:function [:-> ::HOLE]]])

(def transforming-parser-templates
  "Schema templates which have transforming parsers for any value of ::HOLE."
  [[:multi {:dispatch #'any?} [true ::HOLE]]
   [:multi {:dispatch #'boolean} [true :any] [false ::HOLE]]
   [:multi {:dispatch #'boolean} [true ::HOLE] [false :any]]
   [:andn [0 ::HOLE]]
   [:andn [0 ::HOLE] [1 :any]] ;; generator will fail if :any is first
   [:orn [0 ::HOLE]]
   [:orn [0 ::HOLE] [1 :any]]
   [:orn [0 :any] [1 ::HOLE]]
   [:orn [0 ::HOLE] [1 ::HOLE]]])

(def simple-parser-schemas
  "Schemas with simple parsers."
  [:any
   [:and :any]
   :int
   #'map?
   :tuple
   [:fn {:gen/schema :any} #'any?]
   [:= 42] [:enum 42] [:not= 42] [:< 5] [:> 5] [:<= 5] [:>= 5]
   #?@(:cljs [] :default [[:re #""]]) ;; no generator in cljs
   :nil
   :qualified-symbol
   :uuid
   [:not [:= (random-uuid)]] ;; generator is too unreliable to nest
   :some])

(def transforming-parser-schemas
  "Schemas with transforming parsers."
  [[:andn [:any :any]]
   [:catn [:any :any]]
   [:seqable [:catn [:any :any]]]
   [:multi {:dispatch #'any?} [true :any]]])

(defn ensure-parser-type [expected-simple s]
  #?(:bb nil ;; test.chuck doesn't work in bb
     :default (let [s (m/schema s)
                    parse (m/parser s)
                    unparse (m/parser s)]
                (if expected-simple
                  (doseq [g (mg/sample s)]
                    (testing (pr-str g)
                      (let [p (parse g)]
                        (is (identical? g p))
                        (is (identical? g (unparse p))))))
                  (is (some (fn [g]
                              (let [p (parse g)]
                                (and (not (identical? g p))
                                     (not (identical? g (unparse p))))))
                            (mg/sample s {:seed 0})))))))

(deftest parser-info-test
  ;; should really be in simple-parser-templates but :not has an unreliable generator
  (testing ":not is simple"
    (is (every? #(simple-parser? [:not %]) (concat simple-parser-schemas transforming-parser-schemas)))
    (ensure-parser-type true [:not [:= (random-uuid)]])
    (ensure-parser-type true [:not [:andn [:tag [:= (random-uuid)]]]]))
  (testing ":multi is transforming"
    (is (every? #(simple-parser? [:not %]) (concat simple-parser-schemas transforming-parser-schemas)))
    (ensure-parser-type true [:not [:andn [:any [:= (random-uuid)]]]]))
  (let [d (m/default-schemas)]
    (doseq [[hole hold-simple] (concat (map vector simple-parser-schemas (repeat true))
                                       (map vector transforming-parser-schemas (repeat false)))
            :let [_ (testing (pr-str hole)
                      (is (= hold-simple (simple-parser? hole))))]
            [template expected-simple] (concat (map vector simple-parser-templates (repeat true))
                                               (map vector transforming-parser-templates (repeat false))
                                               (map vector inheriting-parser-templates (repeat hold-simple)))
            :let [s (testing {:template template :hole hole}
                      (is (m/schema template {:registry (assoc d ::HOLE (m/schema hole))})))]]
      (testing (pr-str (list 'm/schema template
                             {:registry (list 'assoc (list 'm/default-schemas)
                                              (symbol "::HOLE") (list 'm/schema hole))}))
        (is (= expected-simple (simple-parser? s)))
        (ensure-parser-type expected-simple s)))))

(deftest and-complex-parser-test
  (is (= {} (m/parse [:and :map [:fn map?]] {})))
  (is (= {} (m/parse [:and [:fn map?] :map] {})))
  (is (= #malli.core.Tag{:key :left, :value 1} (m/parse [:and [:orn [:left :int] [:right :int]] [:fn number?]] 1)))
  (is (= #malli.core.Tag{:key :left, :value 1} (m/parse [:and [:fn number?] [:orn [:left :int] [:right :int]]] 1)))
  (is (= 1 (m/parse [:and {:parse/transforming-child :none} [:fn number?] [:orn [:left :int] [:right :int]]] 1)))
  (is (= 1 (m/parse [:and :int [:or :int :boolean]] 1)))
  (is (= 1 (m/parse [:and [:or :int :boolean] :int] 1)))
  (is (= #malli.core.Tag{:key :int, :value 1} (m/parse [:and :int [:orn [:int :int] [:boolean :boolean]]] 1)))
  (is (= #malli.core.Tag{:key :int, :value 1} (m/parse [:and [:orn [:int :int] [:boolean :boolean]] :int] 1)))
  (is (= #malli.core.Tag{:key :int, :value 1} (m/parse [:and [:and [:orn [:int :int] [:boolean :boolean]] :int] :int] 1)))
  (is (= #malli.core.Tag{:key :l, :value #malli.core.Tag{:key :int, :value 1}}
         (m/parse [:and
                   [:orn [:l [:and [:orn [:int :int] [:boolean :boolean]] :int]]]
                   :int] 1)))
  (is (= 1
         (m/parse [:and
                   {:parse/transforming-child :none}
                   [:orn [:l [:and [:orn [:int :int] [:boolean :boolean]] :int]]]
                   [:orn [:r [:and [:orn [:int :int] [:boolean :boolean]] :int]]]]
                  1)))
  (is (= #malli.core.Tag{:key :l, :value #malli.core.Tag{:key :int, :value 1}}
         (m/parse [:and
                   {:parse/transforming-child 0}
                   [:orn [:l [:and [:orn [:int :int] [:boolean :boolean]] :int]]]
                   [:orn [:r [:and [:orn [:int :int] [:boolean :boolean]] :int]]]]
                  1)))
  (is (= #malli.core.Tag{:key :r, :value #malli.core.Tag{:key :int, :value 1}}
         (m/parse [:and
                   {:parse/transforming-child 1}
                   [:orn [:l [:and [:orn [:int :int] [:boolean :boolean]] :int]]]
                   [:orn [:r [:and [:orn [:int :int] [:boolean :boolean]] :int]]]]
                  1)))
  (let [s [:and [:orn [:l [:and [:orn [:int :int] [:boolean :boolean]] :int]]] :int]]
    (is (= 1 (->> 1 (m/parse s) (m/unparse s)))))
  (let [s [:and
           {:parse/transforming-child 1}
           [:orn [:l [:and [:orn [:int :int] [:boolean :boolean]] :int]]]
           [:orn [:r [:and [:orn [:int :int] [:boolean :boolean]] :int]]]]]
    (is (= 1 (->> 1 (m/parse s) (m/unparse s)))))
  (is (m/parser [:and [:map] [:map]]))
  (is (m/parser [:and [:map [:left [:orn [:one :int]]]] [:map]]))
  (is (m/parser [:and [:map] [:map [:left [:orn [:one :int]]]]]))
  (is (thrown-with-msg?
        #?(:clj Exception, :cljs js/Error)
        #":malli\.core/and-schema-multiple-transforming-parsers"
        (m/parser [:and [:map [:left [:orn [:one :int]]]] [:map [:right [:orn [:one :int]]]]])))
  (is (-> (m/schema [:vector :int]) (m/-parser-info nil) :simple-parser))
  (is (-> (m/schema [:vector [:orn [:one :int]]]) (m/-parser-info nil) :simple-parser not))
  (is (= #malli.core.Tags{:values {"a" 3, "b" :x}}
         (m/parse [:and [:catn ["a" :int] ["b" :keyword]]
                   [:fn vector?]]
                  [3 :x])))
  (let [s [:and [:catn ["a" :int] ["b" :keyword]]
           [:fn vector?]]
        res (->> [3 :x]
                 (m/parse s)
                 (m/unparse s))]
    (is (= [3 :x] res))
    (is (m/validate s res)))
  (let [s [:and [:catn ["a" :int] ["b" :keyword]]
           [:vector :any]]
        res (->> [3 :x]
                 (m/parse s)
                 (m/unparse s))]
    (is (= [3 :x] res))
    (is (m/validate s res)))
  (let [s [:and [:catn ["a" :int] ["b" :keyword]]
           [:sequential :any]]
        res (->> [3 :x]
                 (m/parse s)
                 (m/unparse s))]
    (is (= [3 :x] res))
    (is (m/validate s res)))
  (let [s [:and [:catn ["a" :int] ["b" :keyword]]
           [:tuple :any :any]]
        res (->> [3 :x]
                 (m/parse s)
                 (m/unparse s))]
    (is (= [3 :x] res))
    (is (m/validate s res))))

(def cyclic-simple-parsers
  [[:schema {:registry
             {::Name [:or :keyword :string]
              ::Value [:or
                       number?
                       :string
                       :boolean
                       :nil
                       :keyword
                       [:sequential [:ref ::Value]]
                       [:map-of [:ref ::Name] [:ref ::Value]]]
              ::Arguments [:map-of [:ref ::Name] [:ref ::Value]]}}
         ::Arguments]
   [:schema {:registry
             {::Value [:sequential [:ref ::Value]]}}
    ::Value]
   ;; equivalent to :nil
   [:schema {:registry {::a [:maybe [:ref ::a]]}}
    [:ref ::a]]
   ;; inner ::a shadows outer ::a
   [:schema {:registry {::a [:schema {:registry {::a [:= 42]}} [:ref ::a]]}}
    [:ref ::a]]
   [:schema {:registry {::a [:ref ::b] ;; (1)
                        ::b [:schema {:registry {::b [:= 42]}}
                             ;; (2)
                             [:ref ::b]]}}
    [:ref ::a]]
   ;; it's insufficient to identify refs just by their expansion. here, ::a
   ;; expands to [:ref ::b] twice at (1) and (2), so it looks like a recursion point, except
   ;; they are different ::b's!
   [:schema {:registry {::a [:ref ::b] ;; (1)
                        ::b [:schema {:registry {::a [:ref ::b] ;; (2)
                                                 ::b [:= 42]}}
                             [:ref ::a]]}}
    [:ref ::a]]
   ;; if the outer ::a shadowed the inner one, it would be equivalent to
   ;; [:maybe :never] == [:maybe [:maybe :never]] == ..., which is just :nil
   [:schema {:registry {::a [:schema {:registry {::a :int}} [:maybe [:ref ::a]]]}}
    [:ref ::a]]
   [:schema
    {:registry {::outer [:schema {:registry {::outer :int
                                             ::inner [:ref ::outer]}}
                         [:ref ::inner]]}}
    [:ref ::outer]]
   [:schema {:registry {::cons [:or :nil [:tuple pos-int? [:ref ::cons]]]}}
    [:ref ::cons]]])

(def cyclic-transforming-parsers
  [[:schema {:registry
             {::Name [:orn [:k :keyword] [:s :string]]
              ::Value [:or
                       number?
                       :string
                       :boolean
                       :nil
                       :keyword
                       [:sequential [:ref ::Value]]
                       [:map-of [:ref ::Name] [:ref ::Value]]]
              ::Arguments [:map-of [:ref ::Name] [:ref ::Value]]}}
         ::Arguments]
   [:schema {:registry
             {::Value [:sequential [:orn [:a [:ref ::Value]]]]}}
    ::Value]])

(deftest cycle-detection-test
  (doseq [s (map m/schema cyclic-simple-parsers)]
    (testing (pr-str (m/form s))
      (is (m/parser s))
      (is (m/unparser s))
      (is (true? (:simple-parser (m/-parser-info s nil))))))
  (doseq [s (map m/schema cyclic-transforming-parsers)]
    (testing (pr-str (m/form s))
      (is (m/parser s))
      (is (m/unparser s))
      (is (not (:simple-parser (m/-parser-info s nil)))))))


(def infinite-parsers
  "Schemas whose parsers diverge or are unsatisfiable.
  -parser-info may infer these as simple, or diverge also."
  [[:schema {:registry {::a [:ref ::a]}} [:ref ::a]]
   [:schema {:registry {::a [:tuple [:ref ::a]]}}
    [:ref ::a]]
   ;; the scopes at (1) and (2) are different, so no recursion is detected between them.
   ;; instead, the [:ref ::a] at (2) is the recursion point with itself.
   [:schema {:registry {::a [:schema {:registry {::b [:= true]}}
                             ;; (2)
                             [:or [:ref ::a] [:ref ::b]]]}}
    [:schema {:registry {::b [:= false]}}
     ;; (1)
     [:or [:ref ::a] [:ref ::b]]]]])

#?(:bb nil :clj
   (deftest infinite-parser-test
     (doseq [s (map m/schema infinite-parsers)]
       (is (m/parser s))
       (is (m/unparser s))
       (is (try (:simple-parser (m/-parser-info s nil))
                (catch StackOverflowError _ true))))))
