(ns malli.adapter.spec1-test
  (:require [malli.adapter.spec1 :as from]
            malli.adapter.spec1.generator
            [malli.core :as m]
            [malli.generator :as mg]
            [malli.registry :as mr]
            [clojure.test :refer [deftest is]]
            [clojure.spec.alpha :as s]))

(def options {:registry (mr/composite-registry
                          (m/default-schemas)
                          (malli.adapter.spec1/schemas))})

(defn schema [m] (m/schema m options))

(s/def ::from-spec int?)
(def from-malli :int)

(def malli-map-int-int
  (schema [:map-of from-malli (from/spec ::from-spec)]))

(s/def ::spec-map-int-int
  (s/map-of (from/malli from-malli options)
            ::from-spec))

(defn forms [s]
  {:m/form (m/form s)
   :s/form (s/form s)})

(deftest form-test
  (is (= {:m/form :int
          :s/form `(from/malli :int)}
         (forms (from/malli from-malli options))
         (forms (from/malli (schema from-malli)))))
  (is (= {:m/form [::from/spec ::from-spec]
          :s/form ::from-spec}
         (forms (from/spec ::from-spec))))
  (is (= {:m/form [::from/spec ::spec-map-int-int]
          :s/form ::spec-map-int-int}
         (forms (from/spec ::spec-map-int-int))))
  (is (= {:m/form [::from/spec ::spec-map-int-int]
          :s/form `(from/malli [::from/spec ::spec-map-int-int])}
         (forms (from/malli (schema [::from/spec ::spec-map-int-int])))
         (forms (from/malli [::from/spec ::spec-map-int-int]
                            options)))))

(deftest validate-test
  (is (m/validate malli-map-int-int {}))
  (is (m/validate malli-map-int-int {1 2}))
  (is (not (m/validate malli-map-int-int {nil 2})))
  (is (not (m/validate malli-map-int-int {1 nil})))
  (is (not (m/validate malli-map-int-int {nil nil})))

  (is (s/valid? ::spec-map-int-int {}))
  (is (s/valid? ::spec-map-int-int {1 2}))
  (is (not (s/valid? ::spec-map-int-int {nil 2})))
  (is (not (s/valid? ::spec-map-int-int {1 nil})))
  (is (not (s/valid? ::spec-map-int-int {nil nil}))))

(deftest explain-test
  (is (nil? (m/explain malli-map-int-int {})))
  (is (nil? (m/explain malli-map-int-int {1 2})))

  ;;reference
  (is (= {:schema [:map-of :int :int]
          :value {nil nil, :a :a}
          :errors [{:path [0], :in [nil], :schema :int, :value nil}
                   {:path [1], :in [nil], :schema :int, :value nil}
                   {:path [0], :in [:a], :schema :int, :value :a}
                   {:path [1], :in [:a], :schema :int, :value :a}]}
         (-> (m/explain [:map-of :int :int] {nil nil :a :a})
             (update :schema m/form)
             (update :errors #(mapv (fn [m] (update m :schema m/form)) %)))))
  (is (= {:schema malli-map-int-int
          :value {nil 2}
          :errors [{:path [0], :in [nil], :schema :int, :value nil}]}
         (-> (m/explain malli-map-int-int {nil 2} options)
             (update :errors #(mapv (fn [m] (update m :schema m/form)) %)))))
  (is (= {:schema [:map-of :int [:malli.adapter.spec1/spec :malli.adapter.spec1-test/from-spec]]
          :value {1 nil}
          :errors [{:path [1]
                    :in [1]
                    :schema [:not {:malli.adapter.spec1/pred 'clojure.core/int?} :any]
                    :value nil}]}
         (-> (m/explain malli-map-int-int {1 nil} options)
             (update :schema m/form)
             (update :errors #(mapv (fn [m] (update m :schema m/form)) %)))))
  (is (= {:schema [:map-of :int [:malli.adapter.spec1/spec :malli.adapter.spec1-test/from-spec]]
          :value {nil nil}
          :errors [{:path [0], :in [nil], :schema :int, :value nil}
                   {:path [1], :in [nil], :schema [:not {:malli.adapter.spec1/pred 'clojure.core/int?} :any], :value nil}]}
         (-> (m/explain malli-map-int-int {nil nil} options)
             (update :schema m/form)
             (update :errors #(mapv (fn [m] (update m :schema m/form)) %)))))

  (is (nil? (s/explain-data ::spec-map-int-int {})))

  ;;reference
  (is (= '{::s/problems
           [{:path [0], :pred clojure.core/int?, :val nil, :via [], :in [nil 0]}
            {:path [1], :pred clojure.core/int?, :val nil, :via [], :in [nil 1]}
            {:path [0], :pred clojure.core/int?, :val :a, :via [], :in [:a 0]}
            {:path [1], :pred clojure.core/int?, :val :a, :via [], :in [:a 1]}]
           ::s/spec (clojure.spec.alpha/map-of clojure.core/int? clojure.core/int?)
           ::s/value {nil nil, :a :a}}
         (-> (s/explain-data (s/map-of int? int?) {nil nil :a :a})
             (update ::s/spec s/form))))

  (is (= '{::s/problems
           ;;FIXME :in
           [{:path [0], :pred (malli.adapter.spec1/malli :int), :val nil, :via [], :in [nil #_0]}
            {:path [1], :pred (malli.adapter.spec1/malli [:not {:malli.adapter.spec1/pred clojure.core/int?} :any]), :val nil, :via [], :in [nil #_1]}
            {:path [0], :pred (malli.adapter.spec1/malli :int), :val :a, :via [], :in [:a #_0]}
            {:path [1], :pred (malli.adapter.spec1/malli [:not {:malli.adapter.spec1/pred clojure.core/int?} :any]), :val :a, :via [], :in [:a #_1]}]
           ::s/spec (malli.adapter.spec1/malli [:map-of :int [:malli.adapter.spec1/spec :malli.adapter.spec1-test/from-spec]])
           ::s/value {nil nil, :a :a}}
         (-> (s/explain-data (from/malli malli-map-int-int) {nil nil :a :a})
             (update ::s/spec s/form)))))

;;TODO non-trivial conform
(deftest conform-test
  ;;reference 
  (is (= {} (s/conform ::spec-map-int-int {})))
  (is (= {1 2 3 4} (s/conform ::spec-map-int-int {1 2 3 4})))
  (is (s/invalid? (s/conform ::spec-map-int-int {nil nil})))

  (is (= {} (s/conform (from/malli malli-map-int-int) {})))
  (is (= {1 2 3 4} (s/conform (from/malli malli-map-int-int) {1 2 3 4})))
  (is (s/invalid? (s/conform (from/malli malli-map-int-int) {nil nil})))

  ;;reference 
  (is (= {} (m/parse malli-map-int-int {})))
  (is (= {1 2 3 4} (m/parse malli-map-int-int {1 2 3 4})))
  (is (= ::m/invalid (m/parse malli-map-int-int {nil nil})))

  (is (= {} (m/parse (from/spec ::spec-map-int-int) {})))
  (is (= {1 2 3 4} (m/parse (from/spec ::spec-map-int-int) {1 2 3 4})))
  (is (= ::m/invalid (m/parse (from/spec ::spec-map-int-int) {nil nil})))
  )

;;TODO non-trivial unform
(deftest unform-test
  ;;reference 
  (is (= {} (s/unform ::spec-map-int-int {})))
  (is (= {1 2 3 4} (s/unform ::spec-map-int-int {1 2 3 4})))
  ;;unspecified
  #_(is (= {nil nil} (s/unform ::spec-map-int-int {nil nil})))

  (is (= {} (s/unform (from/malli malli-map-int-int) {})))
  (is (= {1 2 3 4} (s/unform (from/malli malli-map-int-int) {1 2 3 4})))
  ;;unspecified, but returns ::s/invalid instead of {nil nil} here
  #_(is (= {nil nil} (s/unform (from/malli malli-map-int-int) {nil nil})))

  ;;reference 
  (is (= {} (m/unparse malli-map-int-int {})))
  (is (= {1 2 3 4} (m/unparse malli-map-int-int {1 2 3 4})))
  ;;unspecified
  #_(is (= ::m/invalid (m/unparse malli-map-int-int {nil nil})))

  (is (= {} (m/unparse (from/spec ::spec-map-int-int) {})))
  (is (= {1 2 3 4} (m/unparse (from/spec ::spec-map-int-int) {1 2 3 4})))
  ;;unspecified, but returns {nil nil} instead of ::s/invalid here
  #_(is (= ::m/invalid (m/unparse (from/spec ::spec-map-int-int) {nil nil})))
  )

(deftest generator-test
  (is (= 1784201 (mg/generate :int {:seed 0})))
  (is (= 1784201 (mg/generate (from/spec int?) {:seed 0})))
  (is (= {-4 -13, -3570485 1096685, -2 131807357, -13524428 39436, -14680951 1153921, 347743661 -2751,
          -103615943 -648438, -38928 -16, 69268 -107, 57 -1639299, -148186477 42426, -2983045 129753,
          -18747 -6, -30222466 28, 18476 65, -6893431 -605609}
         (mg/generate (from/spec ::spec-map-int-int) (assoc options :seed 0))))

  ;; use mg/sample to exercise s/gen's with seed
  (is (= [0 -1 -1 -1 0 3 1 1 7 109]
         (mg/sample (s/gen (from/malli :int options))
                    {:seed 0})))
  (is (= [0 -1 -1 -1 0 3 1 1 7 109]
         (mg/sample (s/gen int?)
                    {:seed 0})))


  (is (every? vector? (s/exercise int?)))
  (is (every? vector? (s/exercise (from/malli :int options))))
  )

(deftest readme-test
  (is (= (s/form (from/malli :int))
         `(from/malli :int)))
  (is (= (m/form (from/malli :int))
         :int))
  (is (= (s/form (from/spec int?))
         'int?))
  (is (= (m/form (from/spec int?))
         [:malli.adapter.spec1/spec 'int? int?]))

  (is (= (mg/generate [:tuple
                       (from/spec
                         (s/tuple
                           (from/malli
                             [:tuple (from/spec int?)])))]
                      {:seed 0})
         [[[0]]]))
  )
