(ns malli.adapter.spec1-test
  (:require [malli.adapter.spec1 :as from]
            [malli.registry :as mr]
            [malli.core :as m]
            [clojure.test :refer [deftest is]]
            [clojure.spec.alpha :as s]))
(set! *print-namespace-maps* false)

(def options {:registry (mr/composite-registry
                          (m/default-schemas)
                          (malli.adapter.spec1/schemas))})

(defn schema [m] (m/schema m options))

(s/def ::from-spec int?)
(def from-malli :int)

(def malli-map-int-int
  (schema [:map-of from-malli [::from/spec ::from-spec]]))

(s/def ::spec-map-int-int
  (s/map-of (from/malli from-malli options)
            ::from-spec))

(defn forms [s]
  {:m/form (m/form s)
   :s/form (s/form s)})

(deftest form-test
  (is (= {:m/form :int
          :s/form `(from/malli :int options)}
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
                    :schema [::from/spec 'clojure.core/int?]
                    :value nil}]}
         (-> (m/explain malli-map-int-int {1 nil} options)
             (update :schema m/form)
             (update :errors #(mapv (fn [m] (update m :schema m/form)) %)))))
  (is (= {:schema [:map-of :int [:malli.adapter.spec1/spec :malli.adapter.spec1-test/from-spec]]
          :value {nil nil}
          :errors [{:path [0], :in [nil], :schema :int, :value nil}
                   {:path [1], :in [nil], :schema [:malli.adapter.spec1/spec clojure.core/int?], :value nil}]}
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
           [{:path [0], :pred (malli.adapter.spec1/malli :int), :val nil, :via [], :in [nil 0]}
            {:path [1], :pred (malli.adapter.spec1/malli [:malli.adapter.spec1/spec clojure.core/int?]), :val nil, :via [], :in [nil 1]}
            {:path [0], :pred (malli.adapter.spec1/malli :int), :val :a, :via [], :in [:a 0]}
            {:path [1], :pred (malli.adapter.spec1/malli [:malli.adapter.spec1/spec clojure.core/int?]), :val :a, :via [], :in [:a 1]}]
           ::s/spec (malli.adapter.spec1/malli [:map-of :int [:malli.adapter.spec1/spec :malli.adapter.spec1-test/from-spec]])
           ::s/value {nil nil, :a :a}}
         (-> (s/explain-data (from/malli malli-map-int-int) {nil nil :a :a})
             (update ::s/spec s/form)))))
