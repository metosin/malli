(ns malli.clj-kondo-test
  (:require [clojure.test :refer [deftest is]]
            [malli.clj-kondo :as clj-kondo]
            [malli.core :as m]
            [malli.util :as mu]))

(def Schema
  (m/schema
    [:map {:registry {::id string?
                      ::price double?}}
     ::id
     [::price {:optional true}]
     [:name string?]
     [:description [:maybe string?]]
     [:tags {:optional true} [:set qualified-keyword?]]
     [::y {:optional true} boolean?]
     [:select-keys [:maybe [:select-keys [:map [:x int?] [:y int?]] [:x]]]]
     [:nested [:merge
               [:map [:id ::id]]
               [:map [:price ::price]]]]
     [:z [:vector [:map-of int? int?]]]]
    {:registry (merge (m/default-schemas) (mu/schemas))}))

(defn kikka
  ([x] (* x x))
  ([x y & z] (apply + (* x y) z)))

(m/=> kikka [:function
             [:=> [:cat :int] [:int {:min 0}]]
             [:=> [:cat :int :int [:* :int]] :int]])

(defn siren [f coll]
  (into {} (map (juxt f identity) coll)))

(m/=> siren [:=> [:cat ifn? coll?] map?])

(deftest clj-kondo-integration-test

  (is (= {:op :keys,
          :opt {::price :double, :tags :set, ::y :boolean},
          :req {::id :string,
                :name :string,
                :description :nilable/string,
                :select-keys {:op :keys, :req {:x :int}},
                :nested {:op :keys, :req {:id :string, :price :double}},
                :z :vector}}
         (clj-kondo/transform Schema)))

  #?(:clj
     (is (= {'malli.clj-kondo-test
             {'kikka
              {:arities {1 {:args [:int],
                            :ret :int},
                         :varargs {:args [:int :int {:op :rest, :spec :int}],
                                   :ret :int,
                                   :min-arity 2}}}
              'siren
              {:arities {2 {:args [:ifn :coll], :ret :map}}}}}
            (-> 'malli.clj-kondo-test
                (clj-kondo/collect)
                (clj-kondo/linter-config)
                (get-in [:linters :type-mismatch :namespaces]))))))
