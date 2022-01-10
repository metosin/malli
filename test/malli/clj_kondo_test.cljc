(ns malli.clj-kondo-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [malli.clj-kondo :as clj-kondo]
            [malli.core :as m]
            [malli.registry :as mr]
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
     [:string-type-enum  [:maybe [:enum "b" "c"]]]
     [:keyword-type-enum [:enum :a :b]]
     [:any-type-enum [:enum :a "b" "c"]]
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

(def StringStartingWithA
  (m/schema
   (m/-simple-schema
    {:type ::string-starting-with-a
     :pred #(str/starts-with? % "a")
     :type-properties {:clj-kondo/type ::string-starting-with-a}})))

(def options
  {:registry (mr/composite-registry
              (m/-registry)
              {::string-starting-with-a StringStartingWithA})})

(defn f1
  [s]
  (str s " - this starts with a `a`"))

(m/=> f1 (m/schema [:=> [:cat ::string-starting-with-a] :string] options))

(deftest clj-kondo-integration-test

  (is (= {:op :keys,
          :opt {::price :double, :tags :set, ::y :boolean},
          :req {::id :string,
                :name :string,
                :description :nilable/string,
                :select-keys {:op :keys, :req {:x :int}},
                :nested {:op :keys, :req {:id :string, :price :double}},
                :string-type-enum :nilable/string
                :keyword-type-enum :keyword
                :any-type-enum :any
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
              {:arities {2 {:args [:ifn :coll], :ret :map}}}
              'f1
              {:arities {1 {:args [:malli.clj-kondo-test/string-starting-with-a]
                            :ret :string}}}}}
            (-> 'malli.clj-kondo-test
                (clj-kondo/collect)
                (clj-kondo/linter-config)
                (get-in [:linters :type-mismatch :namespaces])))))
  (testing "sequential elements"
    (is (= {:op :rest :spec :int}
           (clj-kondo/transform [:repeat :int])))
    (is (= {:op :rest :spec {:op :keys :req {:price :int}}}
           (clj-kondo/transform [:repeat [:map [:price :int]]])))
    (is (= {:op :rest :spec [:int]}
           (clj-kondo/transform [:repeat [:tuple :int]])))))
