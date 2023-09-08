(ns malli.clj-kondo-test
  (:require [clojure.test :refer [deftest is testing]]
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
    [:xyz :any]
    [:xyz2 [:maybe :any]]
    [:xyz3 [:maybe :int]]
    [:nested [:merge
              [:map [:id ::id]]
              [:map [:price ::price]]]]
    [:string-type-enum [:maybe [:enum "b" "c"]]]
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

(defn clj-kondo-issue-1922-1 [_x])
(m/=> clj-kondo-issue-1922-1
      [:=> [:cat [:map [:keys [:+ :keyword]]]] :nil])

(defn clj-kondo-issue-1922-2 [_x])
(m/=> clj-kondo-issue-1922-2
      [:=> [:cat [:map [:keys [:* :int]]]] :nil])

(defn clj-kondo-issue-1922-3 [_x])
(m/=> clj-kondo-issue-1922-3
      [:=> [:cat [:map [:keys [:? :string]]]] :nil])

(deftest clj-kondo-integration-test

  (is (= {:op :keys,
          :opt {::price :double, :tags :set, ::y :boolean},
          :req {::id :string,
                :name :string,
                :description :nilable/string,
                :select-keys {:op :keys, :req {:x :int} :nilable true},
                :xyz :any
                :xyz2 :any
                :xyz3 :nilable/int
                :nested {:op :keys, :req {:id :string, :price :double}},
                :string-type-enum :nilable/string
                :keyword-type-enum :keyword
                :any-type-enum :any
                :z :vector}}
         (clj-kondo/transform Schema)))

  (let [expected-out
        {'malli.clj-kondo-test
         {'kikka
          {:arities {1 {:args [:int],
                        :ret :int},
                     :varargs {:args [:int :int :seqable],
                               :ret :int,
                               :min-arity 2}}}
          'siren
          {:arities {2 {:args [:ifn :coll], :ret :map}}}

          'clj-kondo-issue-1922-1
          {:arities {1 {:args [{:op :keys
                                :req {:keys :seqable}}]
                        :ret :nil}}}

          'clj-kondo-issue-1922-2
          {:arities {1 {:args [{:op :keys
                                :req {:keys :seqable}}]
                        :ret :nil}}}

          'clj-kondo-issue-1922-3
          {:arities {1 {:args [{:op :keys
                                :req {:keys :seqable}}]
                        :ret :nil}}}}}]

    #?(:clj
       (is (= expected-out
              (-> 'malli.clj-kondo-test
                  (clj-kondo/collect)
                  (clj-kondo/linter-config)
                  (get-in [:linters :type-mismatch :namespaces])))))

    #?(:cljs
       (is (= expected-out
              (-> 'malli.clj-kondo-test
                  (clj-kondo/collect-cljs)
                  (clj-kondo/linter-config)
                  (get-in [:linters :type-mismatch :namespaces]))))))
  (testing "sequential elements"
    (is (= {:op :rest :spec :int}
           (clj-kondo/transform [:repeat :int])))
    (is (= {:op :rest :spec {:op :keys :req {:price :int}}}
           (clj-kondo/transform [:repeat [:map [:price :int]]])))
    (is (= {:op :rest :spec [:int]}
           (clj-kondo/transform [:repeat [:tuple :int]]))))

  (testing "regular expressions"
    (is (= :string (clj-kondo/transform [:re "kikka"]))
        "the :re schema models a string, clj-kondo's :regex a Pattern object")))
