(ns malli.clj-kondo-test
  (:require [clojure.test :refer [deftest is testing]]
            [malli.clj-kondo :as clj-kondo]
            [malli.core :as m]
            #?@(:clj [[clojure.java.io :as io]
                      [clojure.edn :as edn]])
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
    [:tuple-of-ints [:maybe [:tuple :int :int]]]
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

(defn kikka2
  ([x] (* x x))
  ([x y & z] (apply + (* x y) z)))

(m/=> kikka2 [:function
              [:-> :int [:int {:min 0}]]
              [:-> :int :int [:* :int] :int]])

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

(defn clj-kondo-issue-1922-4 [_x])
(m/=> clj-kondo-issue-1922-4
      [:function
       [:=> [:cat :int :int] :nil]
       [:=> [:cat :int :int [:repeat :int]] :nil]])

(defn clj-kondo-issue-836-1
  "Predicate `:fn` schema's should be type-checked as expecting to be passed `:any`, not a fn."
  [x y z] (* x y z))
(m/=> clj-kondo-issue-836-1 [:=> [:cat int? [:fn #(int? %)] int?] [:fn #(int? %)]])

(defn- cljk-collect-for-test
  "Collect up all of the clj-kondo linters generated for fn's in this test ns."
  []
  (-> 'malli.clj-kondo-test
      #?(:clj (clj-kondo/collect))
      #?(:cljs (clj-kondo/collect-cljs))
      (clj-kondo/linter-config)
      (get-in [:linters :type-mismatch :namespaces])))

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
                :z :vector
                :tuple-of-ints :nilable/seqable}}
         (clj-kondo/transform Schema)))

  (let [expected-out
        {'malli.clj-kondo-test
         {'kikka
          {:arities {1 {:args [:int],
                        :ret :int},
                     :varargs {:args [:int :int {:op :rest :spec :int}],
                               :ret :int,
                               :min-arity 2}}}
          'kikka2
          {:arities {1 {:args [:int],
                        :ret :int},
                     :varargs {:args [:int :int {:op :rest :spec :int}],
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
                        :ret :nil}}}

          'clj-kondo-issue-1922-4
          {:arities {2 {:args [:int :int]
                        :ret :nil}
                     :varargs {:args [:int :int {:op :rest :spec :int}]
                               :ret :nil
                               :min-arity 2}}}
          ;; should output `:any` for `:fn` predicate schema's, not `:fn`
          'clj-kondo-issue-836-1
          {:arities {3 {:args [:int :any :int], :ret :any}}}}}]
    #?(:clj
       (is (= expected-out (cljk-collect-for-test))))
    #?(:cljs
       (is (= expected-out (cljk-collect-for-test)))))
  (testing "sequential elements"
    (is (= :seqable
           (clj-kondo/transform [:repeat :int])))
    (is (= :seqable
           (clj-kondo/transform [:repeat [:map [:price :int]]])))
    (is (= :seqable
           (clj-kondo/transform [:repeat [:tuple :int]]))))

  (testing "regular expressions"
    (is (= :string (clj-kondo/transform [:re "kikka"]))
        "the :re schema models a string, clj-kondo's :regex a Pattern object")))

#?(:clj
   (deftest fix-1083
     (clj-kondo/emit! {:key "value"})
     (let [data (edn/read-string (slurp (io/file ".clj-kondo/metosin/malli-types-clj/config.edn")))]
       (is (map? data))
       (is (= [:linters] (keys data))))))
