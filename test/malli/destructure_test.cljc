(ns malli.destructure-test
  (:require [clojure.test :refer [deftest is testing]]
            [malli.core :as m]
            [malli.destructure :as md]))

(def expectations
  [{:name "empty"
    :bind '[]
    :schema :cat}
   {:name "1 arg"
    :bind '[a]
    :schema [:cat :any]}
   {:name "2 args"
    :bind '[a b]
    :schema [:cat :any :any]}
   {:name "2 + varargs"
    :bind '[a b & cs]
    :schema [:cat :any :any [:* :any]]}
   {:name "sequence destructuring"
    :bind '[a [b1 [b2] & bs :as bss] & [c1 c2 & cs :as css]]
    :schema [:cat
             :any
             [:maybe
              [:cat
               [:? :any]
               [:maybe
                [:cat
                 [:? :any]
                 [:* :any]]]
               [:* :any]]]
             [:maybe
              [:cat
               [:? :any]
               [:? :any]
               [:* :any]]]]}
   {:name "map destructuring"
    :bind '[a {:keys [b]
               :strs [c]
               :syms [d]
               :demo/syms [e]
               :demo/keys [f]
               g :demo/g
               h 123
               :or {b 0, d 0, f 0}
               :as map}]
    :schema [:cat
             :any
             [:orn
              ;; Unfortunately, the output order is different between clj and cljs, and we use strict equality in the test
              [:map #?(:clj
                       [:map
                        [:b {:optional true} :any]
                        ["c" {:optional true} :any]
                        ['d {:optional true} :any]
                        ['demo/e {:optional true} :any]
                        [:demo/f {:optional true}]
                        [123 {:optional true} :any]
                        [:demo/g {:optional true}]]
                       :cljs
                       [:map
                        [:b {:optional true} :any]
                        ["c" {:optional true} :any]
                        ['d {:optional true} :any]
                        ['demo/e {:optional true} :any]
                        [:demo/f {:optional true}]
                        [:demo/g {:optional true}]
                        [123 {:optional true} :any]])]
              [:args [:schema
                      #?(:clj
                         [:*
                          [:alt
                           [:cat [:= :b] :any]
                           [:cat [:= "c"] :any]
                           [:cat [:= 'd] :any]
                           [:cat [:= 'demo/e] :any]
                           [:cat [:= :demo/f] :demo/f]
                           [:cat [:= 123] :any]
                           [:cat [:= :demo/g] :demo/g]
                           [:cat [:not [:enum :b "c" 'd 'demo/e :demo/f 123 :demo/g]] :any]]]
                         :cljs
                         [:*
                          [:alt
                           [:cat [:= :b] :any]
                           [:cat [:= "c"] :any]
                           [:cat [:= 'd] :any]
                           [:cat [:= 'demo/e] :any]
                           [:cat [:= :demo/f] :demo/f]
                           [:cat [:= :demo/g] :demo/g]
                           [:cat [:= 123] :any]
                           [:cat [:not [:enum :b "c" 'd 'demo/e :demo/f :demo/g 123]] :any]]])]]]]
    :errors '[[{::keysz [z]}]
              [{:kikka/keyz [z]}]]}
   {:name "map destructuring with required-keys"
    :bind '[{:keys [a :demo/b] :demo/keys [c]}]
    :options {::md/required-keys true}
    :schema [:cat
             [:orn
              [:map [:map
                     [:a :any]
                     :demo/b
                     :demo/c]]
              [:args [:schema [:* [:alt
                                   [:cat [:= :a] :any]
                                   [:cat [:= :demo/b] :demo/b]
                                   [:cat [:= :demo/c] :demo/c]
                                   [:cat [:not [:enum :a :demo/b :demo/c]] :any]]]]]]]}
   {:name "map destructuring with required-keys and closed-maps"
    :bind '[{:keys [a :demo/b] :demo/keys [c]}]
    :options {::md/required-keys true
              ::md/closed-maps true}
    :schema [:cat
             [:orn
              [:map [:map {:closed true}
                     [:a :any]
                     :demo/b
                     :demo/c]]
              [:args [:schema [:* [:alt
                                   [:cat [:= :a] :any]
                                   [:cat [:= :demo/b] :demo/b]
                                   [:cat [:= :demo/c] :demo/c]]]]]]]}
   {:name "map destructuring with required-keys, closed-maps and references disallowed"
    :bind '[{:keys [a :demo/b] :demo/keys [c]}]
    :options {::md/required-keys true
              ::md/closed-maps true
              ::md/references false}
    :schema [:cat
             [:orn
              [:map [:map {:closed true}
                     [:a :any]
                     [:demo/b :any]
                     [:demo/c :any]]]
              [:args [:schema [:* [:alt
                                   [:cat [:= :a] :any]
                                   [:cat [:= :demo/b] :any]
                                   [:cat [:= :demo/c] :any]]]]]]]}
   {:name "map destructuring with required-keys, closed-maps, references and no sequential-maps"
    :bind '[{:keys [a :demo/b] :demo/keys [c]}]
    :options {::md/required-keys true
              ::md/closed-maps true
              ::md/sequential-maps false}
    :schema [:cat
             [:map {:closed true}
              [:a :any]
              :demo/b
              :demo/c]]}
   {:name "Keyword argument functions now also accept maps"
    :bind '[a & {:keys [b]
                 :strs [c]
                 :syms [d]
                 :demo/keys [e]
                 :demo/syms [f]
                 :or {b 0, d 0, f 0} :as map}]
    :options {::md/sequential-maps false} ;; no effect here
    :schema [:cat
             :any
             [:orn
              [:map [:map
                     [:b {:optional true} :any]
                     ["c" {:optional true} :any]
                     ['d {:optional true} :any]
                     [:demo/e {:optional true}]
                     ['demo/f {:optional true} :any]]]
              [:args [:*
                      [:alt
                       [:cat [:= :b] :any]
                       [:cat [:= "c"] :any]
                       [:cat [:= 'd] :any]
                       [:cat [:= :demo/e] :demo/e]
                       [:cat [:= 'demo/f] :any]
                       [:cat [:not [:enum :b "c" 'd :demo/e 'demo/f]] :any]]]]]]}
   {:name "Nested Keyword argument"
    :bind '[[& {:keys [a b] :as opts}]
            & {:keys [a b] :as opts}]
    :schema [:cat
             [:maybe
              [:cat
               [:orn
                [:map [:map
                       [:a {:optional true} :any]
                       [:b {:optional true} :any]]]
                [:args [:* [:alt
                            [:cat [:= :a] :any]
                            [:cat [:= :b] :any]
                            [:cat [:not [:enum :a :b]] :any]]]]]]]
             [:orn
              [:map [:map
                     [:a {:optional true} :any]
                     [:b {:optional true} :any]]]
              [:args [:* [:alt
                          [:cat [:= :a] :any]
                          [:cat [:= :b] :any]
                          [:cat [:not [:enum :a :b]] :any]]]]]]}
   {:name "Nest right-to-left map syntax"
    :bind '[{{inner :inner} :outer}]
    :schema [:cat
             [:orn
              [:map [:map
                     [:outer
                      {:optional true}
                      [:orn
                       [:map [:map
                              [:inner {:optional true} :any]]]
                       [:args [:schema
                               [:* [:alt
                                    [:cat [:= :inner] :any]
                                    [:cat [:not [:enum :inner]] :any]]]]]]]]]
              [:args [:schema
                      [:* [:alt
                           [:cat
                            [:= :outer]
                            [:orn
                             [:map [:map [:inner {:optional true} :any]]]
                             [:args [:schema [:* [:alt
                                                  [:cat [:= :inner] :any]
                                                  [:cat [:not [:enum :inner]] :any]]]]]]]
                           [:cat [:not [:enum :outer]] :any]]]]]]]}])

(def schematized-expectations
  [{:name "empty"
    :bind '[]
    :schema :cat}
   {:name "1 arg"
    :bind '[a :- :int]
    :schema [:cat :int]}
   {:name "2 args"
    :bind '[a :- :int, b :- :boolean]
    :schema [:cat :int :boolean]}
   {:name "2 + varargs"
    :bind '[a, b :- :int & cs :- [:* :boolean]]
    :schema [:cat :any :int [:* :boolean]]}
   {:name "Sequence destructuring - 1"
    :bind '[a :- :int [b1 :- :int [b2 :- :int] & bs :as bss]]
    :schema [:cat
             :int
             [:maybe
              [:cat
               [:? :int]
               [:maybe
                [:cat
                 [:? :int]
                 [:* :any]]]
               [:* :any]]]]}
   {:name "Sequence destructuring - 2 (rest)"
    :bind '[a :- :int [b1 :- :int [b2 :- :int] & bs :- [:* :int] :as bss]]
    :schema [:cat
             :int
             [:maybe
              [:cat
               [:? :int]
               [:maybe
                [:cat
                 [:? :int]
                 [:* :any]]]
               [:* :int]]]]}
   {:name "Sequence destructuring - 3 (as)"
    :bind '[a :- :int [b1 :- :int [b2 :- :int] & bs :as bss :- [:* :int]]]
    :schema [:cat
             :int
             [:schema [:* :int]]]}
   {:name "Sequence destructuring - 4 (bind rest)"
    :bind '[a :- :int & [b1 :- :int [b2 :- :int] & bs :- [:* :int] :as bss]]
    :schema [:cat
             :int
             [:maybe
              [:cat
               [:? :int]
               [:maybe
                [:cat
                 [:? :int]
                 [:* :any]]]
               [:* :int]]]]}
   {:name "map destructuring"
    :bind '[a :- :int, {:keys [b]
                        :strs [c]
                        :syms [d]
                        :demo/keys [e]
                        :demo/syms [f]
                        :or {b 0, d 0, f 0} :as map}
            :- [:map
                [:b :int]
                ["c" :int]
                [d :string]
                [:demo/e :string]
                [demo/f :symbol]]]
    :schema [:cat
             :int
             [:map
              [:b :int]
              ["c" :int]
              ['d :string]
              [:demo/e :string]
              ['demo/f :symbol]]]}
   {:name "Keyword argument functions now also accept maps"
    :bind '[& {:keys [b]
               :strs [c]
               :syms [d]
               :demo/keys [e]
               :demo/syms [f]
               :or {b 0, d 0, f 0} :as map}
            :- [:map
                [:b :int]
                ["c" :int]
                [d :string]
                [:demo/e :string]
                [demo/f :symbol]]]
    :schema [:cat
             [:map
              [:b :int]
              ["c" :int]
              ['d :string]
              [:demo/e :string]
              ['demo/f :symbol]]]}
   {:name "Nested Keyword argument"
    :bind '[[& {:keys [a b] :as opts} :- [:map [:a :int] [:b :int]]]
            & {:keys [a b] :as opts} :- [:map [:a :int] [:b :int]]]
    :schema [:cat
             [:maybe
              [:cat
               [:map
                [:a :int]
                [:b :int]]]]
             [:map
              [:a :int]
              [:b :int]]]}
   {:name "derived map keys"
    :bind '[{[g :- :int & gs :- [:* :string]] :value
             [a & as :as aas :- [:* :boolean]] 123}]
    :options {::md/sequential-maps false
              ::md/required-keys true}
    :schema [:cat [:map
                   [:value [:maybe [:cat
                                    [:? :int]
                                    [:* :string]]]]
                   [123 [:schema [:* :boolean]]]]]}])

(deftest parse-test
  (let [test-all (fn [expectations]
                   (doseq [{:keys [name bind errors options] expected :schema} expectations]
                     (testing (str "- " name " -")
                       (let [{:keys [arglist schema]} (md/parse bind options)]
                         (testing "has expected schema"
                           (when-not (is (= expected schema))
                             (prn "?" expected)
                             (prn ">" schema)))
                         (testing "has valid arglist"
                           (is (not= ::m/invalid arglist)))
                         (testing "errors"
                           (doseq [error errors]
                             (is (thrown? #?(:clj Exception, :cljs js/Error) (md/parse error)))))))))]

    (testing "parsing schematized syntax"
      (let [syntax '[x :- :int]]

        (testing "succeeds by default"
          (is (= [:cat :int] (:schema (md/parse syntax)))))

        (testing "fails if inline-schemas is disables"
          (is (thrown? #?(:clj Exception, :cljs js/Error) (md/parse syntax {::md/inline-schemas false}))))))

    (testing "vanilla clojure"
      (test-all expectations))

    (testing "schematized clojure"
      (test-all schematized-expectations))))

(deftest binding-schema
  (is (m/form md/Binding)))

(deftest function-schema-test
  (is (= [:=> [:cat [:map [:a :any] :demo/b :demo/c]] :any]
         (md/-function-schema
          '[[{:keys [a :demo/b] :demo/keys [c]}]]
          {::md/sequential-maps false
           ::md/required-keys true})))
  (is (= [:function
          [:=> [:cat :int] :any]
          [:=> [:cat :int [:* :int]] :any]]
         (md/-function-schema
          '([a :- :int]
            [a :- :int & bs :- [:* :int]])))))

(defn my-var
  ([a] (my-var a nil))
  ([a & bs] [a bs]))

#?(:clj
   (deftest infer-test
     (is (= [:function
             [:=> [:cat :any] :any]
             [:=> [:cat :any [:* :any]] :any]]
            (md/infer #'my-var)))))
