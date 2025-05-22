(ns malli.generator-test
  (:require [clojure.test :refer [are deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check :refer [quick-check]]
            [clojure.test.check.properties :refer [for-all]]
            [malli.core :as m]
            [malli.generator :as mg]
            [malli.json-schema-test :as json-schema-test]
            [malli.util :as mu]
            #?(:clj  [malli.test-macros :refer [when-env]]
               :cljs ["@js-joda/timezone/dist/js-joda-timezone-10-year-range"]))
  #?(:cljs (:require-macros [malli.test-macros :refer [when-env]])))

(defn shrink [?schema]
  (-> (quick-check 1 (for-all [s (mg/generator ?schema)] false) {:seed 0})
      :shrunk
      :smallest
      first))

(deftest generator-test
  (doseq [[?schema _ ?fn] json-schema-test/expectations
          ;; cljs doesn't have a regex generator :(
          #?@(:cljs [:when (not= (m/type ?schema) :re)])
          :let [f (if ?fn #(%) identity)]]
    (testing (m/form ?schema)
      (testing "generate"
        (is (= (f (mg/generate ?schema {:seed 123}))
               (f (mg/generate ?schema {:seed 123}))))
        (is (= (f (mg/generate ?schema {:seed 123, :size 10}))
               (f (mg/generate ?schema {:seed 123, :size 10}))))
        (is (m/validate ?schema (mg/generate ?schema {:seed 123}))))
      (testing "sample"
        (is (= (map f (mg/sample ?schema {:seed 123}))
               (map f (mg/sample ?schema {:seed 123}))))
        (is (= (map f (mg/sample ?schema {:seed 123, :size 10}))
               (map f (mg/sample ?schema {:seed 123, :size 10}))))
        (doseq [value (mg/sample ?schema {:seed 123})]
          (is (m/validate ?schema value))))))

  (testing "simple schemas"
    (doseq [schema [:any
                    :some
                    [:string {:min 1, :max 4}]
                    [:int {:min 1, :max 4}]
                    [:double {:min 0.0, :max 1.0}]
                    :boolean
                    :keyword
                    :symbol
                    :qualified-keyword
                    :qualified-symbol]]
      (is (every? (m/validator schema) (mg/sample schema {:size 1000})))))

  (doseq [s [:double :float]]
    (testing (str s " properties")
      (let [infinity? #(or (= % ##Inf)
                           (= % ##-Inf))
            NaN? (fn [x]
                   (#?(:clj  Double/isNaN
                       :cljs js/isNaN)
                            x))
            special? #(or (NaN? %)
                          (infinity? %))
            valid? (m/validator s)
            test-presence (fn [f options]
                            (let [vs (mg/sample [s options]
                                                {:size 1000})]
                              (and (every? valid? vs)
                                   (some f vs))))]
        (is (test-presence infinity? {:gen/infinite? true}))
        (is (test-presence NaN? {:gen/NaN? true}))
        (is (test-presence special? {:gen/infinite? true
                                     :gen/NaN? true}))
        (is (not (test-presence special? nil))))))

  (testing "qualified-keyword properties"
    (testing "no namespace => random"
      (is (< 1 (->> (mg/sample [:qualified-keyword {:namespace nil}]
                               {:size 100})
                    (map namespace)
                    frequencies
                    (count))))
      (is (< 1 (->> (mg/sample [:qualified-keyword {}]
                               {:size 100})
                    (map namespace)
                    frequencies
                    (count)))))
    (testing "namespace => keyword with exact namesapce"
      (is (= {"hi" 100}
             (->> (mg/sample [:qualified-keyword {:namespace :hi}]
                             {:size 100})
                  (map namespace)
                  frequencies)))
      (is (= {"hi" 100}
             (->> (mg/sample [:qualified-keyword {:namespace "hi"}]
                             {:size 100})
                  (map namespace)
                  frequencies))))
    (testing "generated result should pass validation"
      (is (->> (mg/sample [:qualified-keyword {:namespace "hi"}]
                          {:size 100})
               (remove (partial m/validate [:qualified-keyword {:namespace "hi"}]))
               empty?))))

  (testing "qualified-symbol properties"
    (testing "no namespace => random"
      (is (< 1 (->> (mg/sample [:qualified-symbol {:namespace nil}]
                               {:size 100})
                    (map namespace)
                    frequencies
                    (count))))
      (is (< 1 (->> (mg/sample [:qualified-symbol {}]
                               {:size 100})
                    (map namespace)
                    frequencies
                    (count)))))
    (testing "namespace => symbol with exact namesapce"
      (is (= {"hi" 100}
             (->> (mg/sample [:qualified-symbol {:namespace :hi}]
                             {:size 100})
                  (map namespace)
                  frequencies)))
      (is (= {"hi" 100}
             (->> (mg/sample [:qualified-symbol {:namespace "hi"}]
                             {:size 100})
                  (map namespace)
                  frequencies))))
    (testing "generated result should pass validation"
      (is (->> (mg/sample [:qualified-symbol {:namespace "hi"}]
                          {:size 100})
               (remove (partial m/validate [:qualified-symbol {:namespace "hi"}]))
               empty?))))

  (testing "map entries"
    (is (= {:korppu "koira"
            :piilomaan "pikku aasi"
            :muuli "mukkelis"
            :aasi "isaskarin"
            :kissa "pikimustan kissan"}
           (mg/generate [:map {:gen/fmap '#(assoc % :korppu "koira")}
                         [:piilomaan {:gen/fmap '(partial str "pikku ")} [:string {:gen/elements ["aasi"]}]]
                         [:muuli {:gen/elements ["mukkelis"]} [:string {:gen/elements ["???"]}]]
                         [:aasi {:gen/return "isaskarin"} [:string {:gen/return "???"}]]
                         [:kissa {:gen/fmap '(partial str "pikimustan ")} [:string {:gen/return "kissan"}]]]))))

  (testing "ref"
    (testing "recursion"
      (let [schema [:schema {:registry {::cons [:maybe [:tuple int? [:ref ::cons]]]}}
                    ::cons]]
        (is (every? (m/validator schema) (mg/sample schema {:size 100})))))
    (testing "mutual recursion"
      (let [schema [:schema
                    {:registry {::ping [:maybe [:tuple [:= "ping"] [:ref ::pong]]]
                                ::pong [:maybe [:tuple [:= "pong"] [:ref ::ping]]]}}
                    ::ping]]
        (is (every? (m/validator schema) (mg/sample schema {:size 100})))))
    (testing "recursion limiting"
      (are [schema]
        (every? (m/validator schema) (mg/sample schema {:size 100}))

        [:schema {:registry {::rec [:maybe [:ref ::rec]]}} ::rec]
        [:schema {:registry {::rec [:map [:rec {:optional true} [:ref ::rec]]]}} ::rec]
        [:schema {:registry {::tuple [:tuple boolean? [:ref ::or]]
                             ::or [:or int? ::tuple]}} ::or]
        [:schema {:registry {::rec [:tuple int? [:vector {:max 2} [:ref ::rec]]]}} ::rec]
        [:schema {:registry {::rec [:tuple int? [:set {:max 2} [:ref ::rec]]]}} ::rec]
        [:schema {:registry {::multi
                             [:multi {:dispatch :type}
                              [:int [:map [:type [:= :int]] [:int int?]]]
                              [:multi [:map [:type [:= :multi]] [:multi {:optional true} [:ref ::multi]]]]]}} ::multi])))

  #?(:bb  nil ;; test.chuck doesn't work in bb
     :clj (testing "regex"
            (let [re #"^\d+ \d+$"]
              (m/validate re (mg/generate re)))

            (let [re-test #"(?=.{8,})" ;; contains unsupported feature
                  elements ["abcdefgh" "01234567"]
                  fmap '(fn [s] (str "prefix_" s))]
              (is (thrown-with-msg? Exception #"Unsupported-feature" (mg/generator [:re re-test])))
              (m/validate #".{8,}" (mg/generate [:re {:gen/elements elements} re-test]))
              (m/validate #"prefix_.{8,}" (mg/generate [:re {:gen/fmap fmap, :gen/elements elements} re-test])))))

  (testing "regex with custom generators"
    (is (= 42 (mg/generate [:re
                            {:gen/gen (gen/return 42)}
                            #"abc"]))
        "Using :gen/gen")
    (is (= 42 (mg/generate [:re
                            {:gen/return 42}
                            #"abc"]))
        "Using :gen/return")
    (is (= 42 (mg/generate [:re
                            {:gen/fmap (fn [_] 42)
                             :gen/schema :int}
                            #"abc"]))
        "Using :gen/fmap and :gen/schema")
    (is (= 42 (mg/generate [:re
                            {:gen/elements [42]}
                            #"abc"]))
        "Using :gen/elements"))

  (testing "no generator"
    (is (thrown-with-msg?
         #?(:clj Exception, :cljs js/Error)
         #":malli.generator/no-generator"
         (mg/generate [:fn '(fn [x] (<= 0 x 10))]))))

  (when-env
   "TEST_SCI"
   (testing "sci not available"
     (let [schema (m/schema [:string {:gen/fmap '(partial str "kikka_")}] {::m/disable-sci true})]
       (is (thrown-with-msg?
            #?(:clj Exception, :cljs js/Error)
            #":malli.core/sci-not-available"
            (mg/generator schema)))
       (is (thrown-with-msg?
            #?(:clj Exception, :cljs js/Error)
            #":malli.core/sci-not-available"
            (mg/generator [:string {:gen/fmap '(partial str "kikka_")}] {::m/disable-sci true})))
       (testing "direct options win"
         (is (mg/generator schema {::m/disable-sci false}))))))

  (testing "generator override"
    (testing "without generator"
      (let [schema [:fn {:gen/elements [5]
                         :gen/fmap '(fn [i] (rand-int i))}
                    '(fn [x] (<= 0 x 10))]
            generator (mg/generator schema)]
        (dotimes [_ 100]
          (let [v (mg/generate generator)]
            (is (m/validate schema v))
            (is (<= 0 v 5))))))
    (testing "with generator"
      (is (re-matches #"kikka_\d+" (mg/generate [:and {:gen/fmap '(partial str "kikka_")} pos-int?])))))

  (testing "gen/return"
    (is (every? nil? (mg/sample [:and {:gen/return nil} int?] {:size 1000})))
    (is (every? #{1} (mg/sample [:and {:gen/return 1} int?] {:size 1000})))
    (is (every? #{"1"} (mg/sample [:and {:gen/return 1, :gen/fmap 'str} int?] {:size 1000}))))

  (testing "gen/elements"
    (is (every? #{1 2} (mg/sample [:and {:gen/elements [1 2]} int?] {:size 1000})))
    (is (every? #{"1" "2"} (mg/sample [:and {:gen/elements [1 2], :gen/fmap 'str} int?] {:size 1000}))))

  (testing "gen/schema"
    (is (every? #{1 2} (mg/sample [:int {:gen/schema [:int {:gen/elements [1 2]}]}] {:size 1000})))
    (is (every? #{"+1" "+2"} (mg/sample [:int {:gen/schema [:int {:gen/elements [1 2], :gen/fmap str}]
                                               :gen/fmap (partial str "+")}] {:size 1000}))))

  (testing "gen/gen"
    (is (every? #{1 2} (mg/sample [:and {:gen/gen (gen/elements [1 2])} int?] {:size 1000})))
    (is (every? #{"1" "2"} (mg/sample [:and {:gen/gen (gen/elements [1 2]) :gen/fmap str} int?] {:size 1000})))))

(defn- schema+coll-gen [type children-gen]
  (gen/let [children children-gen]
    (let [schema (into [type] children)]
      (gen/tuple (gen/return schema) (mg/generator schema)))))

(def ^:private seqex-child
  (let [s (gen/elements [string? int? keyword?])]
    (gen/one-of [s (gen/fmap #(vector :* %) s)])))

(defspec cat-test 100
  (for-all [[s coll] (schema+coll-gen :cat (gen/vector seqex-child))]
    (m/validate s coll)))

(defspec catn-test 100
  (for-all [[s coll] (->> (gen/vector (gen/tuple gen/keyword seqex-child))
                          (gen/such-that (fn [coll] (or (empty? coll) (apply distinct? (map first coll)))))
                          (schema+coll-gen :catn))]
    (m/validate s coll)))

(defspec alt-test 100
  (for-all [[s coll] (schema+coll-gen :alt (gen/not-empty (gen/vector seqex-child)))]
    (m/validate s coll)))

(defspec altn-test 100
  (for-all [[s coll] (->> (gen/not-empty (gen/vector (gen/tuple gen/keyword seqex-child)))
                          (gen/such-that (fn [coll] (or (empty? coll) (apply distinct? (map first coll)))))
                          (schema+coll-gen :altn))]
    (m/validate s coll)))

(defspec ?*+-test 100
  (for-all [[s coll] (gen/let [type (gen/elements [:? :* :+])
                               child seqex-child]
                       (let [schema [type child]]
                         (gen/tuple (gen/return schema) (mg/generator schema))))]
    (m/validate s coll)))

(defspec repeat-test 100
  (for-all [[s coll] (schema+coll-gen :repeat (gen/tuple
                                               (gen/let [min gen/nat
                                                         len gen/nat]
                                                 {:min min, :max (+ min len)})
                                               seqex-child))]
    (m/validate s coll)))

(deftest min-max-test

  (testing "valid properties"
    (are [schema]
      (every? #(<= 10 % 20) (map count (mg/sample schema {:size 1000})))

      [:vector {:min 10, :max 20} int?]
      [:set {:min 10, :max 20} int?]
      [:string {:min 10, :max 20}]

      [:vector {:gen/min 10, :max 20} int?]
      [:set {:gen/min 10, :max 20} int?]
      [:string {:gen/min 10, :max 20}]

      [:vector {:min 10, :gen/max 20} int?]
      [:set {:min 10, :gen/max 20} int?]
      [:string {:gen/min 10, :max 20}]

      [:+ {:min 10 :max 20} :int]
      [:+ {:gen/min 10 :max 20} :int]
      [:+ {:min 10 :gen/max 20} :int]
      [:+ {:gen/min 10 :gen/max 20} :int]

      [:* {:min 10 :max 20} :int]
      [:* {:gen/min 10 :max 20} :int]
      [:* {:min 10 :gen/max 20} :int]
      [:* {:gen/min 10 :gen/max 20} :int]

      [:vector {:min 1, :gen/min 10, :max 100, :gen/max 20} int?]
      [:set {:min 1, :gen/min 10, :max 100, :gen/max 20} int?]
      [:string {:min 1, :gen/min 10, :max 100, :gen/max 20}]))

  (testing ":+ enforces a minimum count of 1 generated elements"
    (every? #(<= 1 %) (map count (mg/sample [:+ :int] {:size 1000}))))

  (testing "invalid properties"
    (are [schema]
      (thrown? #?(:clj Exception, :cljs js/Error) (mg/sample schema {:size 1000}))

      ;; :gen/min less than :min
      [:vector {:min 11, :gen/min 10, :max 100, :gen/max 20} int?]
      [:set {:min 11, :gen/min 10, :max 100, :gen/max 20} int?]
      [:string {:min 11, :gen/min 10, :max 100, :gen/max 20}]

      ;; :gen/max over :max
      [:vector {:min 1, :gen/min 10, :max 100, :gen/max 200} int?]
      [:set {:min 1, :gen/min 10, :max 100, :gen/max 200} int?]
      [:string {:min 1, :gen/min 10, :max 100, :gen/max 200}])))

(deftest protocol-test
  (let [values #{1 2 3 5 8 13}
        schema (reify
                 m/Schema
                 (-parent [_] (reify m/IntoSchema (-type-properties [_])))
                 (-properties [_])
                 mg/Generator
                 (-generator [_ _] (gen/elements values)))]
    (is (every? values (mg/sample schema {:size 1000})))))

(deftest util-schemas-test
  (let [registry (merge (m/default-schemas) (mu/schemas))]
    (doseq [schema [[:merge {:title "merge"}
                     [:map [:x int?] [:y int?]]
                     [:map [:z int?]]]
                    [:union {:title "union"}
                     [:map [:x int?] [:y int?]]
                     [:map [:x string?]]]
                    [:select-keys {:title "select-keys"}
                     [:map [:x int?] [:y int?]]
                     [:x]]]
            :let [schema (m/schema schema {:registry registry})]]
      (is (every? (m/validator schema) (mg/sample schema {:size 1000}))))))

#?(:clj
   (deftest function-schema-test
     (doseq [?schema [[:=> [:cat int? int?] int?]
                      [:-> int? int? int?]]]
       (let [f (m/schema ?schema)
             {:keys [input output]} (m/-function-info f)]
         (is (every? #(m/validate output (apply % (mg/generate input))) (mg/sample f {:size 1000})))))

     (doseq [?schema [[:function
                       [:=> [:cat int?] int?]
                       [:=> [:cat int? int?] int?]]
                      [:function
                       [:-> int? int?]
                       [:-> int? int? int?]]]]
       (is (every? #(m/validate int? (apply % (mg/generate [:or [:cat int?] [:cat int? int?]]))) (mg/sample ?schema {:size 1000}))))))

(deftest recursive-schema-generation-test-307
  (let [sample (mg/generate [:schema {:registry {::A
                                                 [:cat
                                                  [:= ::a]
                                                  [:vector {;; :gen/min 2 makes this schema generate infinite values
                                                            #_#_:gen/min 2, :gen/max 2} [:ref ::A]]]}}
                             ::A] {:size 1, :seed 1})]
    (is (-> sample flatten count (> 1)))))

(deftest slow-recursive-test
  (let [schema [:schema {:registry {::A [:tuple [:= :A]]
                                    ::B [:tuple [:= :B]]
                                    ::C [:tuple [:= :C]]
                                    ::D [:tuple [:= :D]]
                                    ::E [:tuple [:= :E] [:ref ::item]]
                                    ::F [:tuple [:= :F] [:ref ::item]]
                                    ::G [:tuple [:= :G] [:ref ::item]]
                                    ::item [:multi {:dispatch first}
                                            [:A ::A]
                                            [:B ::B]
                                            [:C ::C]
                                            [:D ::D]
                                            [:E ::E]
                                            [:F ::F]
                                            [:G ::G]]}}
                ::E]
        valid? (m/validator schema)]
    (is (every? valid? (mg/sample schema {:size 10000})))))

(deftest recursive-distinct-col-test
  (is (not (every? empty? (mg/sample [:set
                                      {:registry {::foo :int}}
                                      [:ref ::foo]]
                                     {:size 1000})))))

;; note: use malli.generator-ast/generator-code to regenerate the raw test.check code in this test
(deftest recursive-gen-test
  (is (= '([] [] [] [] nil nil [[1 nil]] nil [[1 nil]] nil)
         (mg/sample [:schema {:registry {::cons [:maybe [:vector [:tuple pos-int? [:ref ::cons]]]]}}
                     [:ref ::cons]]
                    {:seed 1})
         (mg/sample (gen/recursive-gen
                     (fn [rec]
                       (gen/one-of [(gen/return nil)
                                    (gen/vector (gen/tuple (gen/large-integer* {:min 1}) rec))]))
                     (gen/one-of [(gen/return nil)
                                  (gen/return [])]))
                    {:seed 1})))
  (is (= '(-1 0 [-1 -1] [] 0 [[]] [] [[] []] -1 [])
         (mg/sample [:schema
                     {:registry {::data [:or
                                         ::int
                                         ::vector]
                                 ::int :int
                                 ::vector [:vector
                                           [:ref ::data]]}}
                     [:ref ::data]]
                    {:seed 0})
         (mg/sample (gen/recursive-gen
                     (fn [rec] (gen/one-of [(gen/large-integer* {})
                                            (gen/vector rec)]))
                     (gen/one-of [(gen/large-integer* {})
                                  (gen/return [])]))
                    {:seed 0})))
  (is (= '(-1 [[]] [] [[]] 1 0 -14 2 -1 [[0 1] [] 0 [] -1 -1 [] [-1] []])
         (mg/sample [:schema
                     {:registry {::data [:or
                                         ::int
                                         ::vector]
                                 ::int :int
                                 ::vector [:vector
                                           [:ref ::data]]}}
                     ::data]
                    {:seed 0})
         (mg/sample (gen/one-of [(gen/large-integer* {})
                                 (gen/vector (gen/recursive-gen
                                              (fn [rec] (gen/one-of [(gen/large-integer* {})
                                                                     (gen/vector rec)]))
                                              (gen/one-of [(gen/large-integer* {})
                                                           (gen/return [])])))])
                    {:seed 0})))
  (is (= '(nil nil ["ping" ["pong" nil]] ["ping" nil] nil ["ping" ["pong" nil]] ["ping" nil] ["ping" ["pong" nil]] nil ["ping" nil])
         (mg/sample [:schema
                     {:registry {::ping [:maybe [:tuple [:= "ping"] [:ref ::pong]]]
                                 ::pong [:maybe [:tuple [:= "pong"] [:ref ::ping]]]}}
                     [:ref ::ping]]
                    {:seed 0})
         (mg/sample (gen/recursive-gen
                     (fn [ping]
                       (gen/one-of [(gen/return nil)
                                    (gen/tuple (gen/return "ping")
                                               (gen/one-of [(gen/return nil)
                                                            (gen/tuple (gen/return "pong")
                                                                       ping)]))]))
                     (gen/one-of [(gen/return nil)
                                  (gen/tuple (gen/return "ping")
                                             (gen/one-of
                                              [(gen/return nil)
                                               (gen/tuple (gen/return "pong")
                                                          (gen/return nil))]))]))
                    {:seed 0})))
  (is (= '([["ping" ["pong" ["ping" ["pong" nil]]]] ["pong" ["ping" nil]]] [["ping" nil] ["pong" ["ping" ["pong" ["ping" nil]]]]]
           [["ping" ["pong" nil]] ["pong" ["ping" nil]]] [["ping" nil] ["pong" ["ping" nil]]] [["ping" nil] ["pong" nil]]
           [["ping" ["pong" nil]] ["pong" nil]] [["ping" nil] ["pong" ["ping" nil]]] [["ping" ["pong" nil]] ["pong" nil]]
           [["ping" ["pong" ["ping" ["pong" ["ping" ["pong" nil]]]]]] ["pong" nil]] [["ping" nil] ["pong" ["ping" nil]]])
         (mg/sample [:schema
                     {:registry {::ping [:tuple [:= "ping"] [:maybe [:ref ::pong]]]
                                 ::pong [:tuple [:= "pong"] [:maybe [:ref ::ping]]]}}
                     [:tuple [:ref ::ping] [:ref ::pong]]]
                    {:seed 0})
         (mg/sample (gen/tuple (gen/recursive-gen
                                (fn [ping]
                                  (gen/tuple (gen/return "ping")
                                             (gen/one-of [(gen/return nil)
                                                          (gen/tuple (gen/return "pong")
                                                                     (gen/one-of [(gen/return nil)
                                                                                  ping]))])))
                                (gen/tuple (gen/return "ping")
                                           (gen/one-of
                                            [(gen/return nil)
                                             (gen/tuple (gen/return "pong")
                                                        (gen/return nil))])))
                               (gen/recursive-gen
                                (fn [pong]
                                  (gen/tuple (gen/return "pong")
                                             (gen/one-of [(gen/return nil)
                                                          (gen/tuple (gen/return "ping")
                                                                     (gen/one-of [(gen/return nil)
                                                                                  pong]))])))
                                (gen/tuple (gen/return "pong")
                                           (gen/one-of
                                            [(gen/return nil)
                                             (gen/tuple (gen/return "ping")
                                                        (gen/return nil))]))))
                    {:seed 0})))
  (is (= '([["A" ["B" ["C" ["A" nil]]]] ["B" ["C" nil]]] [["A" nil] ["B" ["C" ["A" ["B" nil]]]]] [["A" ["B" nil]] ["B" ["C" nil]]]
           [["A" nil] ["B" ["C" nil]]] [["A" nil] ["B" nil]] [["A" ["B" nil]] ["B" nil]] [["A" nil] ["B" ["C" ["A" nil]]]]
           [["A" ["B" nil]] ["B" nil]] [["A" ["B" ["C" ["A" nil]]]] ["B" nil]] [["A" nil] ["B" ["C" nil]]])
         (mg/sample [:schema
                     {:registry {::A [:tuple [:= "A"] [:maybe [:ref ::B]]]
                                 ::B [:tuple [:= "B"] [:maybe [:ref ::C]]]
                                 ::C [:tuple [:= "C"] [:maybe [:ref ::A]]]}}
                     [:tuple [:ref ::A] [:ref ::B]]]
                    {:seed 0})
         (mg/sample (gen/tuple
                     (gen/recursive-gen
                      (fn [A]
                        (gen/tuple (gen/return "A")
                                   (gen/one-of [(gen/return nil)
                                                (gen/tuple (gen/return "B")
                                                           (gen/one-of [(gen/return nil)
                                                                        (gen/tuple (gen/return "C")
                                                                                   (gen/one-of [(gen/return nil)
                                                                                                A]))]))])))
                      (gen/tuple (gen/return "A")
                                 (gen/one-of [(gen/return nil)
                                              (gen/tuple (gen/return "B")
                                                         (gen/one-of [(gen/return nil)
                                                                      (gen/tuple (gen/return "C")
                                                                                 (gen/return nil))]))])))
                     (gen/recursive-gen
                      (fn [B]
                        (gen/tuple (gen/return "B")
                                   (gen/one-of [(gen/return nil)
                                                (gen/tuple (gen/return "C")
                                                           (gen/one-of [(gen/return nil)
                                                                        (gen/tuple (gen/return "A")
                                                                                   (gen/one-of [(gen/return nil)
                                                                                                B]))]))])))
                      (gen/tuple (gen/return "B")
                                 (gen/one-of [(gen/return nil)
                                              (gen/tuple (gen/return "C")
                                                         (gen/one-of [(gen/return nil)
                                                                      (gen/tuple (gen/return "A")
                                                                                 (gen/return nil))]))]))))
                    {:seed 0})))
  ;; linked list of ABC that never repeats
  (is (= '(["A" ["B" nil]] ["A" nil] ["A" nil] ["A" ["C" ["B" nil]]] ["A" ["C" nil]] ["A" nil] ["A" ["C" ["B" ["C" ["B" nil]]]]] ["A" nil] ["A" ["B" nil]] ["A" nil])
         (mg/sample [:schema
                     {:registry {::A [:tuple [:= "A"] [:maybe [:or [:ref ::B] [:ref ::C]]]]
                                 ::B [:tuple [:= "B"] [:maybe [:or [:ref ::C] [:ref ::A]]]]
                                 ::C [:tuple [:= "C"] [:maybe [:or [:ref ::A] [:ref ::B]]]]}}
                     [:ref ::A]]
                    {:seed 0})
         (mg/sample (gen/recursive-gen
                     (fn [A]
                       (gen/tuple (gen/return "A")
                                  (gen/one-of [(gen/return nil)
                                               (gen/one-of
                                                [(gen/recursive-gen
                                                  (fn [B]
                                                    (gen/tuple (gen/return "B")
                                                               (gen/one-of [(gen/return nil)
                                                                            (gen/one-of
                                                                             [(gen/tuple (gen/return "C")
                                                                                         (gen/one-of [(gen/return nil)
                                                                                                      (gen/one-of [A B])]))
                                                                              A])])))
                                                  (gen/tuple (gen/return "B")
                                                             (gen/one-of [(gen/return nil)
                                                                          (gen/one-of
                                                                           [(gen/tuple (gen/return "C")
                                                                                       (gen/one-of [(gen/return nil)
                                                                                                    A]))
                                                                            A])])))
                                                 (gen/recursive-gen
                                                  (fn [C]
                                                    (gen/tuple (gen/return "C")
                                                               (gen/one-of [(gen/return nil)
                                                                            (gen/one-of
                                                                             [A
                                                                              (gen/tuple (gen/return "B")
                                                                                         (gen/one-of [(gen/return nil)
                                                                                                      (gen/one-of [C A])]))])])))
                                                  (gen/tuple (gen/return "C")
                                                             (gen/one-of [(gen/return nil)
                                                                          (gen/one-of
                                                                           [A
                                                                            (gen/tuple (gen/return "B")
                                                                                       (gen/one-of [(gen/return nil)
                                                                                                    A]))])])))])])))
                     (gen/tuple (gen/return "A")
                                (gen/one-of [(gen/return nil)
                                             (gen/one-of
                                              [(gen/recursive-gen
                                                (fn [B]
                                                  (gen/tuple (gen/return "B")
                                                             (gen/one-of [(gen/return nil)
                                                                          (gen/tuple (gen/return "C")
                                                                                     (gen/one-of [(gen/return nil)
                                                                                                  B]))])))
                                                (gen/tuple (gen/return "B")
                                                           (gen/one-of [(gen/return nil)
                                                                        (gen/tuple (gen/return "C")
                                                                                   (gen/return nil))])))
                                               (gen/recursive-gen
                                                (fn [C]
                                                  (gen/tuple (gen/return "C")
                                                             (gen/one-of [(gen/return nil)
                                                                          (gen/tuple (gen/return "B")
                                                                                     (gen/one-of [(gen/return nil)
                                                                                                  C]))])))
                                                (gen/tuple (gen/return "C")
                                                           (gen/one-of [(gen/return nil)
                                                                        (gen/tuple (gen/return "B")
                                                                                   (gen/return nil))])))])])))
                    {:seed 0})))
  (is (= '([:E [:B]] [:E [:G [:D]]] [:E [:B]] [:E [:C]] [:E [:F [:D]]] [:E [:G [:B]]] [:E [:C]] [:E [:G [:C]]] [:E [:A]] [:E [:B]])
         (mg/sample [:schema {:registry {::A [:tuple [:= :A]]
                                         ::B [:tuple [:= :B]]
                                         ::C [:tuple [:= :C]]
                                         ::D [:tuple [:= :D]]
                                         ::E [:tuple [:= :E] [:ref ::item]]
                                         ::F [:tuple [:= :F] [:ref ::item]]
                                         ::G [:tuple [:= :G] [:ref ::item]]
                                         ;; only use :ref on ::item to make manual generator easier to create
                                         ::item [:multi {:dispatch first}
                                                 [:A ::A]
                                                 [:B ::B]
                                                 [:C ::C]
                                                 [:D ::D]
                                                 [:E ::E]
                                                 [:F ::F]
                                                 [:G ::G]]}}
                     ::E]
                    {:seed 0})
         (mg/sample (gen/tuple (gen/return :E)
                               (gen/recursive-gen
                                (fn [item]
                                  (gen/one-of [(gen/tuple (gen/return :A))
                                               (gen/tuple (gen/return :B))
                                               (gen/tuple (gen/return :C))
                                               (gen/tuple (gen/return :D))
                                               (gen/tuple (gen/return :E) item)
                                               (gen/tuple (gen/return :F) item)
                                               (gen/tuple (gen/return :G) item)]))
                                (gen/one-of [(gen/tuple (gen/return :A))
                                             (gen/tuple (gen/return :B))
                                             (gen/tuple (gen/return :C))
                                             (gen/tuple (gen/return :D))])))
                    {:seed 0})))
  (is (= '([:not true] [:not false] [:and [[:not false]]] [:or [[:not false]]] false [:and [true [:not true]]] [:and ()] [:or [[:or ()] false]] [:not true] [:and [[:not true]]])
         (mg/sample [:schema
                     {:registry
                      {::formula
                       [:or
                        :boolean
                        [:tuple [:enum :not] :boolean]
                        [:tuple [:enum :and] [:* [:ref ::formula]]]
                        [:tuple [:enum :or] [:* [:ref ::formula]]]]}}
                     [:ref ::formula]]
                    {:seed 0})
         (mg/sample (gen/recursive-gen
                     (fn [formula]
                       (gen/one-of [gen/boolean
                                    (gen/tuple (gen/return :not) gen/boolean)
                                    (gen/tuple (gen/return :and) (gen/vector formula))
                                    (gen/tuple (gen/return :or) (gen/vector formula))]))
                     (gen/one-of [gen/boolean
                                  (gen/tuple (gen/return :not) gen/boolean)
                                  (gen/tuple (gen/return :and) (gen/return ()))
                                  (gen/tuple (gen/return :or) (gen/return ()))]))
                    {:seed 0})))
  (is (= '([:not true] [:not false] [:and [true true]] [:or [true [:not true]]] false [:and [true [:not true] [:not true]]] [:not false] [:or [[:not true] [:not true] false]] [:not true] [:and [[:and [[:not false]]] [:not true]]])
         (mg/sample [:schema
                     {:registry
                      {::formula
                       [:or
                        :boolean
                        [:tuple [:enum :not] :boolean]
                        [:tuple [:enum :and] [:+ [:ref ::formula]]]
                        [:tuple [:enum :or] [:+ [:ref ::formula]]]]}}
                     [:ref ::formula]]
                    {:seed 0})
         (mg/sample (gen/recursive-gen
                     (fn [formula]
                       (gen/one-of [gen/boolean
                                    (gen/tuple (gen/return :not) gen/boolean)
                                    (gen/tuple (gen/return :and) (#'mg/gen-vector {:min 1} formula))
                                    (gen/tuple (gen/return :or) (#'mg/gen-vector {:min 1} formula))]))
                     (gen/one-of [gen/boolean
                                  (gen/tuple (gen/return :not) gen/boolean)]))
                    {:seed 0}))))

(deftest infinite-generator-test
  ;; equivalent to :never, which is infinite
  (is (thrown? #?(:clj Exception, :cljs js/Error)
               (mg/generate [:schema {:registry {::a [:ref ::a]}}
                             [:ref ::a]])))
  ;; equivalent to [:tuple :never], which is infinite
  (is (thrown? #?(:clj Exception, :cljs js/Error)
               (mg/generate [:schema {:registry {::a [:tuple [:ref ::a]]}}
                             [:ref ::a]])))
  ;; equivalent to [:maybe :never] == [:maybe [:maybe :never]] == ..., which is just :nil
  (is (every? nil? (mg/sample [:schema {:registry {::a [:maybe [:ref ::a]]}}
                               [:ref ::a]]))))

(deftest recursive-ref-shadowing-test
  ;; if the outer ::a shadowed the inner ::a, we'd have an unreachable generator
  (is (= 42
         (mg/generate [:schema {:registry {::a [:schema {:registry {::a [:= 42]}} [:ref ::a]]}}
                       [:ref ::a]])))
  ;; if the outer ::b shadowed the inner ::b, we'd have an unreachable generator
  (is (= 42
         (mg/generate [:schema {:registry {::a [:ref ::b] ;; (1)
                                           ::b [:schema {:registry {::b [:= 42]}}
                                                ;; (2)
                                                [:ref ::b]]}}
                       [:ref ::a]])))
  ;; the scopes at (1) and (2) are different, so no recursion is detected between them.
  ;; instead, the [:ref ::a] at (2) is the recursion point with itself.
  (is (boolean? (mg/generate [:schema {:registry {::a [:schema {:registry {::b [:= true]}}
                                                       ;; (2)
                                                       [:or [:ref ::a] [:ref ::b]]]}}
                              [:schema {:registry {::b [:= false]}}
                               ;; (1)
                               [:or [:ref ::a] [:ref ::b]]]])))
  ;; it's insufficient to identify refs just by their expansion. here, ::a
  ;; expands to [:ref ::b] twice at (1) and (2), so it looks like a recursion point, except
  ;; they are different ::b's! The generator would be unreachable if we use
  ;; this incorrect strategy.
  (is (= 42
         (mg/generate
          [:schema {:registry {::a [:ref ::b] ;; (1)
                               ::b [:schema {:registry {::a [:ref ::b] ;; (2)
                                                        ::b [:= 42]}}
                                    [:ref ::a]]}}
           [:ref ::a]])))
  ;; if the outer ::a shadowed the inner one, it would be equivalent to
  ;; [:maybe :never] == [:maybe [:maybe :never]] == ..., which is just :nil
  (is (not-every? nil? (mg/sample [:schema {:registry {::a [:schema {:registry {::a :int}} [:maybe [:ref ::a]]]}}
                                   [:ref ::a]]
                                  {:seed 0})))
  (is (int? (mg/generate [:schema
                          {:registry {::outer [:schema {:registry {::outer :int
                                                                   ::inner [:ref ::outer]}}
                                               [:ref ::inner]]}}
                          [:ref ::outer]]))))

(deftest recursive-gen-depth-test
  (is (some some? (mg/sample
                   [:schema {:registry {::cons [:or :nil [:tuple pos-int? [:ref ::cons]]]}}
                    [:ref ::cons]]
                   {:seed 0}))))

(deftest recursiven-gen-correspondence-test
  (dotimes [seed 10]
    (testing seed
      (testing "initial :ref"
        (is (= (mg/sample
                (mg/generator
                 [:schema {:registry {::cons [:or :nil [:tuple [:ref ::cons]]]}}
                  [:ref ::cons]])
                {:seed seed
                 :size 10})
               (mg/sample (gen/recursive-gen
                           (fn [rec]
                             (gen/one-of [(gen/return nil)
                                          (gen/tuple rec)]))
                           (gen/return nil))
                          {:seed seed
                           :size 10}))))
      (testing "no initial :ref"
        (is (= (mg/sample
                (mg/generator
                 [:schema {:registry {::cons [:or :nil [:tuple [:ref ::cons]]]}}
                  ::cons])
                {:seed seed
                 :size 10})
               (mg/sample (gen/one-of
                           [(gen/return nil)
                            (gen/tuple (gen/recursive-gen
                                        (fn [rec]
                                          (gen/one-of [(gen/return nil)
                                                       (gen/tuple rec)]))
                                        (gen/return nil)))])
                          {:seed seed
                           :size 10})))))))

(deftest recursive-gen-cache-test
  (let [s (m/schema [:schema {:registry {::cons [:or :nil [:tuple [:ref ::cons]]]}}
                     [:ref ::cons]])]
    (is (identical? (mg/generator s)
                    (mg/generator s)))))

(deftest recursive-gen-exponential-growth-test
  (is (= [{0 1}
          {0 6, 2 1, 3 3}
          {0 54, 2 26, 3 11, 4 7, 5 2}
          {0 513, 2 264, 3 136, 4 67, 5 18, 6 1, 7 1}
          {0 5066, 2 2635, 3 1349, 4 641, 5 245, 6 53, 7 11}]
         (for [size-pow (range 5)]
           (into (sorted-map)
                 (frequencies
                  (map
                   (comp count flatten)
                   (mg/sample
                    [:schema {:registry {::cons [:maybe [:tuple pos-int? [:ref ::cons]]]}}
                     ::cons]
                    {:seed 0
                     :size (Math/pow 10 size-pow)}))))))))

;; tests gen/sized usage
(deftest generator-min-only-test
  (is (some seq (mg/sample [:string {:min 0}]
                           {:seed 0})))
  (is (some #(< 1 (count %)) (mg/sample [:string {:min 1}]
                                        {:seed 0})))
  (is (some seq (mg/sample [:vector {:min 0} :any]
                           {:seed 0})))
  (is (some #(< 1 (count %)) (mg/sample [:vector {:min 1} :any]
                                        {:seed 0}))))

(deftest and-schema-simplify-test
  (is (= '(-1 0 [[] []] [] 0 [[]] [] [[] []] -1 [])
         (mg/sample [:schema {:registry {::A [:or :int [:vector [:and [:ref ::A] vector?]]]}}
                     [:ref ::A]]
                    {:seed 0})
         (mg/sample (gen/recursive-gen
                     (fn [A]
                       (gen/one-of [(gen/large-integer* {})
                                    (gen/vector (gen/such-that vector? A 100))]))
                     (gen/one-of [(gen/large-integer* {})
                                  (gen/return [])]))
                    {:seed 0}))))

(deftest map-schema-simplify-test
  (testing "simplify optional key"
    (is (= '({} {} {:rec {}} {:rec {}} {} {:rec {}} {} {:rec {}} {} {:rec {}})
           (mg/sample [:schema {:registry {::rec [:map [:rec {:optional true} [:ref ::rec]]]}} [:ref ::rec]]
                      {:seed 0})
           (mg/sample (gen/recursive-gen
                       (fn [rec]
                         (gen/fmap (fn [opt] (into {} opt))
                                   (gen/tuple
                                    (gen/one-of [(gen/return nil)
                                                 (gen/fmap (fn [v] [:rec v]) rec)]))))
                       (gen/fmap (fn [opt] (into {} opt))
                                 (gen/return nil)))
                      {:seed 0}))))
  (testing "simplify required key"
    (is (= '([] [] [{:rec []} {:rec []}] [{:rec []} {:rec []}] [] [{:rec []}] [] [{:rec []} {:rec []}] [{:rec []}] [{:rec [{:rec []} {:rec []}]} {:rec [{:rec []} {:rec []}]}])
           (mg/sample [:schema {:registry {::rec [:vector [:map [:rec [:ref ::rec]]]]}} [:ref ::rec]]
                      {:seed 0})
           (mg/sample (gen/recursive-gen
                       (fn [rec]
                         (gen/vector
                          (gen/fmap (fn [req] (into {} req))
                                    (gen/tuple (gen/fmap (fn [v] [:rec v]) rec)))))
                       (gen/return []))
                      {:seed 0})))))

(deftest map-of-schema-simplify-test
  (testing "empty maps allowed if :min is not positive"
    (is (= '({} {} {{} {}} {{} {}} {} {{} {}} {} {{} {}} {{} {}} {{{} {}} {{} {}}, {} {}})
           (mg/sample [:schema {:registry {::rec [:map-of [:ref ::rec] [:ref ::rec]]}} [:ref ::rec]]
                      {:seed 0})
           (mg/sample [:schema {:registry {::rec [:map-of {:min 0} [:ref ::rec] [:ref ::rec]]}} [:ref ::rec]]
                      {:seed 0})
           (mg/sample (gen/recursive-gen
                       (fn [rec]
                         (gen/fmap #(into {} %)
                                   (gen/vector-distinct-by first (gen/tuple rec rec))))
                       (gen/return {}))
                      {:seed 0}))))
  (testing "cannot generate empty for positive :min"
    (try (mg/generate [:schema {:registry {::rec [:map-of {:min 1} [:ref ::rec] [:ref ::rec]]}} [:ref ::rec]]
                      {:seed 0})
         (is false)
         (catch #?(:clj Exception, :cljs js/Error) e
           (is (re-find #":malli\.generator/unsatisfiable-schema"
                        (ex-message e)))
           (is (= [:ref :malli.generator-test/rec] (-> e ex-data :data :schema m/form))))))
  (testing "can generate empty regardless of :max"
    (is (= '({{} {}} {{} {}} {{} {}} {{} {}} {} {{} {}} {} {{} {}} {{} {}} {{{} {}} {{} {}}, {} {}})
           (mg/sample [:schema {:registry {::rec [:map-of {:max 3} [:ref ::rec] [:ref ::rec]]}} [:ref ::rec]]
                      {:seed 0})
           (mg/sample [:schema {:registry {::rec [:map-of {:min 0 :max 3} [:ref ::rec] [:ref ::rec]]}} [:ref ::rec]]
                      {:seed 0})))))

(deftest vector-schema-simplify-test
  (testing "empty vectors allowed if :min is not positive"
    (is (= '([] [] [[] []] [[] []] [] [[]] [] [[] []] [[]] [[[] []] [[] []]])
           (mg/sample [:schema {:registry {::rec [:vector [:ref ::rec]]}} [:ref ::rec]]
                      {:seed 0})
           (mg/sample [:schema {:registry {::rec [:vector {:min 0} [:ref ::rec]]}} [:ref ::rec]]
                      {:seed 0})
           (mg/sample (gen/recursive-gen
                       (fn [rec]
                         (gen/vector rec))
                       (gen/return []))
                      {:seed 0}))))
  (testing "no empty vectors allowed with positive :min"
    (is (= [nil nil [nil nil nil] [nil] nil [nil nil] nil [nil nil nil] nil [[nil nil nil]]]
           (mg/sample [:schema {:registry {::rec [:maybe [:vector {:min 1} [:ref ::rec]]]}} [:ref ::rec]]
                      {:seed 0})
           (mg/sample (gen/recursive-gen
                       (fn [rec]
                         (gen/one-of [(gen/return nil)
                                      (gen/sized #(gen/vector rec 1 (+ 1 %)))]))
                       (gen/one-of [(gen/return nil)]))
                      {:seed 0}))))
  (testing "can generate empty regardless of :max"
    (is (= '([[] [] [] []] [[] [] [] []] [[] [] [] [] [] [] []] [[] [] [] [] [] [] [] []] [[] []]
             [[] [] [] [] []] [] [[] [] [] [] [] [] [] []] [[] [] [] []]
             [[[] [] [] [] [] [] [] []] [[] [] []] [] [[] [] [] []] [] [[] [] [] [] [] [] [] []] [[] [] []]])
           (mg/sample [:schema {:registry {::rec [:vector {:max 10} [:ref ::rec]]}} [:ref ::rec]]
                      {:seed 0})
           (mg/sample [:schema {:registry {::rec [:vector {:min 0 :max 10} [:ref ::rec]]}} [:ref ::rec]]
                      {:seed 0})))))

(deftest sequential-schema-simplify-test
  (testing "empty sequentials allowed"
    (is (= '([] [] [[] []] [[] []] [] [[]] [] [[] []] [[]] [[[] []] [[] []]])
           (mg/sample [:schema {:registry {::rec [:sequential [:ref ::rec]]}} [:ref ::rec]]
                      {:seed 0})
           (mg/sample (gen/recursive-gen
                       (fn [rec]
                         (gen/vector rec))
                       (gen/return []))
                      {:seed 0}))))
  (testing "no empty sequentials allowed with positive :min"
    (is (= [nil nil [nil nil nil] [nil] nil [nil nil] nil [nil nil nil] nil [[nil nil nil]]]
           (mg/sample [:schema {:registry {::rec [:maybe [:sequential {:min 1} [:ref ::rec]]]}} [:ref ::rec]]
                      {:seed 0})
           (mg/sample (gen/recursive-gen
                       (fn [rec]
                         (gen/one-of [(gen/return nil)
                                      (gen/sized #(gen/vector rec 1 (+ 1 %)))]))
                       (gen/one-of [(gen/return nil)]))
                      {:seed 0}))))
  (testing "can generate empty regardless of :max"
    (is (= '([[] [] [] []] [[] [] [] []] [[] [] [] [] [] [] []] [[] [] [] [] [] [] [] []] [[] []]
             [[] [] [] [] []] [] [[] [] [] [] [] [] [] []] [[] [] [] []]
             [[[] [] [] [] [] [] [] []] [[] [] []] [] [[] [] [] []] [] [[] [] [] [] [] [] [] []] [[] [] []]])
           (mg/sample [:schema {:registry {::rec [:sequential {:max 10} [:ref ::rec]]}} [:ref ::rec]]
                      {:seed 0})
           (mg/sample [:schema {:registry {::rec [:sequential {:min 0 :max 10} [:ref ::rec]]}} [:ref ::rec]]
                      {:seed 0})))))

(deftest set-schema-simplify-test
  (testing "empty sets allowed if :min is not positive"
    (is (= '(#{} #{} #{#{}} #{#{}} #{} #{#{}} #{} #{#{}} #{#{}} #{#{} #{#{}}})
           (mg/sample [:schema {:registry {::rec [:set [:ref ::rec]]}} [:ref ::rec]]
                      {:seed 0})
           (mg/sample [:schema {:registry {::rec [:set {:min 0} [:ref ::rec]]}} [:ref ::rec]]
                      {:seed 0})
           (mg/sample (gen/recursive-gen
                       (fn [rec]
                         (gen/fmap set (gen/vector-distinct rec)))
                       (gen/return #{}))
                      {:seed 0}))))
  (testing "no empty sets allowed with positive :min"
    (is (= '(nil nil #{nil} #{nil} nil #{nil} nil #{nil} nil #{#{nil}})
           (mg/sample [:schema {:registry {::rec [:maybe [:set {:min 1} [:ref ::rec]]]}} [:ref ::rec]]
                      {:seed 0})
           (mg/sample (gen/recursive-gen
                       (fn [rec]
                         (gen/one-of [(gen/return nil)
                                      (gen/fmap set (gen/vector-distinct rec {:min-elements 1}))]))
                       (gen/one-of [(gen/return nil)]))
                      {:seed 0}))))
  (testing "can generate empty regardless of :max"
    (is (= '(#{#{}} #{#{}} #{#{}} #{#{}} #{#{}} #{#{}} #{} #{#{}} #{#{}} #{#{} #{#{}}})
           (mg/sample [:schema {:registry {::rec [:set {:max 10} [:ref ::rec]]}} [:ref ::rec]]
                      {:seed 0})
           (mg/sample [:schema {:registry {::rec [:set {:min 0 :max 10} [:ref ::rec]]}} [:ref ::rec]]
                      {:seed 0})))))

(defn alphanumeric-char? [c]
  {:pre [(char? c)]}
  (let [int (fn [c]
              #?(:clj (int c)
                 :cljs (.charCodeAt c 0)))
        i (int c)]
    (or (<= (int \a) i (int \z))
        (<= (int \A) i (int \Z))
        (<= (int \0) i (int \9)))))

(deftest alphanumeric-char?-test
  (is (alphanumeric-char? \a))
  (is (not (alphanumeric-char? \-))))

(defn alphanumeric-string? [s]
  {:pre [(string? s)]}
  (every? alphanumeric-char? s))

(deftest string-gen-alphanumeric-test
  (dotimes [seed 100]
    (testing (pr-str seed)
      (testing "(and min (= min max))"
        (is (alphanumeric-string?
             (mg/generate [:string {:min 10, :max 10}]
                          {:seed seed}))))
      (testing "(and min max)"
        (is (alphanumeric-string?
             (mg/generate [:string {:min 10, :max 20}]
                          {:seed seed}))))
      (testing "min"
        (is (alphanumeric-string?
             (mg/generate [:string {:min 10}]
                          {:seed seed}))))
      (testing "max"
        (is (alphanumeric-string?
             (mg/generate [:string {:max 20}]
                          {:seed seed}))))
      (testing ":else"
        (is (alphanumeric-string?
             (mg/generate [:string {}]
                          {:seed seed})))))))

(deftest non-empty-vector-generator-test
  (is (= [:.+ [1]]
         (mg/generate [:and [:cat {:gen/fmap vec} :keyword [:* :any]] vector?]
                      {:size 1
                       :seed 2})))
  (doseq [v (mg/sample [:and [:cat {:gen/fmap vec} :keyword [:* :any]] vector?]
                       {:seed 2})]
    (is (vector? v))
    (is (seq v))))

(deftest map-of-min-max-test
  (is (empty? (remove #(<= 2 (count %))
                      (mg/sample [:map-of {:min 2} [:enum 1 2 3] :any]
                                 {:size 100
                                  :seed 3}))))
  (is (empty? (remove #(<= (count %) 2)
                      (mg/sample [:map-of {:max 2} [:enum 1 2 3] :any]
                                 {:size 100
                                  :seed 3}))))
  (is (empty? (remove #(<= 2 (count %) 3)
                      (mg/sample [:map-of {:min 2 :max 3} [:enum 1 2 3] :any]
                                 {:size 100
                                  :seed 3})))))

(deftest such-that-generator-failure-test
  (is (thrown-with-msg?
       #?(:clj Exception, :cljs js/Error)
       #":malli\.generator/such-that-failure"
       (mg/generate [:not :any])))
  (is (thrown-with-msg?
       #?(:clj Exception, :cljs js/Error)
       #":malli\.generator/distinct-generator-failure"
       (mg/generate [:set {:min 2} [:= 1]])))
  (is (thrown-with-msg?
       #?(:clj Exception, :cljs js/Error)
       #":malli\.generator/distinct-generator-failure"
       (mg/generate [:map-of {:min 2} [:= 1] :any])))
  (is (thrown-with-msg?
       #?(:clj Exception, :cljs js/Error)
       #":malli\.generator/such-that-failure"
       (mg/generate [:and pos? neg?]))))

(deftest seqable-every-generator-test
  (doseq [op [:seqable :every]]
    (testing op
      #?(:clj  (is (= '[[nil ()]
                        ["Eduction" (0)]
                        ["PersistentHashSet" ()]
                        ["Object[]" (0)]
                        ["PersistentVector" (-2 2 0 1)]
                        ["PersistentVector" (1 -2)]
                        ["PersistentVector" (-9)]
                        ["PersistentVector" (3 -49 -4)]
                        ["PersistentVector" (-23 1 82)]
                        ["Eduction" (126 -24 -236 0 -18 0 0 2 -1)]]
                      (mapv (juxt #(some-> (class %) .getSimpleName) sequence) (mg/sample [op :int] {:seed 0}))))
         :cljs (is (= '[() (0) () (0) (-2 2 0 1) (1 -2) (-9) (3 -49 -4) (-23 1 82) (126 -24 -236 0 -18 0 0 2 -1)]
                      (mapv sequence (mg/sample [op :int] {:seed 0})))))
      (is (= '({-1 false}
               {-4399 true, 59 false, -4049 false, -49 false, -1 false, 15 false, -967 false, -3 false, -674 false, 2730 true, -2104 false, 3 false, -444 true, 8 false}
               {119 true, 1324 false, 7276 false, -2410 true})
             (filter map? (mg/sample [op [:tuple :int :boolean]] {:seed 1 :size 30})))))))

(deftest double-with-long-min-test
  (is (m/validate :double (shrink [:double {:min 3}])))
  (is (= 3.0 (shrink [:double {:min 3}]))))

(deftest multi-keyword-dispatch-test
  (testing "keyword dispatch value accumulates to generated value"
    (let [schema [:multi {:dispatch :type}
                  ["duck" :map]
                  ["boss" :map]]]
      (is (every? #{{:type "duck"} {:type "boss"}} (mg/sample schema)))
      (is (every? (m/validator schema) (mg/sample schema)))))

  (testing "non keyword doesn't accumulate data"
    (let [schema [:multi {:dispatch (fn [x] (:type x))}
                  ["duck" :map]
                  ["boss" :map]]]
      (is (every? #{{}} (mg/sample schema)))
      (is (not (every? (m/validator schema) (mg/sample schema))))))

  (testing "::m/default works too"
    (let [schema [:multi {:dispatch :type}
                  ["duck" :map]
                  [::m/default [:= "boss"]]]]
      (is (every? #{{:type "duck"} "boss"} (mg/sample schema)))
      (is (every? (m/validator schema) (mg/sample schema)))))

  (testing "works with nil & {} too"
    (let [schema [:multi {:dispatch :type}
                  [nil :map]
                  [{} :map]]]
      (is (every? #{{:type nil} {:type {}}} (mg/sample schema)))
      (is (every? (m/validator schema) (mg/sample schema))))))

(deftest seqable-generates-non-empty-with-positive-min-test
  (is (seq (mg/generate [:seqable {:min 4 :max 4} :int] {:seed 0})))
  (doseq [_ (range 100)
          v (mg/sample [:seqable {:min 1} :any])]
    (is (seq v))))

(deftest empty?-generator-test
  (is (every? empty? (mg/sample empty?))))
