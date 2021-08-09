(ns malli.generator-test
  (:require [clojure.test :refer [deftest testing is are]]
            [clojure.test.check.properties :refer [for-all]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [malli.json-schema-test :as json-schema-test]
            [malli.generator :as mg]
            [malli.core :as m]
            [malli.util :as mu]))

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
                    [:string {:min 1, :max 4}]
                    [:int {:min 1, :max 4}]
                    [:double {:min 0.0, :max 1.0}]
                    :boolean
                    :keyword
                    :symbol
                    :qualified-keyword
                    :qualified-symbol]]
      (is (every? (partial m/validate schema) (mg/sample schema {:size 1000})))))

  (testing "double properties"
    (let [infinity? #(or (= % ##Inf)
                         (= % ##-Inf))
          NaN? (fn [x]
                 (#?(:clj  Double/isNaN
                     :cljs js/isNaN)
                   x))
          special? #(or (NaN? %)
                        (infinity? %))
          test-presence (fn [f options]
                          (some f (mg/sample [:double options]
                                             {:size 1000})))]
      (is (test-presence infinity? {:gen/infinite? true}))
      (is (test-presence NaN? {:gen/NaN? true}))
      (is (test-presence special? {:gen/infinite? true
                                   :gen/NaN? true}))
      (is (not (test-presence special? nil)))))

  (testing "map entries"
    (is (= {:korppu "koira"
            :piilomaan "pikku aasi"
            :muuli "mukkelis"}
           (mg/generate [:map {:gen/fmap '#(assoc % :korppu "koira")}
                         [:piilomaan {:gen/fmap '(partial str "pikku ")} [:string {:gen/elements ["aasi"]}]]
                         [:muuli {:gen/elements ["mukkelis"]} [:string {:gen/elements ["???"]}]]]))))

  (testing "ref"
    (testing "recursion"
      (let [schema [:schema {:registry {::cons [:maybe [:tuple int? [:ref ::cons]]]}}
                    ::cons]]
        (is (every? (partial m/validate schema) (mg/sample schema {:size 100})))))
    (testing "mutual recursion"
      (let [schema [:schema
                    {:registry {::ping [:maybe [:tuple [:= "ping"] [:ref ::pong]]]
                                ::pong [:maybe [:tuple [:= "pong"] [:ref ::ping]]]}}
                    ::ping]]
        (is (every? (partial m/validate schema) (mg/sample schema {:size 100})))))
    (testing "recursion limiting"
      (are [schema]
        (is (every? (partial m/validate schema) (mg/sample schema {:size 100})))

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

  #?(:clj (testing "regex"
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
        (is (mg/generator schema {::m/disable-sci false})))))

  (testing "generator override"
    (testing "without generator"
      (let [schema [:fn {:gen/fmap '(fn [_] (rand-int 10))}
                    '(fn [x] (<= 0 x 10))]
            generator (mg/generator schema)]
        (dotimes [_ 100]
          (m/validate schema (mg/generate generator)))))
    (testing "with generator"
      (is (re-matches #"kikka_\d+" (mg/generate [:and {:gen/fmap '(partial str "kikka_")} pos-int?])))))

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
      (is (every? #(<= 10 % 20) (map count (mg/sample schema {:size 1000}))))

      [:vector {:min 10, :max 20} int?]
      [:set {:min 10, :max 20} int?]
      [:string {:min 10, :max 20}]

      [:vector {:gen/min 10, :max 20} int?]
      [:set {:gen/min 10, :max 20} int?]
      [:string {:gen/min 10, :max 20}]

      [:vector {:min 10, :gen/max 20} int?]
      [:set {:min 10, :gen/max 20} int?]
      [:string {:gen/min 10, :max 20}]

      [:vector {:min 1, :gen/min 10, :max 100, :gen/max 20} int?]
      [:set {:min 1, :gen/min 10, :max 100, :gen/max 20} int?]
      [:string {:min 1, :gen/min 10, :max 100, :gen/max 20}]))

  (testing "invalid properties"
    (are [schema]
      (is (thrown? #?(:clj Exception, :cljs js/Error) (mg/sample schema {:size 1000})))

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
      (is (every? (partial m/validate schema) (mg/sample schema {:size 1000}))))))

#?(:clj
   (deftest function-schema-test
     (let [=> (m/schema [:=> [:cat int? int?] int?])
           {:keys [input output]} (m/-function-info =>)]
       (is (every? #(m/validate output (apply % (mg/generate input))) (mg/sample => {:size 1000}))))

     (let [=> (m/schema [:function [:=> [:cat int?] int?] [:=> [:cat int? int?] int?]])]
       (is (every? #(m/validate int? (apply % (mg/generate [:or [:cat int?] [:cat int? int?]]))) (mg/sample => {:size 1000}))))))

(deftest recursive-schema-generation-test-307
  (let [sample (mg/generate [:schema {:registry {::A
                                                 [:cat
                                                  [:= ::a]
                                                  [:vector {:gen/min 2, :gen/max 2} [:ref ::A]]]}}
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

(comment
  (gen/one-of
    [(gen/return nil)
     (gen/tuple (gen/return "ping")
                (gen/recursive-gen
                  (fn [pong])))
     ])

  [:schema
   {:registry {::ping [:maybe [:tuple [:= "ping"] [:ref ::pong]]]
               ::pong [:maybe [:tuple [:= "pong"] [:ref ::ping]]]}}
   ::ping]
  (->> (gen/sample
         (gen/recursive-gen
           (fn [ping]
             (gen/one-of 
               [(gen/return nil)
                (gen/tuple (gen/return "ping")
                           (gen/recursive-gen
                             (fn [pong]
                               (gen/one-of
                                 [(gen/return nil)
                                  (gen/tuple (gen/return "pong")
                                             ping)]))
                             (gen/one-of
                               [(gen/return nil)
                                (gen/tuple (gen/return "pong")
                                           (gen/return nil))])))]))
           (gen/one-of
             [(gen/return nil)
              (gen/tuple (gen/return "ping")
                         (gen/return nil))]))
         1000)
       (drop 75))

  [:schema
   {:registry {::ping [:tuple [:= "ping"] [:maybe [:ref ::pong]]]
               ::pong [:tuple [:= "pong"] [:maybe [:ref ::ping]]]}}
   ::ping]
  (->> (gen/sample
         (gen/recursive-gen
           (fn [ping]
             (gen/tuple (gen/return "ping")
                        (gen/one-of
                          [(gen/return nil)
                           (gen/recursive-gen
                             (fn [pong]
                               (gen/tuple (gen/return "pong")
                                          ping))
                             (gen/tuple (gen/return "pong")
                                        (gen/return nil)))])))
           (gen/tuple (gen/return "ping")
                      (gen/return nil)))
         100)
       (drop 75))

  [:schema
   {:registry {::A [:tuple [:= "A"] [:maybe [:ref ::B]]]
               ::B [:tuple [:= "B"] [:maybe [:ref ::C]]]
               ::C [:tuple [:= "C"] [:maybe [:ref ::A]]]}}
   [:tuple ::A ::B]]
  (->> (gen/sample
         (gen/tuple
           (gen/recursive-gen
             (fn [A]
               (gen/tuple (gen/return "A")
                          (gen/one-of
                            [(gen/return nil)
                             (gen/recursive-gen
                               (fn [B]
                                 (gen/tuple (gen/return "B")
                                            (gen/recursive-gen
                                              (fn [C]
                                                (gen/tuple (gen/return "C")
                                                           A))
                                              (gen/tuple (gen/return "C")
                                                         (gen/return nil)))))
                               (gen/tuple (gen/return "B")
                                          (gen/return nil)))])))
             (gen/tuple (gen/return "A")
                        (gen/return nil)))
           (gen/recursive-gen
             (fn [B]
               (gen/tuple (gen/return "B")
                          (gen/recursive-gen
                            (fn [C]
                              (gen/tuple (gen/return "C")
                                         (gen/recursive-gen
                                           (fn [A]
                                             (gen/tuple (gen/return "A")
                                                        C))
                                           (gen/tuple (gen/return "A")
                                                      (gen/return nil)))))
                            (gen/tuple (gen/return "C")
                                       (gen/return nil)))))
             (gen/tuple (gen/return "B")
                        (gen/return nil)))
           )
         100)
       (drop 75))

  ;; linked list of ABC that never repeats
  [:schema
   {:registry {::A [:tuple [:= "A"] [:maybe [:or [:ref ::B] [:ref ::C]]]]
               ::B [:tuple [:= "B"] [:maybe [:or [:ref ::C] [:ref ::A]]]]
               ::C [:tuple [:= "C"] [:maybe [:or [:ref ::A] [:ref ::B]]]]}}
   [:tuple ::A]]
  (->> (gen/sample
         (gen/tuple
           (gen/recursive-gen
             (fn [A]
               (gen/tuple (gen/return "A")
                          (gen/one-of
                            [(gen/return nil)
                             (gen/recursive-gen
                               (fn [B]
                                 (gen/tuple (gen/return "B")
                                            (gen/one-of
                                              [(gen/return nil)
                                               (gen/recursive-gen
                                                 (fn [C]
                                                   (gen/tuple (gen/return "C")
                                                              (gen/one-of
                                                                [(gen/return nil)
                                                                 A
                                                                 B])))
                                                 (gen/tuple (gen/return "C")
                                                            (gen/return nil)))
                                               A])))
                               (gen/tuple (gen/return "B")
                                          (gen/return nil)))
                             (gen/recursive-gen
                               (fn [C]
                                 (gen/tuple (gen/return "C")
                                            (gen/one-of
                                              [(gen/return nil)
                                               A
                                               (gen/recursive-gen
                                                 (fn [B]
                                                   (gen/tuple (gen/return "B")
                                                              (gen/one-of
                                                                [(gen/return nil)
                                                                 (gen/recursive-gen
                                                                   (fn [C]
                                                                     (gen/tuple (gen/return "C")
                                                                                (gen/one-of
                                                                                  [(gen/return nil)
                                                                                   A
                                                                                   B])))
                                                                   (gen/tuple (gen/return "C")
                                                                              (gen/return nil)))
                                                                 A])))
                                                 (gen/tuple (gen/return "B")
                                                            (gen/return nil)))])))
                               (gen/tuple (gen/return "C")
                                          (gen/return nil)))])))
             (gen/tuple (gen/return "A")
                        (gen/return nil))))
         100)
       (drop 75))

  [:schema
   {:registry {::data    [:or
                          ::int
                          ::vector]
               ::int     :int
               ::vector  [:vector
                          [:ref ::data]]}}
   ::data]
  (->> (gen/sample
         (gen/recursive-gen
           (fn [data]
             (gen/vector
               data))
           gen/large-integer)
         100)
       (drop 75))

  [:schema
   {:registry {::ping [:tuple [:= "ping"] [:maybe [:ref ::pong]]]
               ::pong [:tuple [:= "pong"] [:maybe [:ref ::ping]]]}}
   [:tuple ::ping ::pong]]
  (->> (gen/sample
         (gen/tuple
           (gen/recursive-gen
             (fn [ping]
               (gen/tuple (gen/return "ping")
                          (gen/one-of
                            [(gen/return nil)
                             (gen/recursive-gen
                               (fn [pong]
                                 (gen/tuple (gen/return "pong")
                                            ping))
                               (gen/tuple (gen/return "pong")
                                          (gen/return nil)))])))
             (gen/tuple (gen/return "ping")
                        (gen/return nil)))
           (gen/recursive-gen
             (fn [pong]
               (gen/tuple (gen/return "pong")
                          (gen/one-of
                            [(gen/return nil)
                             (gen/recursive-gen
                               (fn [ping]
                                 (gen/tuple (gen/return "ping")
                                            pong))
                               (gen/tuple (gen/return "ping")
                                          (gen/return nil)))])))
             (gen/tuple (gen/return "pong")
                        (gen/return nil))))
         100)
       (drop 75))
  )

#_
(let [id (gensym)]
 (with-meta
   [:schema
    {:registry {::ping [:maybe [:tuple [:= "ping"]
                                (with-meta [:ref ::pong]
                                           {::registry-id id})]]
                ::pong [:maybe [:tuple [:= "pong"]
                                (with-meta [:ref ::ping]
                                           {::registry-id id})]]}}
    ::ping]
   {::registry-id id}))

(deftest scalar-container-schema-test
  (let [test-cases [{:schema [:schema
                              {:registry {::ping [:maybe [:tuple [:= "ping"] [:ref ::pong]]]
                                          ::pong [:maybe [:tuple [:= "pong"] [:ref ::ping]]]}}
                              ::ping]
                     #_(comment
                         (gen/recursive-gen
                           (fn [ping]
                             ;; container
                             (gen/one-of 
                               [(gen/return nil)
                                (gen/tuple (gen/return "ping")
                                           (gen/one-of
                                             [(gen/return nil)
                                              (gen/tuple (gen/return "pong")
                                                         ping)]))]))
                           ;; scalar
                           (gen/one-of
                             [(gen/return nil)
                              (gen/tuple (gen/return "ping")
                                         (gen/return nil))])))
                     :scalar-schema [:maybe [:tuple [:= "ping"] :nil]]
                     :container-schema [:maybe
                                        [:tuple [:= "ping"]
                                         [:schema
                                          {:registry {::pong [:maybe [:tuple [:= "pong"] [:ref ::ping]]]}}
                                          ::pong]]]}
                    {:schema [:schema
                              {:registry {::ping [:tuple [:= "ping"] [:maybe [:ref ::pong]]]
                                          ::pong [:tuple [:= "pong"] [:maybe [:ref ::ping]]]}}
                              ::ping]
                     :scalar-schema [:tuple [:= "ping"] [:maybe [:tuple [:= "pong"] :nil]]]
                     :container-schema [:tuple
                                        [:= "ping"]
                                        [:schema
                                         {:registry {::ping [:tuple [:= "ping"] [:maybe [:ref ::pong]]]
                                                     ::pong [:tuple [:= "pong"] [:maybe [:ref ::ping]]]}}
                                         ::ping]]}
                    {:schema [:schema
                              {:registry {::data    [:or
                                                     ::int
                                                     ::vector]
                                          ::int     :int
                                          ::vector  [:vector
                                                     [:ref ::data]]}}
                              ::data]
                     :scalar-schema :int
                     :container-schema [:vector
                                        [:schema
                                         {:registry {::data    [:or
                                                                ::int
                                                                ::vector]
                                                     ::int     :int
                                                     ::vector  [:vector
                                                                [:ref ::data]]}}
                                         ::data]]}]]
    (doseq [{:keys [schema scalar-schema container-schema]} test-cases]
      (is (= scalar-schema (m/form
                             (mg/schema->scalar-schema schema
                                                       {}))))
      #_(is (= container-schema (mg/schema->container-schema schema
                                                             {}))))))
