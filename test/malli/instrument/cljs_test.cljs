(ns malli.instrument.cljs-test
  (:require [cljs.test :refer [deftest is testing]]
            [malli.instrument.fn-schemas :as schemas :refer [VecOfInts sum-nums sum-nums2 str-join str-join2 str-join3 str-join4]]
            [malli.instrument.fn-schemas2 :as schemas-2]
            [malli.core :as m]
            [malli.experimental :as mx]
            [malli.instrument.cljs :as mi]))

(defn plus [x] (inc x))
(m/=> plus [:=> [:cat :int] [:int {:max 6}]])

(def small-int [:int {:max 6}])

(defn minus
  "kukka"
  {:malli/schema [:=> [:cat :int] [:int {:min 6}]]
   :malli/scope #{:input :output}}
  [x] (dec x))

(defn multi-arity-fn
  {:malli/schema
   [:function
    [:=> [:cat] [:int]]
    [:=> [:cat :int] [:int]]
    [:=> [:cat :string :string] [schemas-2/string]]]}
  ([] 500)
  ([a] (inc a))
  ([a b] (str a b)))

(defn multi-arity-variadic-fn
  {:malli/schema
   [:function
    [:=> [:cat] [:int]]
    [:=> [:cat :int] [schemas-2/int-arg]]
    [:=> [:cat :string :string] [:string]]
    [:=> [:cat :string :string [:* :string]] [:string]]]}
  ([] 500)
  ([a] (inc a))
  ([a b] (str a b))
  ([a b c & more] (str a b c more)))

(defn variadic-fn1
  {:malli/schema [:=> [:cat [:* :int]] [:int]]}
  [& vs] (apply + vs))

(defn variadic-fn2
  {:malli/schema [:=> [:cat :int [:* :int]] [:int]]}
  [a & vs] (apply + a vs))

(defn minus-small-int
  "kukka"
  {:malli/schema [:=> [:cat :int] small-int]
   :malli/scope #{:input :output}}
  [x] (dec x))

(defn plus-small-int [x] (inc x))
(m/=> plus-small-int [:=> [:cat :int] small-int])

(def Over100 (m/-simple-schema {:type 'Over100, :pred #(and (int? %) (< 100 %))}))

(defn plus-over-100 [x] (inc x))
(m/=> plus-over-100 [:=> [:cat :int] Over100])

(mx/defn power :- [:int {:max 6}]
  "inlined schema power"
  [x :- :int] (* x x))

(mx/defn str-join-mx :- int?
  [args :- VecOfInts]
  (apply str args))

(deftest instrument!-test
  (testing "with instrumentation"
    (mi/instrument! {:filters [(mi/-filter-ns 'malli.instrument.cljs-test 'malli.instrument.fn-schemas)]})

    (is (thrown-with-msg? js/Error #":malli.core/invalid-input" (plus "2")))
    (is (thrown-with-msg? js/Error #":malli.core/invalid-output" (plus 6)))

    (is (thrown-with-msg? js/Error #":malli.core/invalid-input" (plus-small-int "2")))
    (is (thrown-with-msg? js/Error #":malli.core/invalid-output" (plus-small-int 8)))

    (is (thrown-with-msg? js/Error #":malli.core/invalid-input" (plus-over-100 "2")))
    (is (thrown-with-msg? js/Error #":malli.core/invalid-output" (plus-over-100 8)))

    (is (thrown-with-msg? js/Error #":malli.core/invalid-input" (power "2")))
    (is (thrown-with-msg? js/Error #":malli.core/invalid-output" (power 6)))

    (is (thrown-with-msg? js/Error #":malli.core/invalid-input" (str-join-mx ["2"])))
    (is (thrown-with-msg? js/Error #":malli.core/invalid-output" (str-join-mx [6])))

    (is (thrown-with-msg? js/Error #":malli.core/invalid-input" (schemas/str-join-mx2 ["2"])))
    (is (thrown-with-msg? js/Error #":malli.core/invalid-output" (schemas/str-join-mx2 [6])))

    (is (thrown-with-msg? js/Error #":malli.core/invalid-input" (schemas/power-ret-refer "2")))
    (is (thrown-with-msg? js/Error #":malli.core/invalid-output" (schemas/power-ret-refer 6)))

    (is (thrown-with-msg? js/Error #":malli.core/invalid-input" (schemas/power-ret-ns "2")))
    (is (thrown-with-msg? js/Error #":malli.core/invalid-output" (schemas/power-ret-ns 6)))

    (is (thrown-with-msg? js/Error #":malli.core/invalid-input" (schemas/power-arg-refer "2")))
    (is (thrown-with-msg? js/Error #":malli.core/invalid-output" (schemas/power-arg-refer 6)))

    (is (thrown-with-msg? js/Error #":malli.core/invalid-input" (schemas/power-arg-ns "2")))
    (is (thrown-with-msg? js/Error #":malli.core/invalid-output" (schemas/power-arg-ns 6)))

    (is (thrown-with-msg? js/Error #":malli.core/invalid-input" (schemas/power-full "2")))
    (is (thrown-with-msg? js/Error #":malli.core/invalid-output" (schemas/power-full 6)))

    (is (thrown-with-msg? js/Error #":malli.core/invalid-input" (schemas/power-int? "2")))
    (is (thrown-with-msg? js/Error #":malli.core/invalid-output" (schemas/power-int? 6))))

  (testing "without instrumentation"
    (mi/unstrument! {:filters [(mi/-filter-ns 'malli.instrument.cljs-test 'malli.instrument.fn-schemas)]})

    (is (= "21" (plus "2")))
    (is (= 7 (plus 6)))

    (is (= (plus-small-int 8) 9))
    (is (= (plus-over-100 8) 9))

    (is (= 4 (power "2")))
    (is (= 36 (power 6)))

    (is (= "2" (str-join-mx ["2"])))
    (is (= "6" (str-join-mx [6])))

    (is (= "2" (schemas/str-join-mx2 ["2"])))
    (is (= "6" (schemas/str-join-mx2 [6])))

    (is (= 4 (schemas/power-ret-refer "2")))
    (is (= 36 (schemas/power-ret-refer 6)))

    (is (= 4 (schemas/power-ret-ns "2")))
    (is (= 36 (schemas/power-ret-ns 6)))

    (is (= 4 (schemas/power-arg-refer "2")))
    (is (= 36 (schemas/power-arg-refer 6)))

    (is (= 4 (schemas/power-arg-ns "2")))
    (is (= 36 (schemas/power-arg-ns 6)))

    (is (= 4 (schemas/power-full "2")))
    (is (= 36 (schemas/power-full 6)))))

(mi/collect! {:ns ['malli.instrument.cljs-test 'malli.instrument.fn-schemas]})

(deftest collect!-test
  (testing "with instrumentation"
    (mi/instrument! {:filters [(mi/-filter-ns 'malli.instrument.cljs-test 'malli.instrument.fn-schemas)]})
    (is (thrown-with-msg? js/Error #":malli.core/invalid-input" (str-join [1 "2"])))
    (is (thrown-with-msg? js/Error #":malli.core/invalid-input" (str-join2 [1 "2"])))
    (is (thrown-with-msg? js/Error #":malli.core/invalid-input" (str-join3 [1 "2"])))
    (is (thrown-with-msg? js/Error #":malli.core/invalid-input" (str-join4 [1 "2"])))
    (is (thrown-with-msg? js/Error #":malli.core/invalid-input" (sum-nums "2")))
    (is (thrown-with-msg? js/Error #":malli.core/invalid-input" (sum-nums2 "2")))
    (is (thrown-with-msg? js/Error #":malli.core/invalid-input" (minus "2")))
    (is (thrown-with-msg? js/Error #":malli.core/invalid-output" (minus 6)))

    (is (thrown-with-msg? js/Error #":malli.core/invalid-input" (minus-small-int "2")))
    (is (thrown-with-msg? js/Error #":malli.core/invalid-output" (minus-small-int 10)))

    (is (thrown-with-msg? js/Error #":malli.core/invalid-input" (variadic-fn1 1 "2")))
    (is (thrown-with-msg? js/Error #":malli.core/invalid-input" (variadic-fn2 1 "2")))
    (is (= 3 (variadic-fn1 1 2)))
    (is (= 3 (variadic-fn2 1 2)))
    (is (= 500 (multi-arity-fn)))
    (is (= 2 (multi-arity-fn 1)))
    (is (= "ab" (multi-arity-fn "a" "b")))
    (is (thrown-with-msg? js/Error #":malli.core/invalid-input" (multi-arity-fn "a")))
    (is (thrown-with-msg? js/Error #":malli.core/invalid-input" (multi-arity-fn 1 2)))

    (is (= 500 (multi-arity-variadic-fn)))
    (is (= 2 (multi-arity-variadic-fn 1)))
    (is (= "ab" (multi-arity-variadic-fn "a" "b")))
    (is (= "abc(\"d\")" (multi-arity-variadic-fn "a" "b" "c" "d")))
    (is (thrown-with-msg? js/Error #":malli.core/invalid-input" (multi-arity-variadic-fn "a")))
    (is (thrown-with-msg? js/Error #":malli.core/invalid-input" (multi-arity-variadic-fn 1 2)))
    (is (thrown-with-msg? js/Error #":malli.core/invalid-input" (multi-arity-variadic-fn 1 2 :c))))

  (testing "without instrumentation"
    (mi/unstrument! {:filters [(mi/-filter-ns 'malli.instrument.cljs-test 'malli.instrument.fn-schemas)]})

    (is (= 1 (minus "2")))
    (is (= 5 (minus 6)))
    (is (= (sum-nums [2 "3" 4 5]) "2345"))
    (is (= (sum-nums2 [2 "3" 4 5]) "2345"))
    (is (= (str-join [1 "2"]) "12"))
    (is (= (str-join2 [1 "2"]) "12"))
    (is (= (str-join3 [1 "2"]) "12"))
    (is (= (str-join4 [1 "2"]) "12"))

    (is (= 1 (minus-small-int "2")))
    (is (= 9 (minus-small-int 10)))))

(deftest check-test
  (let [results (mi/check)]
    (is (map? results))))

(deftest instrument-external-test

  (testing "Without instrumentation"
    (is (thrown?
         js/Error
         #_:clj-kondo/ignore
         (select-keys {:a 1} :a))))

  (testing "With instrumentation"
    (m/=> clojure.core/select-keys [:=> [:cat map? sequential?] map?])
    (with-out-str (mi/instrument! {:filters [(mi/-filter-ns 'clojure.core)]}))
    (is (thrown-with-msg?
         js/Error
         #":malli.core/invalid-input"
         #_:clj-kondo/ignore
         (select-keys {:a 1} :a)))
    (is (= {:a 1} (select-keys {:a 1} [:a])))
    (with-out-str (mi/unstrument! {:filters [(mi/-filter-ns 'clojure.core)]}))))
