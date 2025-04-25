(ns malli.instrument-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [malli.core :as m]
            [malli.instrument :as mi]))

(defn plus [x] (inc x))
(m/=> plus [:=> [:cat :int] [:int {:max 6}]])

(defn ->plus [] plus)

(defn primitive-DO [^double val] val)
(m/=> primitive-DO [:=> [:cat :double] :double])

(defn opts [] {:filters [(fn [& args]
                           (and (apply (mi/-filter-ns 'malli.instrument-test) args)
                                (apply (mi/-filter-var #(not= #'primitive-DO %)) args)))]})
(defn unstrument! [] (with-out-str (mi/unstrument! (opts))))
(defn instrument! [] (with-out-str (mi/instrument! (opts))))

(deftest instrument!-test

  (testing "without instrumentation"
    (unstrument!)
    (is (thrown?
         ClassCastException
         ((->plus) "2")))
    (is (= 7 ((->plus) 6))))

  (testing "with instrumentation"
    (instrument!)
    (is (thrown-with-msg?
         Exception
         #":malli.core/invalid-input"
         ((->plus) "2")))
    (is (thrown-with-msg?
         Exception
         #":malli.core/invalid-output"
         ((->plus) 6)))))

(defmacro if-bb [then & [else]]
  (if (System/getProperty "babashka.version")
    then
    else))

(if-bb
  (deftest primitive-functions-can-be-instrumented
    (is (= 42 (primitive-DO 42)))
    (is (= "42" (primitive-DO "42")))
    (is (mi/instrument! {:filters [(mi/-filter-var #(= #'primitive-DO %))]}))
    (is (thrown-with-msg?
          Exception
          #":malli.core/invalid-input"
          (primitive-DO "42")))
    (is (mi/unstrument! {:filters [(mi/-filter-var #(= #'primitive-DO %))]}))
    (is (= 42 (primitive-DO 42)))
    (is (= "42" (primitive-DO "42"))))
  (deftest primitive-functions-cannot-be-instrumented
    (is (= 42.0 (primitive-DO 42)))
    (is (thrown? ClassCastException (primitive-DO "42")))
    (is (str/includes?
          (with-out-str (mi/instrument! {:filters [(mi/-filter-var #(= #'primitive-DO %))]}))
          "WARNING: Not instrumenting primitive fn #'malli.instrument-test/primitive-DO"))
    (is (= 42.0 (primitive-DO 42)))
    (is (thrown? ClassCastException (primitive-DO "42")))))

(defn minus
  "kukka"
  {:malli/schema [:=> [:cat :int] [:int {:min 6}]]
   :malli/scope #{:input :output}}
  [x] (dec x))

(defn ->minus [] minus)

(mi/collect!)

(deftest collect!-test

  (testing "without instrumentation"
    (unstrument!)
    (is (thrown?
         ClassCastException
         ((->minus) "2")))
    (is (= 5 ((->minus) 6))))

  (testing "with instrumentation"
    (instrument!)
    (is (thrown-with-msg?
         Exception
         #":malli.core/invalid-input"
         ((->minus) "2")))
    (is (thrown-with-msg?
         Exception
         #":malli.core/invalid-output"
         ((->minus) 6)))))

(defn f1
  "accumulated schema from arities"
  (^{:malli/schema [:=> [:cat :int] :int]} [x] (inc x))
  (^{:malli/schema [:=> [:cat :int :int] :int]} [x y] (+ x y)))

(defn f2
  "top-level schema wins"
  (^{:malli/schema [:=> [:cat :any] :any]} [x] (inc x))
  (^{:malli/schema [:=> [:cat :any :any] :any]} [x y] (+ x y))
  {:malli/schema [:function
                  [:=> [:cat :int] :int]
                  [:=> [:cat :int :int] :int]]})

(defn f3
  "invalid schema as not arities have it"
  ([x] (inc x))
  (^{:malli/schema [:=> [:cat :int :int] :int]} [x y] (+ x y)))

(deftest -schema-test
  (is (= [:function
          [:=> [:cat :int] :int]
          [:=> [:cat :int :int] :int]]
         (mi/-schema #'f1)))
  (is (= [:function
          [:=> [:cat :int] :int]
          [:=> [:cat :int :int] :int]]
         (mi/-schema #'f2)))
  (is (= nil (mi/-schema #'f3))))

(deftest check-test
  (testing "all registered function schemas in this namespace"
    (let [results (mi/check {:filters [(mi/-filter-ns 'malli.instrument-test)]})]
      (is (map? results)))))

(deftest instrument-external-test

  (testing "Without instrumentation"
    (is (thrown?
         java.lang.IllegalArgumentException
         #_:clj-kondo/ignore
         (select-keys {:a 1} :a))))

  (testing "With instrumentation"
    (m/=> clojure.core/select-keys [:=> [:cat map? sequential?] map?])
    (with-out-str (mi/instrument! {:filters [(mi/-filter-ns 'clojure.core)]}))
    (is (thrown-with-msg?
         Exception
         #":malli.core/invalid-input"
         #_:clj-kondo/ignore
         (select-keys {:a 1} :a)))
    (is (= {:a 1} (select-keys {:a 1} [:a])))
    (with-out-str (mi/unstrument! {:filters [(mi/-filter-ns 'clojure.core)]}))))

(defn reinstrumented [] 1)

(deftest reinstrument-test
  (m/=> reinstrumented [:-> [:= 2]])
  (instrument!)
  (is (thrown-with-msg?
        Exception
        #":malli\.core/invalid-output"
        (reinstrumented)))
  (m/=> reinstrumented [:-> [:= 1]])
  (instrument!)
  (is (= 1 (reinstrumented))))
