(ns malli.generator-test
  (:require [clojure.test :refer [deftest testing is]]
            [clojure.test.check.generators :as gen]
            [malli.json-schema-test :as json-schema-test]
            [malli.generator :as mg]
            [malli.core :as m]))

(deftest generator-test
  (doseq [[?schema] json-schema-test/expectations
          ;; cljs doesn't have a regex generator :(
          #?@(:cljs [:when (not= (m/type ?schema) :re)])]
    (testing (m/form ?schema)
      (testing "generate"
        (is (= (mg/generate ?schema {:seed 123})
               (mg/generate ?schema {:seed 123})))
        (is (= (mg/generate ?schema {:seed 123, :size 10})
               (mg/generate ?schema {:seed 123, :size 10})))
        (is (m/validate ?schema (mg/generate ?schema {:seed 123}))))
      (testing "sample"
        (is (= (mg/sample ?schema {:seed 123})
               (mg/sample ?schema {:seed 123})))
        (is (= (mg/sample ?schema {:seed 123, :size 10})
               (mg/sample ?schema {:seed 123, :size 10})))
        (doseq [value (mg/sample ?schema {:seed 123})]
          (is (m/validate ?schema value))))))

  (testing "string"
    (let [schema [:string {:min 1, :max 4}]]
      (is (every? (partial m/validate schema) (mg/sample schema {:size 1000})))))

  #?(:clj (testing "regex"
            (let [re #"^\d+ \d+$"]
              (m/validate re (mg/generate re)))

            (let [re-test #"(?=.{8,})" ;; contains unsupported feature
                  elements ["abcdefgh" "01234567"]
                  fmap '(fn [s] (str "prefix_" s))]
              (is (thrown-with-msg? Exception #"Unsupported-feature" (mg/generator [:re re-test])))
              (m/validate #".{8,}" (mg/generate [:re {:gen/elements elements} re-test]))
              (m/validate #"prefix_.{8,}" (mg/generate [:re {:gen/fmap fmap, :gen/elements elements} re-test])))))

  (testing "no generator"
    (is (thrown-with-msg?
          #?(:clj Exception, :cljs js/Error)
          #":malli.generator/no-generator"
          (mg/generate [:fn '(fn [x] (<= 0 x 10))]))))

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
    (dotimes [_ 1000]
      (is (#{1 2} (mg/generate [:and {:gen/elements [1 2]} int?]))))
    (dotimes [_ 1000]
      (is (#{"1" "2"} (mg/generate [:and {:gen/elements [1 2], :gen/fmap 'str} int?])))))

  (testing "gen/gen"
    (dotimes [_ 1000]
      (is (#{1 2} (mg/generate [:and {:gen/gen (gen/elements [1 2])} int?]))))
    (dotimes [_ 1000]
      (is (#{"1" "2"} (mg/generate [:and {:gen/gen (gen/elements [1 2]) :gen/fmap str} int?]))))))

(deftest protocol-test
  (let [values #{1 2 3 5 8 13}
        schema (reify
                 m/Schema
                 (-validator [_] int?)
                 (-properties [_])
                 mg/Generator
                 (-generator [_ _] (gen/elements values)))]
    (dotimes [_ 1000]
      (is (values (mg/generate schema))))))
