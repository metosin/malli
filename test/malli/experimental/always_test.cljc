(ns malli.experimental.always-test
  (:refer-clojure :exclude [destructure])
  (:require [clojure.test :refer [deftest testing is]]
            [malli.experimental :as mx])
  #?(:clj (:require [malli.dev])))

(mx/defn ^:malli/always addition :- [:int {:min 0}]
  [x :- [:int {:min 0}], y :- :int]
  (+ x y))

(mx/defn addition-2 :- [:int {:min 0}]
  {:malli/always true}
  [x :- [:int {:min 0}], y :- :int]
  (+ x y))

(mx/defn addition-multiarity :- [:int {:min 0}]
  {:malli/always true}
  ([x :- [:int {:min 0}], y :- :int]
   (+ x y))
  ([x :- [:int {:min 2}]]
   x))

(mx/defn ^:malli/always addition-varargs :- [:int {:min 0}]
  [& xs :- [:cat [:int {:min 0}] [:* :int]]]
  (apply + xs))

(mx/defn ^:malli/always destructure :- [:map [:b :int]]
  {:more "metadata"}
  [[val, {:keys [a, b] :as m1}] :- [:tuple :int [:map [:a :string]]],
   {c :foo :as m2} :- [:map [:foo :int]]]
  {:val val
   :a a
   :b b
   :m1 m1
   :c c
   :m2 m2})

(defn always-assertions []
  (doseq [[f description] [[addition ":malli/always meta on var"]
                           [addition-2 ":malli/always meta inside defn"]
                           [addition-multiarity "multiple arities"]
                           [addition-varargs "varargs"]]]
    (testing description
      (is (= 3 (f 1 2))
          "valid input works")
      (is (= :malli.core/invalid-input
             (try (f -2 1)
                  (catch #?(:clj Exception :cljs js/Error) e
                    (:type (ex-data e)))))
          "invalid input throws")
      (is (= :malli.core/invalid-output
             (try (f 2 -3)
                  (catch #?(:clj Exception :cljs js/Error) e
                    (:type (ex-data e)))))
          "invalid output throws")))
  (testing "other arity of multiple arity function"
    (is (= 3 (addition-multiarity 3))
        "valid input works")
    (is (= :malli.core/invalid-input
           (try (addition-multiarity 1)
                (catch #?(:clj Exception :cljs js/Error) e
                  (:type (ex-data e)))))
        "invalid input throws"))
  (testing "destructuring"
    (is (= {:val 1 :a "foo" :b 3
            :m1 {:a "foo" :b 3}
            :c 4
            :m2 {:foo 4 :bar 5}}
           (destructure [1 {:a "foo" :b 3}]
                        {:foo 4 :bar 5}))
        "valid input works")
    (is (= :malli.core/invalid-input
           (try (destructure [1 {:a 2 :b 3}]
                             {:foo 4 :bar 5})
                (catch #?(:clj Exception :cljs js/Error) e
                  (:type (ex-data e)))))
        "invalid input throws")
    (is (= :malli.core/invalid-input
           (try (destructure [1 {:a "foo" :b 3}]
                             {:bar 5})
                (catch #?(:clj Exception :cljs js/Error) e
                  (:type (ex-data e)))))
        "invalid input throws")
    (is (= :malli.core/invalid-output
           (try (destructure [1 {:a "foo" :b true}]
                             {:foo 4 :bar 5})
                (catch #?(:clj Exception :cljs js/Error) e
                  (:type (ex-data e)))))
        "invalid output throws")))

(deftest always-test
  (testing "without malli.dev"
    (always-assertions))
  #?(:clj
     (do
       (testing "with malli.dev/start!"
         (malli.dev/start!)
         (try
           (always-assertions)
           (finally
             (malli.dev/stop!))))
       (testing "after malli.dev/stop!"
         (always-assertions)))))

(mx/defn destructure2 :- [:map [:b :int]]
  {:more "metadata"}
  [[val, {:keys [a, b] :as m1}] :- [:tuple :int [:map [:a :string]]],
   {c :foo :as m2} :- [:map [:foo :int]]]
  {:val val
   :a a
   :b b
   :m1 m1
   :c c
   :m2 m2})

#?(:clj
   (deftest always-metadata-test
     (let [clean #(dissoc % :name :line :malli/always)]
       (is (= (clean (meta #'destructure2))
              (clean (meta #'destructure)))
           ":malli/always doesn't affect generated metadata"))))
