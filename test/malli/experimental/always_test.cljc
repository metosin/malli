(ns malli.experimental.always-test
  (:require [clojure.test :refer [deftest testing is]]
            [malli.experimental :as mx])
  #?(:clj(:require [malli.dev])))

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

(defn always-assertions []
  (doseq [[f description] [[addition ":malli/always meta on var"]
                           [addition-2 ":malli/always meta inside defn"]
                           [addition-multiarity "multiple arities"]]]
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
        "invalid input throws")))

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
