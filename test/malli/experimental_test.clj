(ns malli.experimental-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest testing is]]
            [malli.core :as m]
            [malli.experimental :as mx]
            [malli.instrument :as mi]
            [malli.registry :as mr]))

;; normal, no-args
(mx/defn f1 [] 1)

;; normal, args
(mx/defn f2 [x] x)

;; schematized, arg
(mx/defn f3 [x :- :int] x)

;; schematized, many args
(mx/defn f4 :- [:int {:min 0}]
  "int int -> int functions"
  [x :- [:int {:min 0}], y :- :int]
  (+ x y))

(def AB [:map [:a [:int {:min 0}]] [:b :int]])
(def CD [:map [:c [:int {:min 0}]] [:d :int]])

;; schematized, nested keywords args
(mx/defn f5 :- [:cat :int :int :int :int AB CD]
  "Nested Keyword argument"
  [[& {:keys [a b] :as m1} :- AB]
   & {:keys [c d] :as m2} :- CD]
  [a b c d m1 m2])

;; multi-arity
(mx/defn f6 :- [:int {:min 0}]
  "docstring"
  {:some "meta"}
  ([x :- [:int {:min 0}]] (inc x))
  ([x :- [:int {:min 0}], y :- :int] (+ x y))
  ([x :- [:int {:min 0}], y :- :int & zs :- [:* :int]] (apply + x y zs))
  {:more "meta"})

(def StringStartingWithA
  (m/schema
   (m/-simple-schema
    {:type ::string-starting-with-a
     :pred #(str/starts-with? % "a")})))

(def options
  {:registry (mr/composite-registry
              (m/-registry)
              {::string-starting-with-a StringStartingWithA})})

;; custom types
(mx/defn ^{:malli/options options} f7
  [s :- ::string-starting-with-a]
  (str s " - this starts with a `a`"))

(def expectations
  [{:var #'f1
    :calls [[nil 1]
            [[1] ::throws]]
    :instrumented [[nil 1]
                   [[1] ::throws]]}
   {:var #'f2
    :calls [[[1] 1]
            [["kikka"] "kikka"]
            [[] ::throws]
            [[1 2] ::throws]]
    :instrumented [[[1] 1]
                   [["kikka"] "kikka"]
                   [[] ::throws]
                   [[1 2] ::throws]]}
   {:var #'f3
    :meta {:arglists '([x])
           :raw-arglists '[[x :- :int]]
           :schema [:=> [:cat :int] :any]}
    :calls [[[1] 1]
            [["kikka"] "kikka"]
            [[1 2] ::throws]]
    :instrumented [[[1] 1]
                   [["kikka"] ::throws]
                   [[1 2] ::throws]]}
   {:var #'f4
    :meta {:doc "int int -> int functions"
           :arglists '([x y])
           :raw-arglists '([x :- [:int {:min 0}] y :- :int])
           :schema [:=> [:cat [:int {:min 0}] :int] [:int {:min 0}]]}
    :calls [[[1 2] 3]
            [[-2 1] -1]
            [[-1 -1] -2]
            [[1 "2"] ::throws]]
    :instrumented [[[1 2] 3]
                   [[-2 1] ::throws]  ;; input
                   [[-1 -1] ::throws] ;; ret
                   [[1 "2"] ::throws]]}
   {:var #'f5
    :meta {:arglists '([[& {:keys [a b], :as m1}] & {:keys [c d], :as m2}])
           :raw-arglists '([[& {:keys [a b] :as m1} :- AB]
                            & {:keys [c d] :as m2} :- CD])
           :schema [:=>
                    [:cat [:maybe [:cat AB]] CD]
                    [:cat :int :int :int :int AB CD]]}
    :calls [[[[{:a 1, :b 2}] {:c 3, :d 4}]
             [1 2 3 4 {:a 1, :b 2} {:c 3, :d 4}]]
            [[[{:a -1, :b 2}] {:c 3, :d 4}]
             [-1 2 3 4 {:a -1, :b 2} {:c 3, :d 4}]]]
    :instrumented [[[[{:a 1, :b 2}] {:c 3, :d 4}]
                    [1 2 3 4 {:a 1, :b 2} {:c 3, :d 4}]]
                   [[[{:a -1, :b 2}] {:c 3, :d 4}]
                    ::throws]]}
   {:var #'f6
    :meta {:arglists '([x] [x y] [x y & zs])
           :raw-arglists '([x :- [:int {:min 0}]]
                           [x :- [:int {:min 0}] y :- :int]
                           [x :- [:int {:min 0}] y :- :int & zs :- [:* :int]])
           :schema [:function
                    [:=> [:cat [:int {:min 0}]] [:int {:min 0}]]
                    [:=> [:cat [:int {:min 0}] :int] [:int {:min 0}]]
                    [:=> [:cat [:int {:min 0}] :int [:* :int]] [:int {:min 0}]]]}
    :calls [[[1] 2]
            [[-1] 0]
            [[1 2] 3]
            [[1 -2] -1]
            [[-1 2] 1]
            [[1 2 3 4] 10]
            [[-1 2 3 4] 8]]
    :instrumented [[[1] 2]
                   [[-1] ::throws]
                   [[1 2] 3]
                   [[1 -2] ::throws]
                   [[-1 2] ::throws]
                   [[1 2 3 4] 10]
                   [[-1 2 3 4] ::throws]]}
   {:var #'f7
    :calls [[["ab"] "ab - this starts with a `a`"]
            [["ba"] "ba - this starts with a `a`"]]
    :instrumented [[["ab"] "ab - this starts with a `a`"]
                   [["ba"] ::throws]]}])

(defn -strument! [mode v]
  (with-out-str
    (mi/instrument!
     {:mode mode
      :filters [(mi/-filter-var #(= % v))]})))

(deftest defn-test
  (doseq [{:keys [var calls instrumented] :as e} expectations]

    (testing "plain calls"
      (doseq [[arg ret] calls]
        (if (= ::throws ret)
          (is (thrown? Exception (apply var arg)))
          (is (= ret (apply var arg))))))

    (when-let [m (:meta e)]
      (testing "meta"
        (doseq [[k v] m]
          (is (= v (k (meta var)))))))

    (when instrumented
      (testing "instrumented calls"
        (-strument! :instument var)
        (try
          (doseq [[arg ret] instrumented]
            (if (= ::throws ret)
              (is (thrown? Exception (apply var arg)))
              (is (= ret (apply var arg)))))
          (finally
            (-strument! :unstrument var)))))))
