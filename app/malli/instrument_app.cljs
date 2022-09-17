(ns malli.instrument-app
  (:require
    [malli.clj-kondo :as mari]
    ;[malli.helpers2 :as h2]
    [malli.core :as m]
    [malli.dev.cljs :as dev]
    [malli.dev.pretty :as pretty]
    [malli.generator :as mg]
    [malli.dev.cljs :as md]
    [malli.experimental :as mx]
    [malli.instrument.cljs :as mi]))

(defn init [] (js/console.log "INIT!"))

(defn x+y
  {:malli/schema [:=> [:cat float? float?] :double]}
  [x y]
  (+ x y))

(defn sum [a b] (+ a b))

(def sum2
  (m/-instrument {:schema (m/schema [:=> [:cat :int :int] :int])
                  :report (pretty/reporter)}
    sum))

(m/=> sum [:=> [:cat :int :int] :int])

(set! sum
  (m/-instrument {:schema (m/schema [:=> [:cat :int :int] :int])
                  :report (pretty/reporter)}
    sum))

(defn minus
  "a normal clojure function, no dependencies to malli"
  {:malli/schema [:=> [:cat :int] [:int {:min 6}]]
   :malli/gen    true
   :malli/scope  #{:input :output}}
  [x]
  (dec x))

(defn plus-gen
  {:malli/schema [:=> [:cat :int] [:int {:min 6}]]}
  [x]
  (dec x))

(comment
  @mi/instrumented-vars
  ((get @mi/instrumented-vars `sum) 1 "2"))

(defn plus1 [a] (inc a))
(m/=> plus1 [:=> [:cat :int] :int])

(defn plus2
  {:validate? true}
  [a b]
  (+ a b))
(m/=> plus2 [:=> [:cat :string :int] :int])

;; multi-arity function
(defn plus-many
  ([a] (inc a))
  ([a b & others]
   (apply + a b others)))

(m/=> plus-many
  [:function
   [:=> [:cat :int] :int]
   [:=> [:cat :int :int [:* :int]] :int]])

(def pow-gen
  (m/-instrument
    {:schema [:function
              [:=> [:cat :int] [:int {:max 6}]]
              [:=> [:cat :int :int] [:int {:max 6}]]]
     :gen mg/generate}))

(defn minus2
  "kukka"
  {:malli/schema [:=> [:cat :int] [:int {:min 6}]]
   :malli/scope  #{:input :output}}
  [x] (dec x))

(defn ->minus [] minus2)
(defn minus-test [x] (dec x))

(defn plus-it [x] (inc x))
(m/=> plus-it [:=> [:cat :int] [:int {:max 6}]])

(defn sum3 [a b] (+ a b))
(m/=> sum3 [:=> [:cat :int :int] :int])

(def small-int [:int {:max 6}])

(def MyInt (m/-simple-schema {:type 'MyInt, :pred #(and (int? %) (< 100 %))}))

(defn plus [x] (inc x))
(m/=> plus [:=> [:cat :int] small-int])

(defn plusX [x] (inc x))
(m/=> plusX [:=> [:cat :int] MyInt])

(defn my-function-bad
  {:malli/schema [:=> [:cat :int [:* :int]] :any]}
  [x & args]
  (prn "X is " x " args are " args)
  123)

(defn pure-vary
  {:malli/schema [:=> [:cat [:* :string]] some?]}
  [& x] x)

(defn multi-arity-variadic-fn
  {:malli/schema
     [:function
      ;[:=> [:cat] [:int]]
      [:=> [:cat :int] [:int]]
      ;[:=> [:cat :string :string] [:string]]
      [:=> [:cat :string :string :string [:* :string]] [:string]]]}
  ([] 500)
  ([a] (inc a))
  ([a b] (str a b))
  ([a b c & more] (str a b c more)))

(defn try-it []
  (println "minus2")
  ;(minus2 1)
  (plus-many 5 8 1 0 20)
  ;(plus-many "hi")
  (my-function-bad 1)
  (my-function-bad 1 5)
  ;(my-function-bad 1 nil)
  ;(pure-vary "hi" 5)
  (multi-arity-variadic-fn "a" "b")
  ;; works b/c there is no schema for this arity:
  (multi-arity-variadic-fn 'a "b" )
  ;; fails as it hits the last fn schema
  (multi-arity-variadic-fn 'a "b" "b")
  ;(multi-arity-variadic-fn "a" "b" "c" :x)
  )

(defn ^:dev/after-load x []
  (println "AFTER LOAD - malli.dev.cljs/start!")
  (md/start!)
  (js/setTimeout try-it 100))
