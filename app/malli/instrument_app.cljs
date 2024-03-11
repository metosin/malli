(ns malli.instrument-app
  (:require
   [malli.instrument :as mi-new]
   malli.helpers
   [malli.core :as m]
   [clojure.test.check.generators :as gen]
   [malli.experimental.time.generator]
   [malli.dev.pretty :as pretty]
   [malli.generator :as mg]
   [malli.experimental :as mx]
   [malli.experimental.time :as time]))

(js/console.log "now: " (time/LocalDate.now))
(js/console.log "generated: "
                (gen/sample (mg/-schema-generator (time/-local-date-time-schema) nil) 10))

(js/console.log
 (into-array
  (mg/sample :time/zoned-date-time {:registry (merge (m/default-schemas) (time/schemas))})))

(mx/defn my-ex-fn :- [:int]
  [a :- :string] (+ 5 a))

(mx/defn my-ex-fn2 :- [:double]
  ([a :- :string] (+ 5 a))
  ([a :- :string, b :- :double] (+ 5 a b)))

(defn init [] (js/console.log "INIT!"))
(defn x+y
  {:malli/schema [:=> [:cat float? float?] :double]}
  [x y]
  (+ x y))

;(defn add-dates [a b]
;  {:malli/schema [:=> [:cat :time/local-date :time/local-date] :time/local-date]}

(defn sum [a b] (+ a b))

(def sum2
  (m/-instrument {:schema (m/schema [:=> [:cat :int :int] :int])
                  :report (pretty/reporter)}
                 sum))

;(m/=> sum [:=> [:cat :int :int] :int])

(set! sum
      (m/-instrument {:schema (m/schema [:=> [:cat :int :int] :int])
                      :report (pretty/reporter)}
                     sum))

(defn minus
  "a normal clojure function, no dependencies to malli"
  ;{:malli/schema [:=> [:cat :int] [:int {:min 6}]]
  ; :malli/gen    true
  ; :malli/scope  #{:input :output}}
  [x]
  (dec x))

(defn plus-gen
  ;{:malli/schema [:=> [:cat :int] [:int {:min 6}]]}
  [x]
  (dec x))

;(comment
;  @mi/instrumented-vars
;  ((get @mi/instrumented-vars `sum) 1 "2"))

(defn plus1 [a] (inc a))
;(m/=> plus1 [:=> [:cat :int] :int])

(defn plus2
  {:validate? true}
  [a b]
  (+ a b))
;(m/=> plus2 [:=> [:cat :string :int] :int])

;; multi-arity function

(defn plus-many
  {:malli/schema
   [:function
    [:=> [:cat :int] :int]
    [:=> [:cat :int :int [:* :int]] :int]]}
  ([a] (inc a))
  ([a b & others]
   (apply + a b others)))

(defn a-fun {:malli/schema [:=> [:cat :int] :int]} [a] (+ 5 a))
(defn a-fun2 {:malli/schema [:=> [:cat :string] :int]} [a] (+ 5 a))

;(m/=> plus-many
;  [:function
;   [:=> [:cat :int] :int]
;   [:=> [:cat :int :int [:* :int]] :int]])

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
;(m/=> plus-it [:=> [:cat :int] [:int {:max 6}]])

(defn sum3 [a b] (+ a b))
(comment (meta sum3))

(m/=> sum3 [:=> [:cat :int :int] :int])

(def small-int [:int {:max 6}])

(def MyInt (m/-simple-schema {:type 'MyInt, :pred #(and (int? %) (< 100 %))}))
;(defn plus [x] (inc x))
;(m/=> plus [:=> [:cat :int] small-int])

(defn plusX [x] (inc x))
;(m/=> plusX [:=> [:cat :int] MyInt])

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
    [:=> [:cat :string :string] [:string]]
    [:=> [:cat :string :string :string [:* :string]] [:string]]]}
  ([] 500)
  ([a] (inc a))
  ([a b] (str a b))
  ([a b c & more] (str a b c more)))

(defn plus [x] (inc x))
(m/=> plus [:=> [:cat :int] [:int {:max 6}]])

(defn try-it []
  (println "in try-it")
  ;(minus2 1)
  (plus-many 5 8 1 0 20)
  ;(pure-vary "hi" 50)
  ;(malli.helpers/x+y "hi" 5)
  ;(plus-many 5 8 1 "0" 20)
  ;(mi-new/unstrument!)
  ;(plus "2")
  ;(str-join-mx2 [2])
  (println "after ")

  ;(plus-many 5 8 1 "0" 20)
  ;(plus-many "hi")
  (my-function-bad 1)

  (my-function-bad 1 5)
  (my-function-bad 1 nil)
  ;(pure-vary "hi" 5)

  (multi-arity-variadic-fn "a" "b")
  ;; works b/c there is no schema for this arity:
  ;(multi-arity-variadic-fn 'a "b" )

  ;; fails as it hits the last fn schema
  (multi-arity-variadic-fn 'a "b" "b")
  ;(multi-arity-variadic-fn "a" "b" "c" :x)
  )
(macroexpand '(mi-new/collect! {:ns ['malli.instrument-app 'malli.instrument-test 'malli.instrument.fn-schemas]}))
(defn ^:dev/after-load x []
  (println "AFTER LOAD - malli.dev.cljs/start!")
  ;(m/-deregister-metadata-function-schemas! :cljs)
  ;(dev/collect-all!)
  ;(mi-new/collect!)
  ;(mi-new/collect! {:ns [
  ;                       'malli.instrument-app
  ;                       'malli.instrument-test 'malli.instrument.fn-schemas]})
  ;
  ;(mi-new/unstrument!)
  ;(mi-new/instrument! {:filters [
  ;                               (mi-new/-filter-ns 'malli.instrument-test 'malli.instrument.fn-schemas 'malli.instrument-app)
  ;                               ;(mi-new/-filter-schema (fn [s] (println "FILTER SCHEMA: " s)))
  ;                               ;(mi-new/-filter-var
  ;                               ;  #{#'str-join}
  ;                               ;  ;(fn [x] (= x #'str-join))
  ;                               ;  )
  ;                               ]
  ;                     ;:skip-instrumented? true
  ;
  ;                 :report (pretty/thrower)})
  ;(dev/start!)
  ;(mi.old/instrument!)
  ;(js/setTimeout try-it 100)
  )
(comment
  (macroexpand
   '(dev/start!))
  (m/function)
  (@(Var. (constantly str-join) 'str-join {:metadata 'here}) [1 2])
  (= (Var. (constantly str-join) `str-join {:metadata 'here})
     #'str-join)
  (macroexpand '(dev/collect-all!))

  (macroexpand '(mi-new/collect-all!))
  (mi-new/-pure-variadic? plus-many)
  (mi-new/-pure-variadic? pure-vary)
  (mi-new/-pure-variadic? my-function-bad))
