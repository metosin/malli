# Working with Functions

In Clojure, functions are first-class. Here's a simple function:

```clj
(defn plus [x y]
  (+ x y))
  
(plus 1 2)
; => 3
```

## Predicate Schemas

Simplest way to describe function values with malli is to use predefined predicate schemas `fn?` and `ifn?`:

```clj
(require '[malli.core :as m])

(m/validate fn? plus)
; => true

(m/validate ifn? plus)
; => true
```

Note that `ifn?` also accepts many data-structures that can be used as functions:

```clj
(m/validate ifn? :kikka)
; => true

(m/validate ifn? {})
; => true
```

But, neither of the predefined function predicate schemas can validate function arity, function arguments or return values. As it stands, [there is no robust way to programmatically check function arity at runtime](https://stackoverflow.com/questions/1696693/clojure-how-to-find-out-the-arity-of-function-at-runtime).

Enter, function schemas.

## Function Schemas

Function valus can be described with `:=>` and `:function` schemas. It allows to describe both function arguments as [sequence schemas](README.md#sequence-schemas) and function output as schemas.

Example function definitions:

```clj
;; no args, no return
[:=> :cat :nil]

;; int -> int
[:=> [:cat :int] :int]

;; x:int, xs:int* -> int
[:=> [:catn 
      [:x :int] 
      [:xs [:+ :int]]] :int]
      
;; multi-arity function
[:function
 [:=> [:cat :int] :int]
 [:=> [:cat :int :int [:* :int]] :int]]      
```

Function definition for our `plus` looks like this:

```clj
(def =>plus [:=> [:cat :int :int] :int])
```

Let's try:

```clj
(m/validate =>plus plus)
; => true
```

But, wait, as there is no way to know the function arity, arguments and returns at runtime, so how did that validation work? Actually, it didn't. By default. `:=>` validation just checks that it's a `fn?`, so this holds too:

```clj
(m/validate =>plus str)
; => true
```
Bummer.

Enter, generative testing.

## Generative testing

Like [clojure.spec](https://clojure.org/about/spec) demonstrated, we can use [test.check](https://github.com/clojure/test.check) to check the functions at runtime.

```clj
(require '[malli.generator :as mg])

(m/validate =>plus plus {::m/function-checker mg/function-checker})
; => true

(m/validate =>plus str {::m/function-checker mg/function-checker})
; => false
```

Explanation why it is not valid:

```clj
(m/explain =>plus str {::m/function-checker mg/function-checker})
;{:schema [:=> [:cat :int :int] :int],
; :value #object[clojure.core$str],
; :errors (#Error{:path [],
;                 :in [],
;                 :schema [:=> [:cat :int :int] :int],
;                 :value #object[clojure.core$str],
;                 :check {:total-nodes-visited 0,
;                         :depth 0,
;                         :pass? false,
;                         :result false,
;                         :result-data nil,
;                         :time-shrinking-ms 1,
;                         :smallest [(0 0)],
;                         :malli.generator/explain-output {:schema :int,
;                                                          :value "00",
;                                                          :errors (#Error{:path []
;                                                                          :in []
;                                                                          :schema :int
;                                                                          :value "00"})}}})}
```

Smallest failing invocation is `(str 0 0)`, which returns `"00"`, which is not an `:int`. Great.

But, why `mg/function-checker` is not enabled by default? It uses generartive testing, which is orders of magnitude slower and would introduce an extra dependency of `test.check` to `malli.core` making the core library much heavier. This would be expecially bad for CLJS bundle size.

## Generating functions

We can also generate implementations for functions based on the function schemas. The generated functions check the function arity and arguments at runtime and return generated values.

```clj
(def plus-gen (mg/generate =>plus))

(plus-gen 1)
; =throws=> :malli.core/invalid-arity {:arity 1, :arities #{{:min 2, :max 2}}, :args [1], :input [:cat :int :int], :schema [:=> [:cat :int :int] :int]}

(plus-gen 1 "2")
; =throws=> :malli.core/invalid-input {:input [:cat :int :int], :args [1 "2"], :schema [:=> [:cat :int :int] :int]}

(plus-gen 1 2)
; => -1
```

## Multi-arity functions

Multi-arity functions can be composed with `:function`:

```clj
(def MyFunction
  (m/schema
    [:function {:registry {"SmallInt" [:int {:min -100, :max 100}]}}
     [:=> [:cat "SmallInt"] :int]
     [:=> [:cat "SmallInt" "SmallInt" [:* "SmallInt"]] :int]]
    {::m/function-checker mg/function-checker}))

(m/validate
  MyFunction
  (fn
    ([x] x)
    ([x y & z] (apply - (- x y) z))))
; => true

(m/validate
  MyFunction
  (fn
    ([x] x)
    ([x y & z] (str x y z))))
; => false

(m/explain
  MyFunction
  (fn
    ([x] x)
    ([x y & z] (str x y z))))
;{:schema [:function
;          {:registry {"SmallInt" [:int {:min -100, :max 100}]}}
;          [:=> [:cat "SmallInt"] :int]
;          [:=> [:cat "SmallInt" "SmallInt" [:* "SmallInt"]] :int]],
; :value #object[malli.core_test$eval27255$fn__27256],
; :errors (#Error{:path [],
;                 :in [],
;                 :schema [:function
;                          {:registry {"SmallInt" [:int {:min -100, :max 100}]}}
;                          [:=> [:cat "SmallInt"] :int]
;                          [:=> [:cat "SmallInt" "SmallInt" [:* "SmallInt"]] :int]],
;                 :value #object[malli.core_test$eval27255$fn__27256],
;                 :check ({:total-nodes-visited 2,
;                          :depth 1,
;                          :pass? false,
;                          :result false,
;                          :result-data nil,
;                          :time-shrinking-ms 0,
;                          :smallest [(0 0)],
;                          :malli.generator/explain-output {:schema :int,
;                                                           :value "00",
;                                                           :errors (#Error{:path []
;                                                                           :in []
;                                                                           :schema :int
;                                                                           :value "00"})}})})}


(def generated-f (mg/generate MyFunction))

(generated-f)
; =throws=> :malli.core/invalid-arity {:arity 0, :arities #{1 :varargs}, :args nil, :input nil, :schema [:function {:registry {"SmallInt" [:int {:min -100, :max 100}]}} [:=> [:cat "SmallInt"] :int] [:=> [:cat "SmallInt" "SmallInt" [:* "SmallInt"]] :int]]}

(generated-f 1)
; => -3237

(generated-f 1 2)
; => --543

(generated-f 1 2 3 4)
; => -2326
```

## Instrumentation
