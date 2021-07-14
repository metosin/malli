(ns malli.demo)

(require '[malli.experimental.schema :as ms])
(require '[malli.dev :as dev])
(require '[malli.core :as m])

(dev/start!)

;; plumatic-style inline schemas
(ms/defn fun :- [:tuple :int :int]
  "return number and the square"
  [x :- :int]
  [x (* x x)])

;; annotating existing clojure functions
(defn times [x y] (* x y))
(m/=> times [:=> [:cat :int :int] :int])

;; static analysis via clj-kondo
(comment
  (fun "1")
  (times "1" 1))



(comment

  (meta #'fun)
  (square -11)
  ; Execution error (ExceptionInfo) at malli.core/-fail! (core.cljc:80).
  ; :malli.experimental.schema/fn-error
  ;
  ;	   name: demo/square
  ;	  phase: :input
  ;	 schema: [:tuple [:int {:min -10}]]
  ;	  value: [-11]
  ;	 errors: [["should be at least -10"]]


  (meta #'square)
  ;{:schema [:or [:-> [:tuple :int] [:int {:min 0}]]],
  ; :ns #object[clojure.lang.Namespace 0x3c5f3ba8 "demo"],
  ; :name square,
  ; :file "/Users/tommi/projects/metosin/malli/src/malli/schema.cljc",
  ; :column 1,
  ; :raw-arglists ([x :- :int]),
  ; :line 64,
  ; :arglists ([x]),
  ; :doc "\n[:or\n [:-> [:tuple :int] [:int {:min 0}]]]"}

  #_(clojure.repl/doc square)

  #_(clojure.repl/doc fun)
  ; -------------------------
  ; demo/fun
  ; ([x] [x y])
  ;
  ;   [:-> [:tuple :int] any?]
  ;   [:-> [:tuple :int :int] [:tuple :int [:int {:min 0}]]]
  ;
  ;   returns a tuple of a number and it's value squared

  (meta #'fun)

  (fun 2)


  [:=>
   [:tuple :int [:int {:min 0}]]
   [:tuple :int]
   [:tuple :int :int]]

  [:=>
   [:tuple :int [:int {:min 0}]]
   [[:tuple :int]
    [:tuple :int :int]]]

  [:=> {:output [:tuple :int [:int {:min 0}]]
        :input [[:tuple :int]
                [:tuple :int :int]]}]

  [:=> {:output [:tuple :int [:int {:min 0}]]
        :inputs [[:tuple :int :int]]}]

  [:=> {:- [:tuple :int [:int {:min 0}]]}
   [:tuple :int]
   [:tuple :int :int]]

  (comment
    (require '[schema.core])

    (schema.core/defn ^:always-validate fun2
      "returns a tuple of a number and it's value squared"
      ([x :- Long, y :- Long]
       [x (* x x)]))

    (schema.core/defn ^:always-validate fun3
      "returns a tuple of a number and it's value squared"
      ([x :- Long] :- Long
       (fun3 x x))
      ([x :- Long, y :- Long]
       [x (* x x)]))

    (meta #'fun)

    ; (=>* Any (java.lang.Long) (java.lang.Long java.lang.Long))

    (defn fun4
      "returns a tuple of a number and it's value squared"
      ([x]
       (fun4 x x))
      ([x y]
       [x (* x x)]))

    (ms/=> fun {:output [:tuple :int [:int {:min 0}]]
                :input [[:tuple :int]
                        [:tuple :int :int]]})



    (ms/=> fun {:arities {1 {:output :int
                             :input [:tuple :int]}
                          2 {:output [:tuple :int [:int {:min 0}]]
                             :input [:tuple :int :int]}}})


    (defn fun1 [x] (* x x))

    ;; short
    (ms/=> fun1 [:=> :int [:tuple [:int {:min 0}]]])

    ;; long
    (ms/=> fun1 {:arities {1 {:input :int
                              :output [:tuple [:int {:min 0}]]}}})


    (defn fun5
      ([x] (fun5 x x))
      ([x y] [x (* x x)]))

    ;; short
    (ms/=> fun [:or
                [:=> :int [:tuple :int]]
                [:=> [:tuple :int [:int {:min 0}]] [:tuple :int]]])

    ;; long
    (ms/=> fun {:arities {1 {:output :int
                             :input [:tuple :int]}
                          2 {:output [:tuple :int [:int {:min 0}]]
                             :input [:tuple :int :int]}}})))
