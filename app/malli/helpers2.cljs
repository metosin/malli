(ns malli.helpers2
  (:require
   [malli.experimental :as mx]
   [malli.core :as m]
   [malli.helpers :as h :refer [int-schema]]))

(mx/defn square-it :- h/int-schema
  [x :- int?]
  (str x)
  ;(str (* x x))
  )
;(mx/defn f1 [] 1)
;(mx/defn f3 [x :- :int] x)
;(mx/defn f4 :- [:int {:min 0}]
;  "int int -> int functions"
;  [x :- [:int {:min 0}], y :- :int]
;  (+ x y))

(def AB [:map [:a [:int {:min 0}]] [:b :int]])
(def CD [:map [:c [:int {:min 0}]] [:d :int]])

;; schematized, nested keywords args
(mx/defn f5 :- [:cat :int :int :int :int AB CD]
  "Nested Keyword argument"
  [[& {:keys [a b] :as m1} :- AB]
   & {:keys [c d] :as m2} :- CD]
  [a b c d m1 m2])

(mx/defn f3 [x :- :int] x)
(comment
  (macroexpand
   '(mx/defn f3 [x :- :int] x))
  (macroexpand
   '(mx/defn f5 :- [:cat :int :int :int :int AB CD]
      "Nested Keyword argument"
      [[& {:keys [a b] :as m1} :- AB]
       & {:keys [c d] :as m2} :- CD]
      [a b c d m1 m2])))

;(defn square-it [x] (* x x))
;(m/=> square-it [:=> [:cat h/int-schema] int-schema])

;(mx/defn square-it2 :- :int     ; h/int-schema
;  [x :- :int]
;  (* x x))

;(comment
;  (macroexpand
;    '(m/=> square-it [:=> [:cat h/int-schema] :int])
;    )
;  (macroexpand-1
;    '(mx/defn square-it :- :int     ; h/int-schema
;      [x :- :int]
;      (* x x))
;    ))
