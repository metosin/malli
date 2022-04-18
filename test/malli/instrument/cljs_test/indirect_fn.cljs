(ns malli.instrument.cljs-test.indirect-fn
  "Bug to do with when things are instrumented
   This declared functions here to be used in cljs_test"
  (:require [malli.core :as m]
            [malli.experimental :as mx]
            [malli.instrument.cljs-test.defs
             :as defs
             :refer [defs-small-int defs-int-arg]]))

;; There's a matrix of things to test here
;; Calling either arg or return type from another namespace
;;  vs whether doing it as a refer or as a namespaced var
;; This hits all 4
(mx/defn power-ret-refer :- defs-small-int
  "Ret, refer"
  [x :- :int] (* x x))
(mx/defn power-ret-ns :- defs/defs-small-int
  "Ret, ns"
  [x :- :int] (* x x))
(mx/defn power-arg-refer :- [:int {:max 6}]
  "Arg, refer"
  [x :- defs-int-arg] (* x x))
(mx/defn power-arg-ns :- [:int {:max 6}]
  "Arg, ns"
  [x :- defs/defs-int-arg] (* x x))
