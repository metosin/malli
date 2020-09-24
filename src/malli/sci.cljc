(ns malli.sci
  (:require [borkdude.dynaload :as dynaload]))

(defn evaluator [options fail!]
  (let [eval-string* (dynaload/dynaload 'sci.core/eval-string* {:default nil})
        init (dynaload/dynaload 'sci.core/init {:default nil})
        fork (dynaload/dynaload 'sci.core/fork {:default nil})]
    (fn [] (if (and @eval-string* @init @fork)
             (let [ctx (init options)]
               (fn eval [s] (eval-string* (fork ctx) (str s))))
             fail!))))
