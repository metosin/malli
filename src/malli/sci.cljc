(ns malli.sci
  (:require #?(:clj  [borkdude.dynaload-clj :refer [dynaload]]
               :cljs [borkdude.dynaload-cljs :refer-macros [dynaload]])))

(defn evaluator [options]
  (let [eval-string* @(dynaload 'sci.core/eval-string* {:default nil})
        init @(dynaload 'sci.core/init {:default nil})
        fork @(dynaload 'sci.core/fork {:default nil})]
    (if (and eval-string* init fork)
      (let [ctx (init options)]
        (fn eval [s] (eval-string* (fork ctx) s))))))
