(ns malli.sci
  (:require [sci.core :as sci]))

(defn evaluator [options]
   (let [ctx (sci/init options)]
     (fn eval [s] (sci/eval-string* (sci/fork ctx) s))))
