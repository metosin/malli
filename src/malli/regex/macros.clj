(ns malli.regex.macros
  (:require [malli.regex.compiler :refer [op->opcode label-ops]]))

(defmacro asm [& exprs]
  (let [gen (gensym 'gen)
        exprs (partition 2 exprs)]
    `(let [~gen (memoize gensym)]
       (concat
         ~@(map
             (fn [[op arg]]
               (let [op (keyword op)]
                 (if (contains? op->opcode op)
                   [op (if (contains? label-ops op)
                         `(~gen ~(keyword arg))
                         arg)]
                   (do
                     (assert (= op :include))
                     arg))))
             exprs)))))

(defmacro opcode-case [op & body]
  (letfn [(convert-pat [pat]
            (if (symbol? pat)
              (doto (get op->opcode (keyword pat)) assert)
              (map convert-pat pat)))]
    `(case ~op
       ~@(->> body
              (partition 2)
              (mapcat (fn [[pat expr]] [(convert-pat pat) expr]))))))
