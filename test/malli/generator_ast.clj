(ns malli.generator-ast
  "For inspecting a malli's generator as data. See `generator-ast`"
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.walk :as walk]
            [malli.generator :as mg]))

(let [s (-> (slurp (io/resource "malli/generator.cljc"))
            ;; change the namespace
            (str/replace-first "(ns malli.generator" "(ns malli.generator-ast")
            ;; change the `gen` alias to the AST version
            (str/replace-first "clojure.test.check.generators" "malli.generator-debug"))]
  ;; eval ns form first so keywords can be resolved in the right namespace
  (eval (read-string {:read-cond :allow :features #{:clj}} s))
  (eval (read-string {:read-cond :allow :features #{:clj}} (str "(do " s ")"))))

(defn generator-ast
  "Return a malli schema's generator as an AST."
  ([?schema]
   (generator-ast ?schema nil))
  ([?schema options]
   (walk/postwalk
     (fn [g]
       (if (mg/-unreachable-gen? g)
         {:op :unreachable}
         g))
     (generator ?schema (assoc options ::mg/generator-ast true)))))
