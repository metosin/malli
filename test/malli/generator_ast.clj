(ns malli.generator-ast
  "For inspecting a malli's generator as data. See `generator-ast`"
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.walk :as walk]
            [clojure.test.check.generators :as tcgen]
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
         (or (-> g meta ::mg/generator-ast)
             g)))
     (generator ?schema (assoc options ::mg/generator-ast true)))))

(defn- qualify-in-ns [q]
  {:pre [(qualified-symbol? q)]}
  (or (when-some [v (resolve q)]
        (when (var? v)
          (when (= q (symbol v))
            (symbol (name q)))))
      (when-some [nsym (some (fn [[asym ns]]
                               (when (= (symbol (namespace q))
                                        (ns-name ns))
                                 (ns-name ns)))
                             (ns-aliases *ns*))]
        (symbol (name nsym) (name q)))
      q))

(defn- explicate [form]
  (walk/postwalk (fn [form]
                   (cond-> form
                     (qualified-symbol? form) qualify-in-ns))))

(defmulti -generator-code (fn [ast _options] (:op ast)))
(defmethod -generator-code :any [_ _] (explicate `tcgen/any))
(defmethod -generator-code :some [_ _] (explicate `tcgen/any-printable))
(defmethod -generator-code :nil [_ _] (explicate `(tcgen/return nil)))
(defmethod -generator-code :recursive-gen [{:keys [rec-gen scalar-gen]} options]
  (explicate `(tcgen/recursive-fn (fn [rec#]
                                    (-generator-code rec-gen options))
                                  ~(-generator-code scalar-gen options))))

(defn generator-code 
  ([?schema] (generator-code ?schema nil))
  ([?schema options]
   (let [ast (-> (generator-ast ?schema options))]
     (-generator-code ast))))
