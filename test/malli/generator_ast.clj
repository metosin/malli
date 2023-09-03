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
            (str/replace-first "clojure.test.check.generators" "malli.generator-debug")
            (str/replace #"::(\S[^\/]+?)" "::mg/$1"))]
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
  (or (when-some [v (get (ns-map *ns*) (symbol (name q)))]
        (when (var? v)
          (when (= q (symbol v))
            (let [uq (symbol (name q))]
              ;; prevent recursive-gen bindings from shadowing globals
              (when (not (re-matches #"recur\d+" (name uq)))
                uq)))))
      (when-some [nsym (some (fn [[asym ns]]
                               (when (= (symbol (namespace q))
                                        (ns-name ns))
                                 asym))
                             (ns-aliases *ns*))]
        (symbol (name nsym) (name q)))
      q))

(defmulti -generator-code (fn [ast _options] (:op ast)))
(defmethod -generator-code :any [_ _] (qualify-in-ns `tcgen/any))
(defmethod -generator-code :one-of [{:keys [generators]} options]
  (list (qualify-in-ns `tcgen/one-of) (mapv #(-generator-code % options) generators)))
(defmethod -generator-code :return [{:keys [value]} _]
  (list (qualify-in-ns `tcgen/return)
        (cond->> value
          (not ((some-fn string? keyword? nil? boolean? number?) value))
          (list 'quote))))
(defmethod -generator-code :recursive-gen [{:keys [target rec-gen scalar-gen]} options]
  (list (qualify-in-ns `tcgen/recursive-gen)
        (list (qualify-in-ns `fn) [(symbol target)] (-generator-code rec-gen options))
        (-generator-code scalar-gen options)))
;; TODO infer pretty name from :ref schema
(defmethod -generator-code :recur [{:keys [target]} options] (symbol target))
(defmethod -generator-code :tuple [{:keys [generators]} options]
  (list* (qualify-in-ns `tcgen/tuple)
         (mapv #(-generator-code % options) generators)))

(defn generator-code
  "Return pretty code that can be evaluated in the current namespace
  to create a generator for schema."
  ([?schema] (generator-code ?schema nil))
  ([?schema options]
   (-> ?schema
       (generator-ast options)
       (-generator-code options))))
