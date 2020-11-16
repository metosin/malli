(ns malli.schema
  (:refer-clojure :exclude [defn])
  (:require [clojure.string :as str]
            [malli.schema.impl :as msi]
            [malli.error :as me]
            [malli.core :as m]))

(clojure.core/defn fn-validator
  "A var that can be rebound to a function to customize the behavior
  of fn validation. When fn validation is on and `fn-validator` is
  bound to a function, normal argument and return value checks will
  be substituted with a call to this function with five arguments:

    phase       - :input or :output
    fn-name     - a symbol, the function's name
    schema      - the schema for the arglist or the return value
    checker     - a precompiled checker to check a value against
                  the schema
    value       - the actual arglist or return value

  The function's return value will be ignored."
  [phase ns fn-name schema explainer value]
  (when-let [error (explainer value)]
    (let [humanized (me/humanize error)]
      (m/-fail! ::fn-error
                (msi/format*
                  (str "\n\n"
                       "\t\u001B[0;37m   name: \u001B[0;33m%s \033[0m\n"
                       "\t\u001B[0;37m  phase: \u001B[0;33m%s \033[0m\n"
                       "\t\u001B[0;37m schema: \u001B[0;33m%s \033[0m\n"
                       "\t\u001B[0;37m  value: \u001B[0;33m%s\u001B[0m\n"
                       "\t\u001B[0;37m errors: \u001B[0;33m%s\u001B[0m\n\n")
                  (symbol (str ns) (str fn-name)) phase (m/form schema) (pr-str value) (pr-str humanized))
                {:phase phase, :schema schema :value value :error error, :humanized humanized}))))

(defmacro defn [& defn-args]
  (let [[name & more-defn-args] (msi/normalized-defn-args &env defn-args)
        {:keys [doc tag] :as standard-meta} (meta name)
        {:keys [outer-bindings schemas fn-body arglists raw-arglists]} (msi/process-fn- &env name more-defn-args)]
    `(let ~outer-bindings
       (let [ret# (clojure.core/defn ~(with-meta name {})
                    ~(assoc (apply dissoc standard-meta (when (msi/primitive-sym? tag) [:tag]))
                       :malli/fn true
                       :doc (str "\n  [:or\n   " (str/join "\n   " (rest schemas)) "]" (when doc (str "\n\n  " doc)))
                       :raw-arglists (list 'quote raw-arglists)
                       :schema (list 'quote schemas)
                       :arglists (list 'quote arglists))
                    ~@fn-body)]
         (msi/declare-class-schema! (msi/fn-schema-bearer ~name) ~schemas)
         ret#))))
