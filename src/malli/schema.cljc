(ns malli.schema
  (:refer-clojure :exclude [defn])
  (:require [clojure.string :as str]
            [malli.schema.impl :as msi]
            [malli.error :as me]
            [malli.core :as m]))

(defmacro set-compile-fn-validation!
  [on?] (msi/set-compile-fn-validation! on?) nil)

(clojure.core/defn fn-validation?
  "Get the current global schema validation setting."
  [] @msi/use-fn-validation)

(clojure.core/defn set-fn-validation!
  "Globally turn on (or off) schema validation for all s/fn and s/defn instances."
  [on?] (reset! msi/use-fn-validation on?))

(defmacro with-fn-validation
  "Execute body with input and output schema validation turned on for
   all s/defn and s/fn instances globally (across all threads). After
   all forms have been executed, resets function validation to its
   previously set value. Not concurrency-safe."
  [& body]
  `(let [body# (fn [] ~@body)]
     (if (fn-validation?)
       (body#)
       (do (set-fn-validation! true)
           (try (body#) (finally (set-fn-validation! false)))))))

(defmacro without-fn-validation
  "Execute body with input and output schema validation turned off for
   all s/defn and s/fn instances globally (across all threads). After
   all forms have been executed, resets function validation to its
   previously set value. Not concurrency-safe."
  [& body]
  `(let [body# (fn [] ~@body)]
     (if (fn-validation?)
       (do (set-fn-validation! false)
           (try (body#) (finally (set-fn-validation! true))))
       (body#))))

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
                       :doc (str "\n  [:or\n   " (str/join "\n   " (rest schemas)) "]" (when doc (str "\n\n  " doc)))
                       :raw-arglists (list 'quote raw-arglists)
                       :schema (list 'quote schemas)
                       :arglists (list 'quote arglists))
                    ~@fn-body)]
         (m/=> ~name ~schemas)
         ret#))))
