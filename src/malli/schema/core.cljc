(ns malli.schema.core
  (:refer-clojure :exclude [defn])
  (:require [schema.core]
            [malli.schema.macros :as msm]
            [malli.schema.utils :as msu]))

(defmacro defn [& defn-args]
  (let [[name & more-defn-args] (msm/normalized-defn-args &env defn-args)
        {:keys [doc tag] :as standard-meta} (meta name)
        {:keys [outer-bindings schema-form fn-body arglists raw-arglists]} (msm/process-fn- &env name more-defn-args)]
    `(let ~outer-bindings
       (let [ret# (clojure.core/defn ~(with-meta name {})
                    ~(assoc (apply dissoc standard-meta (when (msm/primitive-sym? tag) [:tag]))
                       :doc (str
                              (str "Inputs: " (if (= 1 (count raw-arglists))
                                                (first raw-arglists)
                                                (apply list raw-arglists)))
                              (when-let [ret (when (= (second defn-args) :-) (nth defn-args 2))]
                                (str "\n  Returns: " ret))
                              (when doc (str "\n\n  " doc)))
                       :raw-arglists (list 'quote raw-arglists)
                       :arglists (list 'quote arglists)
                       :schema schema-form)
                    ~@fn-body)]
         (msu/declare-class-schema! (msu/fn-schema-bearer ~name) ~schema-form)
         ret#))))

(ns demo)

(require '[malli.schema.core :as m])

(m/defn ^:always-validate fun :- [:tuple int? pos-int?]
  "returns a tuple of a number and it's value squared"
  ([x :- int?]
   (fun x x))
  ([x :- int?, y :- int?]
   [x (* x x)]))

(fun 2 "2")

(meta #'fun)

(macroexpand-1
  `(schema.core/defn ^:always-validate fun
     "returns a tuple of a number and it's value squared"
     ([x :- Long]
      (fun x x))
     ([x :- Long, y :- Long]
      [x (* x x)])))
