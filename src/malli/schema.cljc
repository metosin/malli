(ns malli.schema
  (:refer-clojure :exclude [defn])
  (:require [clojure.string :as str]
            [malli.schema.macros :as msm]
            [malli.error :as me]
            [malli.core :as m]))

(clojure.core/defn fn-validator
  "A var that can be rebound to a function to customize the behavior
  of fn validation. When fn validation is on and `fn-validator` is
  bound to a function, normal argument and return value checks will
  be substituted with a call to this function with five arguments:

    direction   - :input or :output
    fn-name     - a symbol, the function's name
    schema      - the schema for the arglist or the return value
    checker     - a precompiled checker to check a value against
                  the schema
    value       - the actual arglist or return value

  The function's return value will be ignored."
  [direction fn-name schema explainer value]
  (when-let [error (explainer value)]
    (let [humanized (me/humanize error)]
      (m/-fail! ::fn-error
                (msm/format*
                  (str
                    (str/capitalize (name direction)) " to %s does not match schema: \n\n"
                    "\t\u001B[0;37m schema: \u001B[0;33m%s \033[0m\n"
                    "\t\u001B[0;37m  value: \u001B[0;33m%s\u001B[0m\n"
                    "\t\u001B[0;37m  error: \u001B[0;33m%s\u001B[0m\n\n")
                  fn-name (m/form schema) (pr-str value) (pr-str humanized))
                {:direction direction, :schema schema :value value :error error, :humanized humanized}))))

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
         (msm/declare-class-schema! (msm/fn-schema-bearer ~name) ~schema-form)
         ret#))))

(clojure.core/defn => [_var _data])

(ns demo)

(require '[malli.schema :as ms])

(ms/defn ^:always-validate fun :- [:tuple int? pos-int?]
         "returns a tuple of a number and it's value squared"
         ([x :- int?]
          (fun x x))
         ([x :- int?, y :- int?]
          [x (* x x)]))

(ms/defn ^:always-validate fun :- [:tuple int? pos-int?]
         "returns a tuple of a number and it's value squared"
         ([x :- int?]
          (fun x x))
         ([x :- int?, y :- int?]
          [x (* x x)]))

[:=>
 [:tuple int? pos-int?]
 [:tuple int?]
 [:tuple int? int?]]

[:=>
 [:tuple int? pos-int?]
 [[:tuple int?]
  [:tuple int? int?]]]

[:=> {:output [:tuple int? pos-int?]
      :input [[:tuple int?]
              [:tuple int? int?]]}]

[:=> {:output [:tuple int? pos-int?]
      :inputs [[:tuple int? int?]]}]

[:=> {:- [:tuple int? pos-int?]}
 [:tuple int?]
 [:tuple int? int?]]

(fun 2 "2")

(require '[schema.core])

(schema.core/defn ^:always-validate fun
  "returns a tuple of a number and it's value squared"
  ([x :- Long, y :- Long]
   [x (* x x)]))

(schema.core/defn ^:always-validate fun
  "returns a tuple of a number and it's value squared"
  ([x :- Long] :- Long
   (fun x x))
  ([x :- Long, y :- Long]
   [x (* x x)]))

(meta #'fun)

; (=>* Any (java.lang.Long) (java.lang.Long java.lang.Long))

(defn => [var data])

(defn fun
  "returns a tuple of a number and it's value squared"
  ([x]
   (fun x x))
  ([x y]
   [x (* x x)]))

(ms/=> fun {:output [:tuple int? pos-int?]
            :input [[:tuple int?]
                    [:tuple int? int?]]})



(ms/=> fun {:arities {1 {:output int?
                         :input [:tuple int?]}
                      2 {:output [:tuple int? pos-int?]
                         :input [:tuple int? int?]}}})


(defn fun1 [x] (* x x))

;; short
(ms/=> fun1 [:=> int? [:tuple pos-int?]])

;; long
(ms/=> fun1 {:arities {1 {:input int?
                          :output [:tuple pos-int?]}}})


(defn fun
  ([x] (fun x x))
  ([x y] [x (* x x)]))

;; short
(ms/=> fun [:or
            [:=> int? [:tuple int?]]
            [:=> [:tuple int? pos-int?] [:tuple int?]]])

;; long
(ms/=> fun {:arities {1 {:output int?
                         :input [:tuple int?]}
                      2 {:output [:tuple int? pos-int?]
                         :input [:tuple int? int?]}}})
