(ns malli.experimental.schema
  (:refer-clojure :exclude [defn])
  (:require [clojure.string :as str]
            [clojure.core :as c]
            [malli.core :as m]
            #?@(:cljs [goog.string.format
                       [goog.object :as gobject]
                       [goog.string :as gstring]])))

(c/defn format* [fmt & args]
  (apply #?(:clj format, :cljs gstring/format) fmt args))

(c/defn cljs-env? [env] (boolean (:ns env)))
(defmacro if-cljs [then else] (if (cljs-env? &env) then else))

(defmacro error!
  "Generate a cross-platform exception appropriate to the macroexpansion context"
  ([s]
   `(if-cljs
      (throw (js/Error. ~s))
      (throw (RuntimeException. ~(with-meta s `{:tag java.lang.String})))))
  ([s m]
   (let [m (merge {:type :schema.core/error} m)]
     `(if-cljs
        (throw (ex-info ~s ~m))
        (throw (clojure.lang.ExceptionInfo. ~(with-meta s `{:tag java.lang.String}) ~m))))))

(defmacro assert!
  "Like assert, but throws a RuntimeException (in Clojure) and takes args to format."
  [form & format-args]
  `(when-not ~form
     (error! (format* ~@format-args))))

(c/defn maybe-split-first [pred s]
  (if (pred (first s))
    [(first s) (next s)]
    [nil s]))

(def primitive-sym? '#{float double boolean byte char short int long
                       floats doubles booleans bytes chars shorts ints longs objects})

(c/defn valid-tag? [env tag]
  (and (symbol? tag) (or (primitive-sym? tag) (class? (resolve env tag)))))

(c/defn normalized-metadata
  "Take an object with optional metadata, which may include a :tag,
   plus an optional explicit schema, and normalize the
   object to have a valid Clojure :tag plus a :schema field."
  [env imeta explicit-schema]
  (let [{:keys [tag s s? schema]} (meta imeta)]
    (assert! (not (or s s?)) "^{:s schema} style schemas are no longer supported.")
    (assert! (< (count (remove nil? [schema explicit-schema])) 2)
             "Expected single schema, got meta %s, explicit %s" (meta imeta) explicit-schema)
    (let [schema (or explicit-schema schema tag :any)
          tag (let [t (or tag schema)] (if (valid-tag? env t) t))]
      (with-meta
        imeta (-> (or (meta imeta) {}) (dissoc :tag) (cond-> schema (assoc :schema schema) tag (assoc :tag tag)))))))

(c/defn extract-schema-form
  "Pull out the schema stored on a thing.  Public only because of its use in a public macro."
  [symbol]
  (let [s (:schema (meta symbol))]
    (assert! s "%s is missing a schema" symbol)
    s))

(c/defn extract-arrow-schematized-element
  "Take a nonempty seq, which may start like [a ...] or [a :- schema ...], and return
   a list of [first-element-with-schema-attached rest-elements]"
  [env s]
  (assert (seq s))
  (let [[f & more] s]
    (if (= :- (first more))
      [(normalized-metadata env f (second more)) (drop 2 more)]
      [(normalized-metadata env f nil) more])))

(c/defn process-arrow-schematized-args
  "Take an arg vector, in which each argument is followed by an optional :- schema,
   and transform into an ordinary arg vector where the schemas are metadata on the args."
  [env args]
  (loop [in args out []]
    (if (empty? in)
      out
      (let [[arg more] (extract-arrow-schematized-element env in)]
        (recur more (conj out arg))))))

(c/defn split-rest-arg [env bind]
  (let [[pre-& [_ rest-arg :as post-&]] (split-with #(not= % '&) bind)]
    (if (seq post-&)
      [(vec pre-&)
       (if (vector? rest-arg)
         (with-meta (process-arrow-schematized-args env rest-arg) (meta rest-arg))
         rest-arg)]
      [bind nil])))

(c/defn single-arg-schema-form [rest? [index arg]]
  (prn "!!!!" rest? index arg)
  #_`(~(if rest? `schema.core/optional `schema.core/one)
       ~(extract-schema-form arg)
       ~(if (symbol? arg)
          `'~arg
          `'~(symbol (str (if rest? "rest" "arg") index))))
  `[~(extract-schema-form arg) {:name ~(if (symbol? arg)
                                         `'~arg
                                         `'~(symbol (str (if rest? "rest" "arg") index)))}]
  (extract-schema-form arg))

(c/defn simple-arglist-schema-form [rest? regular-args]
  (into [:cat] (mapv (partial single-arg-schema-form rest?) (map-indexed vector regular-args))))

(c/defn rest-arg-schema-form [arg]
  (let [s (extract-schema-form arg)]
    (if (= s :any)
      (if (vector? arg)
        (simple-arglist-schema-form true arg)
        [[:* :any]])
      (do (assert! (vector? s) "Expected seq schema for rest args, got %s" s)
          [s]))))

(c/defn input-schema-form [regular-args rest-arg]
  (let [base (simple-arglist-schema-form false regular-args)]
    (if rest-arg (vec (concat base (rest-arg-schema-form rest-arg))) base)))

(c/defn process-body-return [body]
  (if (= :- (first body))
    [(second body) (drop 2 body)]
    [nil body]))

(c/defn process-fn-arity [env _ns _fn-name top-output-schema bind-meta [bind & body]]
  (assert! (vector? bind) "Got non-vector binding form %s" bind)
  (when-let [bad-meta (seq (filter (or (meta bind) {}) [:tag :s? :s :schema]))]
    (throw (RuntimeException. (str "Meta not supported on bindings, put on fn name" (vec bad-meta)))))
  (let [original-arglist bind
        [output-schema body] (process-body-return body)
        bind (with-meta (process-arrow-schematized-args env bind) bind-meta)
        [regular-args rest-arg] (split-rest-arg env bind)
        input-schema (input-schema-form regular-args rest-arg)
        output-schema (or output-schema top-output-schema)]
    {:arglist bind
     :raw-arglist original-arglist
     :schema [:=> input-schema output-schema]
     :arity-form (cons (into regular-args (when rest-arg ['& rest-arg])) body)}))

(c/defn process-fn- [env name fn-body]
  (let [output-schema (extract-schema-form name)
        bind-meta (or (when-let [t (:tag (meta name))] (when (primitive-sym? t) {:tag t})) {})
        processed-arities (map (partial process-fn-arity env *ns* name output-schema bind-meta)
                               (if (vector? (first fn-body)) [fn-body] fn-body))
        fn-forms (map :arity-form processed-arities)]
    {:arglists (map :arglist processed-arities)
     :schema (into [:function] (mapv :schema processed-arities))
     :raw-arglists (map :raw-arglist processed-arities)
     :fn-body fn-forms}))

(c/defn normalized-defn-args [env macro-args]
  (let [[name macro-args] (extract-arrow-schematized-element env macro-args)
        [maybe-docstring macro-args] (maybe-split-first string? macro-args)
        [maybe-attr-map macro-args] (maybe-split-first map? macro-args)]
    (cons (vary-meta name merge
                     (or maybe-attr-map {})
                     (when maybe-docstring {:doc maybe-docstring}))
          macro-args)))

(c/defn schema-doc [doc schema]
  (str doc "\n\n  [:function\n   " (str/join "\n   " (rest schema)) "]"))

;;
;; public api
;;

(defmacro defn [& defn-args]
  (let [[name & more-defn-args] (normalized-defn-args &env defn-args)
        {:keys [doc tag] :as standard-meta} (meta name)
        {:keys [schema fn-body arglists raw-arglists]} (process-fn- &env name more-defn-args)
        data (assoc (apply dissoc standard-meta (when (primitive-sym? tag) [:tag]))
               :doc (schema-doc doc schema)
               :raw-arglists (list 'quote raw-arglists)
               :schema (list 'quote schema)
               :arglists (list 'quote arglists))]
    `(let [defn# (c/defn ~(with-meta name {}) ~data ~@fn-body)]
       (m/=> ~name ~schema)
       defn#)))
