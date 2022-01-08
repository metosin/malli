(ns malli.instrument.cljs
  (:require [cljs.analyzer.api :as ana-api]
            [malli.core :as m]
            [malli.generator :as mg]))

;;
;; Collect schemas - register them into the known malli.core/-function-schemas* atom based on their metadata.
;;

(defn -collect! [simple-name {:keys [meta] :as var-map}]
  (let [ns (symbol (namespace (:name var-map)))
        schema (:malli/schema meta)]
    (when schema
      (m/-register-function-schema! ns simple-name schema (m/-unlift-keys meta "malli"))
      `(do (m/-register-function-schema! '~ns '~simple-name ~schema ~(m/-unlift-keys meta "malli"))
           '~(:name var-map)))))

(defn -sequential [x] (cond (set? x) x (sequential? x) x :else [x]))

;; intended to be called from a cljs macro
(defn -collect-all-ns []
  `(collect! {:ns ~(ana-api/all-ns)}))

;;
;; instrument
;;

(defn -emit-instrument-fn [{:keys [gen filters] :as instrument-opts} {:keys [schema] :as schema-map} ns-sym fn-sym]
  ;; gen is a function
  (let [schema-map (assoc (select-keys schema-map [:gen :scope :report])
                          ;; At macroexpansion time the schema will be a JVM object, this converts it to a JS object.
                          :schema `(m/function-schema ~(m/form schema)))
        schema-map-with-gen
        (as-> (merge (select-keys instrument-opts [:scope :report :gen]) schema-map) $
              ;; use the passed in gen fn to generate a value
              (cond (and gen (true? (:gen schema-map))) (assoc $ :gen gen)
                    :else (dissoc $ :gen)))
        replace-var-code
        `(do
           (swap! instrumented-vars #(assoc % '~fn-sym ~fn-sym))
           (set! ~fn-sym (m/-instrument ~schema-map-with-gen ~fn-sym))
           (.log js/console "..instrumented" '~fn-sym)
           '~fn-sym)]
    (if filters
      `(when (some #(% '~ns-sym (var ~fn-sym) ~schema-map) ~filters)
         ~replace-var-code)
      replace-var-code)))

(defn -instrument [{:keys [data] :or {data (m/function-schemas)} :as opts}]
  (let [r
        (reduce
         (fn [acc [ns-sym sym-map]]
           (reduce-kv
            (fn [acc' fn-sym schema-map]
              (conj acc'
                    (-emit-instrument-fn opts schema-map ns-sym (symbol (str ns-sym) (str fn-sym)))))
            acc sym-map)) [] data)]
    `(filterv some? ~r)))

;;
;; unstrument
;;

(defn -emit-unstrument-fn [{:keys [schema filters] :as opts} ns-sym fn-sym]
  (let [opts (assoc (select-keys opts [:gen :scope :report])
                    ;; At macroexpansion time the schema will be a JVM object, this converts it to a JS object.
                    :schema `(m/function-schema ~(m/form schema)))
        replace-with-orig
        `(when-let [orig-fn# (get @instrumented-vars '~fn-sym)]
           (swap! instrumented-vars #(dissoc % '~fn-sym))
           (set! ~fn-sym orig-fn#)
           (.log js/console "..unstrumented" '~fn-sym)
           '~fn-sym)]
    (if filters
      `(when (some #(% ~ns-sym (var ~fn-sym) ~opts) ~filters)
         ~replace-with-orig)
      replace-with-orig)))

(defn -unstrument [opts]
  (let [r (reduce
           (fn [acc [ns-sym sym-map]]
             (reduce-kv
              (fn [acc' fn-sym schema-map]
                (conj acc'
                      (-emit-unstrument-fn (assoc opts :schema (:schema schema-map))
                                           ns-sym
                                           (symbol
                                            (str ns-sym) (str fn-sym)))))
              acc sym-map))
           []
           (m/function-schemas))]
    `(filterv some? ~r)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generative testing, check function return values vs their parameters
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn -emit-check [{:keys [schema]} fn-sym]
  `(let [schema# (m/function-schema ~(m/form schema))
         fn# (or (get @instrumented-vars '~fn-sym) '~fn-sym)]
     (when-let [err# (mg/check schema# fn#)]
       ['~fn-sym err#])))

(defn -check []
  (let [r (reduce (fn [acc [ns-sym sym-map]]
                    (reduce-kv (fn [acc' fn-sym schema-map]
                                 (conj acc' (-emit-check schema-map (symbol (str ns-sym) (str fn-sym)))))
                               acc sym-map)) [] (m/function-schemas))]
    `(into {} ~r)))

;;
;; public api
;;

(defmacro check
  "Checks all registered function schemas using generative testing.
   Returns nil or a map of symbol -> explanation in case of errors."
  [] (-check))

(defmacro collect!
  "Reads all public Vars from a given namespace(s) and registers a function (var) schema if `:malli/schema`
   metadata is present. The following metadata key can be used:

   | key             | description |
   | ----------------|-------------|
   | `:malli/schema` | function schema
   | `:malli/scope`  | optional set of scope definitions, defaults to `#{:input :output}`
   | `:malli/report` | optional side-effecting function of `key data -> any` to report problems, defaults to `m/-fail!`
   | `:malli/gen`    | optional value `true` or function of `schema -> schema -> value` to be invoked on the args to get the return value"
  ([] `(collect! ~{:ns (symbol (str *ns*))}))
  ([{:keys [ns]}]
   (reduce (fn [acc [var-name var-map]] (let [v (-collect! var-name var-map)] (cond-> acc v (conj v))))
           #{}
           (mapcat (fn [n]
                     (let [ns-sym (cond (symbol? n) n
                                        ;; handles (quote ns-name) - quoted symbols passed to cljs macros show up this way.
                                        (list? n) (second n)
                                        :else (symbol (str n)))]
                       (ana-api/ns-publics ns-sym)))
                   (-sequential ns)))))

(defmacro instrument!
  "Applies instrumentation for a filtered set of function Vars (e.g. `defn`s).
   See [[malli.core/-instrument]] for possible options."
  ([] (-instrument {}))
  ([opts] (-instrument opts)))

(defmacro unstrument!
  "Removes instrumentation from a filtered set of function Vars (e.g. `defn`s).
   See [[malli.core/-instrument]] for possible options."
  ([] (-unstrument {}))
  ([opts] (-unstrument opts)))
