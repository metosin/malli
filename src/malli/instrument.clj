(ns malli.instrument
  (:require [clojure.walk :as walk]
            [malli.core :as m]
            [malli.generator :as mg]))

(defn -find-var [n s] (find-var (symbol (str n "/" s))))
(defn -sequential [x] (cond (set? x) x (sequential? x) x :else [x]))
(defn -f->original [f] (-> f meta ::original (or f)))
(defn -original [v] (let [f (deref v)] (-f->original f)))

(defn -filter-ns [& ns] (let [ns (set ns)] (fn [n _ _] (contains? ns n))))
(defn -filter-var [f] (fn [n s _] (f (-find-var n s))))
(defn -filter-schema [f] (fn [_ _ {:keys [schema]}] (f schema)))

(defn- -primitive-fn? [f]
  (and (fn? f) (boolean (some (fn [^Class c] (.startsWith (.getName c) "clojure.lang.IFn$")) (supers (class f))))))

(defn -strument!
  ([] (-strument! nil))
  ([{:keys [mode data filters gen report] :or {mode :instrument, data (m/function-schemas)} :as options}]
   (doall
    (for [[n d] data, [s d] d]
      (when-let [v (-find-var n s)]
        (when (and (bound? v)
                   (or (not (-primitive-fn? @v))
                       (println (str "WARNING: Not instrumenting primitive fn " v))))
          (when (or (not filters) (some #(% n s d) filters))
            (case mode
              :instrument (let [dgen (as-> (merge (select-keys options [:scope :report :gen]) d) $
                                       (cond-> $ report (update :report (fn [r] (fn [t data] (r t (assoc data :fn-name (symbol (name n) (name s))))))))
                                       (cond (and gen (true? (:gen d))) (assoc $ :gen gen)
                                             (true? (:gen d)) (dissoc $ :gen)
                                             :else $))]
                            (alter-var-root v (fn [f]
                                                (when (-primitive-fn? f)
                                                  (m/-fail! ::cannot-instrument-primitive-fn {:v v}))
                                                (let [f (-f->original f)]
                                                  (-> (m/-instrument dgen f) (with-meta {::original f}))))))
              :unstrument (alter-var-root v -f->original)
              (mode v d))
            v)))))))

(defn -schema [v]
  (let [{:keys [malli/schema arglists]} (meta v)]
    (or schema (as-> (seq (keep (comp :malli/schema meta) arglists)) $
                 (when (= (count arglists) (count $)) (cond->> $ (next $) (into [:function])))))))

(defn -collect! [v]
  (let [{:keys [ns name] :as m} (meta v)]
    (when-let [s (-schema v)] (m/-register-function-schema! (-> ns str symbol) name s (m/-unlift-keys m "malli")))))

(defn clj-collect!
  ([] (clj-collect! {:ns *ns*}))
  ([{:keys [ns]}]
   (not-empty (reduce (fn [acc v] (let [v (-collect! v)] (cond-> acc v (conj v)))) #{} (vals (mapcat ns-publics (-sequential ns)))))))

;;
;; CLJS macro for collecting function schemas
;;

(let [cljs-find-ns (fn [env] (when (:ns env) (ns-resolve 'cljs.analyzer.api 'find-ns)))
      cljs-ns-interns (fn [env] (when (:ns env) (ns-resolve 'cljs.analyzer.api 'ns-interns)))]
  (defn -cljs-collect!* [env simple-name {:keys [meta] :as var-map}]
    ;; when collecting google closure or other js code symbols will not have namespaces
    (when (namespace (:name var-map))
      (let [ns (symbol (namespace (:name var-map)))
            find-ns' (cljs-find-ns env)
            ns-interns' (cljs-ns-interns env)
            schema (:malli/schema meta)]
        (when schema
          (let [-qualify-sym (fn [form]
                               (if (symbol? form)
                                 (if (simple-symbol? form)
                                   (let [ns-data (find-ns' ns)
                                         intern-keys (set (keys (ns-interns' ns)))]
                                     (cond
                                       ;; a referred symbol
                                       (get-in ns-data [:uses form])
                                       (let [form-ns (str (get-in ns-data [:uses form]))]
                                         (symbol form-ns (str form)))

                                       ;; interned var
                                       (contains? intern-keys form)
                                       (symbol (str ns) (str form))

                                       :else
                                       ;; a cljs.core var, do not qualify it
                                       form))
                                   (let [ns-part (symbol (namespace form))
                                         name-part (name form)
                                         full-ns (get-in (find-ns' ns) [:requires ns-part])]
                                     (symbol (str full-ns) name-part)))
                                 form))
                schema* (walk/postwalk -qualify-sym schema)
                metadata (assoc
                           (walk/postwalk -qualify-sym (m/-unlift-keys meta "malli"))
                           :metadata-schema? true)]
            `(do
               (m/-register-function-schema! '~ns '~simple-name ~schema* ~metadata :cljs identity)
               '~(:name var-map))))))))

(defmacro cljs-collect!
  ([] `(cljs-collect! ~{:ns (symbol (str *ns*))}))
  ([opts]
   (let [ns-publics' (when (:ns &env) (ns-resolve 'cljs.analyzer.api 'ns-publics))]
     (reduce (fn [acc [var-name var-map]] (let [v (-cljs-collect!* &env var-name var-map)] (cond-> acc v (conj v))))
             #{}
             (mapcat (fn [n]
                       (let [ns-sym (cond (symbol? n) n
                                          ;; handles (quote ns-name) - quoted symbols passed to cljs macros show up this way.
                                          (list? n) (second n)
                                          :else (symbol (str n)))]
                         (ns-publics' ns-sym)))
                     ;; support quoted vectors of ns symbols in cljs
                     (let [nses (:ns opts)
                           nses (if (and (= 'quote (first nses)) (coll? (second nses)))
                                  (second nses)
                                  nses)]
                       (-sequential nses)))))))

;;
;; public api
;;

(defn check
  "Checks all registered function schemas using generative testing.
   Returns nil or a map of symbol -> explanation in case of errors."
  ([] (check nil))
  ([options]
   (let [res* (atom {})]
     (-strument! (assoc options :mode (fn [v {:keys [schema]}]
                                        (some->> (mg/check schema (-original v))
                                                 (swap! res* assoc (symbol v))))))
     (not-empty @res*))))

(defmacro collect!
  "Reads all public Vars from a given namespace(s) and registers a function (var) schema if `:malli/schema`
   metadata is present. The following metadata key can be used:

   | key             | description |
   | ----------------|-------------|
   | `:malli/schema` | function schema
   | `:malli/scope`  | optional set of scope definitions, defaults to `#{:input :output}`
   | `:malli/report` | optional side-effecting function of `key data -> any` to report problems, defaults to `m/-fail!`
   | `:malli/gen`    | optional value `true` or function of `schema -> schema -> value` to be invoked on the args to get the return value"
  ([] `(collect! {:ns (symbol (str ~'*ns*))}))
  ([opts]
   (if (:ns &env)
     `(cljs-collect! ~opts)
     `(clj-collect! ~opts))))

(defn instrument!
  "Applies instrumentation for a filtered set of function Vars (e.g. `defn`s).
   See [[malli.core/-instrument]] for possible options."
  ([] (instrument! nil))
  ([options] (-strument! (assoc options :mode :instrument))))

(defn unstrument!
  "Removes instrumentation from a filtered set of function Vars (e.g. `defn`s).
   See [[malli.core/-instrument]] for possible options."
  ([] (unstrument! nil))
  ([options] (-strument! (assoc options :mode :unstrument))))
