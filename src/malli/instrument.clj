(ns malli.instrument
  (:require [malli.core :as m]))

(defn -find-var [n s] (find-var (symbol (str n "/" s))))
(defn -sequential [x] (cond (set? x) x (sequential? x) x :else [x]))
(defn -original [v] (::original-fn (meta v)))

(defn -filter-ns [& ns] (fn [n _ _] ((set ns) n)))
(defn -filter-var [f] (fn [n s _] (f (-find-var n s))))
(defn -filter-schema [f] (fn [_ _ {:keys [schema]}] (f schema)))

(defn -strument!
  ([] (-strument! nil))
  ([{:keys [mode data filters gen] :or {mode :instrument, data (m/function-schemas)} :as options}]
   (doseq [[n d] data, [s d] d]
     (if (or (not filters) (some #(% n s d) filters))
       (if-let [v (-find-var n s)]
         (case mode
           :instrument (let [original-fn (or (-original v) (deref v))
                             dgen (as-> (merge (select-keys options [:scope :report :gen]) d) $
                                        (cond (and gen (true? (:gen d))) (assoc $ :gen gen)
                                              (true? (:gen d)) (dissoc $ :gen)
                                              :else $))]
                         (alter-meta! v assoc ::original-fn original-fn)
                         (alter-var-root v (constantly (m/-instrument dgen original-fn)))
                         (println "..instrumented" v))
           :unstrument (when-let [original-fn (-original v)]
                         (alter-meta! v dissoc ::original-fn)
                         (alter-var-root v (constantly original-fn))
                         (println "..unstrumented" v))
           (mode v d)))))))

(defn -collect! [v]
  (let [{:keys [ns name malli/schema] :as meta} (meta v)]
    (when schema (m/-register-function-schema! (-> ns str symbol) name schema (m/-unlift-keys meta "malli")) v)))

;;
;; public api
;;

(defn collect!
  "Reads all public Vars from a given namespace(s) and registers a function (var) schema if `:malli/schema`
   metadata is present. The following metadata key can be used:

   | key             | description |
   | ----------------|-------------|
   | `:malli/schema` | function schema
   | `:malli/scope`  | optional set of scope definitions, defaults to `#{:input :output}`
   | `:malli/report` | optional side-effecting function of `key data -> any` to report problems, defaults to `m/-fail!`
   | `:malli/gen`    | optional value `true` or function of `schema -> schema -> value` to be invoked on the args to get the return value"
  ([] (collect! {:ns *ns*}))
  ([{:keys [ns]}]
   (not-empty (reduce (fn [acc v] (let [v (-collect! v)] (cond-> acc v (conj v)))) #{} (vals (mapcat ns-publics (-sequential ns)))))))

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
