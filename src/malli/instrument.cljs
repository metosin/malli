(ns malli.instrument
  (:require-macros [malli.instrument])
  (:require [clojure.string :as str]
            [goog.object :as g]
            [malli.core :as m]
            [malli.generator :as mg]))

(defn ^:private -ns-js-path [ns] (into-array (map munge (str/split (str ns) #"\."))))
(defn ^:private -prop-js-path [ns prop] (into-array (map munge (conj (str/split (str ns) #"\.") (name prop)))))
(defn ^:private -get-prop [ns prop] (g/getValueByKeys goog/global (-prop-js-path ns prop)))
(defn ^:private -get-ns [ns] (g/getValueByKeys goog/global (-ns-js-path ns)))
(defn ^:private -find-var [n s] (-get-prop n s))
(defn ^:private -original [f] (g/get f "malli$instrument$original"))
(defn ^:private -instrumented? [f] (true? (g/get f "malli$instrument$instrumented?")))

(defn ^:private meta-fn
  ;; Taken from https://clojure.atlassian.net/browse/CLJS-3018
  ;; Because the current MetaFn implementation can cause quirky errors in CLJS
  [f m]
  (let [new-f (goog/bind f #js{})]
    (js/Object.assign new-f f)
    (specify! new-f IMeta #_:clj-kondo/ignore (-meta [_] m))
    new-f))

(defn -filter-ns [& ns] (fn [n _ _] ((set ns) n)))
(defn -filter-var [f] (fn [n s d] (f (Var. (constantly (-find-var n s)) (symbol n s) d))))
(defn -filter-schema [f] (fn [_ _ {:keys [schema]}] (f schema)))

(defn -arity->schema
  [fn-schema]
  (into {} (map (fn [schema] [(:arity (m/-function-info (m/schema schema))) schema])
                (rest fn-schema))))

(defn -variadic? [f] (g/get f "cljs$core$IFn$_invoke$arity$variadic"))
(defn -max-fixed-arity [f] (g/get f "cljs$lang$maxFixedArity"))
(defn -pure-variadic? [f]
  (let [max-fixed-arity (-max-fixed-arity f)]
    (and max-fixed-arity (-variadic? f)
         (every? #(not (fn? (g/get f (str "cljs$core$IFn$_invoke$arity$" %)))) (range 20)))))

(defn -replace-variadic-fn [original-fn n s opts]
  (let [accessor "cljs$core$IFn$_invoke$arity$variadic"
        arity-fn (g/get original-fn accessor)]
    (when arity-fn
      (g/set original-fn "malli$instrument$instrumented?" true)
      ;; the shape of the argument in the following apply calls are needed to match the call style of the cljs compiler
      ;; so the user's function gets the arguments as expected
      (let [max-fixed-arity (-max-fixed-arity original-fn)
            instrumented-variadic-fn (m/-instrument opts (fn [& args]
                                                           (let [[fixed-args rest-args] (split-at max-fixed-arity (vec args))
                                                                 final-args (into (vec fixed-args) [(not-empty rest-args)])]
                                                             (apply arity-fn final-args))))
            instrumented-wrapper (fn [& args]
                                   (let [[fixed-args rest-args] (split-at max-fixed-arity (vec args))
                                         final-args (vec (apply list* (into (vec fixed-args) (not-empty rest-args))))]
                                     (apply instrumented-variadic-fn final-args)))]
        (g/set instrumented-wrapper "malli$instrument$original" arity-fn)
        (g/set (-get-prop n s) "malli$instrument$instrumented?" true)
        (g/set (-get-prop n s) accessor instrumented-wrapper)
        (g/set (-get-ns n) s (meta-fn original-fn {:instrumented-symbol (symbol n s)}))))))

(defn -replace-multi-arity [original-fn n s opts]
  (let [schema (:schema opts)]
    (g/set original-fn "malli$instrument$instrumented?" true)
    (g/set (-get-ns n) s (meta-fn original-fn {:instrumented-symbol (symbol n s)}))
    (doseq [[arity f-schema] (-arity->schema schema)]
      (if (= arity :varargs)
        (-replace-variadic-fn original-fn n s opts)
        (let [accessor (str "cljs$core$IFn$_invoke$arity$" arity)
              arity-fn (g/get original-fn accessor)]
          (when arity-fn
            (let [instrumented-fn (m/-instrument (assoc opts :schema f-schema) arity-fn)]
              (g/set instrumented-fn "malli$instrument$original" arity-fn)
              (g/set instrumented-fn "malli$instrument$instrumented?" true)
              (g/set (-get-prop n s) accessor instrumented-fn))))))))

(defn -replace-fn [original-fn n s opts]
  (try
    (cond
      (-pure-variadic? original-fn) (-replace-variadic-fn original-fn n s opts)
      (-max-fixed-arity original-fn) (-replace-multi-arity original-fn n s opts)
      :else (let [instrumented-fn (meta-fn (m/-instrument opts original-fn) {:instrumented-symbol (symbol (name n) (name s))})]
              (g/set original-fn "malli$instrument$instrumented?" true)
              (g/set instrumented-fn "malli$instrument$instrumented?" true)
              (g/set instrumented-fn "malli$instrument$original" original-fn)
              (g/set (-get-ns n) (munge (name s)) instrumented-fn)))
    (catch :default e
      (if (instance? ExceptionInfo e)
        (throw
         (ex-info
          (str "Schema error when instrumenting function: " (symbol (name n) (name s)) " - " (ex-message e))
          (ex-data e)))
        (throw (js/Error. (str "Schema error when instrumenting function: " (symbol (name n) (name s)) ". " e)))))))

(defn -strument!
  ([] (-strument! nil))
  ([{:keys [mode data filters gen report skip-instrumented?] :or {skip-instrumented? false
                                                                  mode :instrument, data (m/function-schemas :cljs)} :as options}]
   (doseq [[n d] data, [s d] d]
     (when-let [v (-find-var n s)]
       (when (or (not filters) (some #(% n s d) filters))
         (case mode
           :instrument (let [original-fn (or (-original v) v)
                             dgen (as-> (select-keys options [:scope :report :gen]) $
                                        (cond-> $ report (update :report (fn [r] (fn [t data] (r t (assoc data :fn-name (symbol (name n) (name s))))))))
                                        (merge $ d)
                                        (cond (and gen (true? (:gen d))) (assoc $ :gen gen)
                                              (true? (:gen d)) (dissoc $ :gen)
                                              :else $))]
                         (if (and original-fn (not (and skip-instrumented? (-instrumented? v))))
                           (-replace-fn original-fn n s dgen)))

           :unstrument (when (-instrumented? v)
                         (let [original-fn (or (-original v) v)]
                           (cond
                             (-pure-variadic? original-fn)
                             (let [accessor "cljs$core$IFn$_invoke$arity$variadic"
                                   variadic-fn (g/get v accessor)
                                   orig-variadic-fn (g/get variadic-fn "malli$instrument$original")]
                               (g/set original-fn accessor orig-variadic-fn))

                             (-max-fixed-arity original-fn)
                             (doseq [arity (conj (range 20) "variadic")
                                     :let [accessor (str "cljs$core$IFn$_invoke$arity$" arity)
                                           arity-fn (g/get original-fn accessor)]
                                     :when arity-fn]
                               (let [orig (g/get arity-fn "malli$instrument$original")]
                                 (g/set original-fn accessor orig)))

                             :else (g/set (-get-ns n) (munge (name s)) original-fn))))
           (mode v d)))))))

;;
;; public api
;;

(defn check
  "Checks all registered function schemas using generative testing.
   Returns nil or a map of symbol -> explanation in case of errors."
  ([] (check nil))
  ([options]
   (let [res* (atom {})]
     (-strument! (assoc options :mode (fn [v {:keys [schema ns name]}]
                                        (some->> (mg/check schema (-original v))
                                                 (swap! res* assoc (symbol ns name))))))
     (not-empty @res*))))

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
