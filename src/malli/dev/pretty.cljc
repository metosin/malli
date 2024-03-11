(ns malli.dev.pretty
  (:require [malli.core :as m]
            [malli.dev.virhe :as v]
            [malli.error :as me]
            [malli.edn :as edn]
            [malli.registry :as mr]))

(defn -printer
  ([] (-printer nil))
  ([options]
   (v/-printer
    (merge {:title "Schema Error"
            :width 80
            :colors v/-dark-colors
            :unknown (fn [x] (when (m/schema? x) (m/form x)))
            :throwing-fn-top-level-ns-names ["malli" "clojure" "malli" "nrepl"]
            ::me/mask-valid-values '...}
           options))))

(defn -errors [explanation printer]
  (->> (for [error (->> explanation (me/with-error-messages) :errors)]
         (v/-visit (into {} error) printer)) (interpose :break)))

(defn -explain [schema value printer] (-errors (m/explain schema value) printer))

(defn -log! [text printer]
  (-> [:group (v/-color :title "malli: " printer) text]
      (v/-print-doc printer)))

(defn -ref-text [printer]
  [:group "Reference should be one of the following" :break :break
   "- a qualified keyword, " (v/-visit [:ref :user/id] printer) :break
   "- a qualified symbol,  " (v/-visit [:ref (symbol "'user" "id")] printer) :break
   "- a string,            " (v/-visit [:ref "user/id"] printer) :break
   "- a Var,               " (v/-visit [:ref (symbol "#'user" "id")] printer)])

;;
;; formatters
;;

(defmethod v/-format ::m/explain [_ {:keys [schema] :as explanation} printer]
  {:body [:group
          (v/-block "Value" (v/-visit (me/error-value explanation printer) printer) printer) :break :break
          (v/-block "Errors" (v/-visit (me/humanize (me/with-spell-checking explanation)) printer) printer) :break :break
          (v/-block "Schema" (v/-visit schema printer) printer) :break :break
          (v/-block "More information" (v/-link "https://cljdoc.org/d/metosin/malli/CURRENT" printer) printer)]})

(defmethod v/-format ::m/coercion [_ {:keys [explain]} printer]
  (v/format (m/-exception ::m/explain explain) printer))

(defmethod v/-format ::m/invalid-input [_ {:keys [args input fn-name]} printer]
  {:title "Invalid Function Input"
   :body [:group
          (v/-block "Invalid function arguments" (v/-visit args printer) printer) :break :break
          (when fn-name [:span (v/-block "Function Var" (v/-visit fn-name printer) printer) :break :break])
          (v/-block "Input Schema" (v/-visit input printer) printer) :break :break
          (v/-block "Errors" (-explain input args printer) printer) :break :break
          (v/-block "More information" (v/-link "https://cljdoc.org/d/metosin/malli/CURRENT/doc/function-schemas" printer) printer)]})

(defmethod v/-format ::m/invalid-output [_ {:keys [value args output fn-name]} printer]
  {:title "Invalid Function Output"
   :body [:group
          (v/-block "Invalid function return value" (v/-visit value printer) printer) :break :break
          (when fn-name [:span (v/-block "Function Var" (v/-visit fn-name printer) printer) :break :break])
          (v/-block "Function arguments" (v/-visit args printer) printer) :break :break
          (v/-block "Output Schema" (v/-visit output printer) printer) :break :break
          (v/-block "Errors" (-explain output value printer) printer) :break :break
          (v/-block "More information" (v/-link "https://cljdoc.org/d/metosin/malli/CURRENT/doc/function-schemas" printer) printer)]})

(defmethod v/-format ::m/invalid-guard [_ {:keys [value args guard fn-name]} printer]
  {:title "Function Guard Error"
   :body [:group
          (when fn-name [:span (v/-block "Function Var" (v/-visit fn-name printer) printer) :break :break])
          (v/-block "Guard arguments" (v/-visit [args value] printer) printer) :break :break
          (v/-block "Guard Schema" (v/-visit guard printer) printer) :break :break
          (v/-block "Errors" (-explain guard [args value] printer) printer) :break :break
          (v/-block "More information" (v/-link "https://cljdoc.org/d/metosin/malli/CURRENT/doc/function-schemas" printer) printer)]})

(defmethod v/-format ::m/invalid-arity [_ {:keys [args arity schema fn-name]} printer]
  {:body [:group
          (v/-block (str "Invalid function arity (" arity ")") (v/-visit args printer) printer) :break :break
          (v/-block "Function Schema" (v/-visit schema printer) printer) :break :break
          #?(:cljs (v/-block "Function Var" (v/-visit fn-name printer) printer)) :break :break
          (v/-block "More information" (v/-link "https://cljdoc.org/d/metosin/malli/CURRENT/doc/function-schemas" printer) printer)]})

(defmethod v/-format ::m/register-function-schema [_ {:keys [ns name schema _data key _exception]} printer]
  {:title "Error in registering a Function Schema"
   :body [:group
          (v/-block "Function Var" [:group
                                    (v/-visit (symbol (str ns) (str name)) printer)
                                    " (" (v/-visit key printer) ")"] printer) :break :break
          (v/-block "Function Schema" (v/-visit schema printer) printer) :break :break
          (v/-block "More information" (v/-link "https://cljdoc.org/d/metosin/malli/CURRENT/doc/function-schemas" printer) printer)]})

(defmethod v/-format ::m/invalid-ref [_ {:keys [ref]} printer]
  {:body [:group
          (v/-block "Invalid Reference" (v/-visit [:ref ref] printer) printer) :break :break
          (v/-block "Reason" (-ref-text printer) printer) :break :break
          (v/-block "More information" (v/-link "https://cljdoc.org/d/metosin/malli/CURRENT" printer) printer)]})

(defmethod v/-format ::m/invalid-schema [_ {:keys [schema form]} printer]
  (let [proposals (seq (me/-most-similar-to #{schema} schema (set (keys (mr/schemas m/default-registry)))))]
    {:title "Schema Creation Error"
     :body [:group
            (v/-block "Invalid Schema" (v/-visit form printer) printer) :break :break
            (when proposals
              [:group (v/-block "Did you mean" (->> (for [proposal proposals] (v/-visit proposal printer)) (interpose :break)) printer)
               :break :break])
            (v/-block "More information" (v/-link "https://cljdoc.org/d/metosin/malli/CURRENT" printer) printer)]}))

(defmethod v/-format ::m/child-error [_ {:keys [type children properties] :as data} printer]
  (let [form (m/-raw-form type properties children)
        constraints (reduce (fn [acc k] (if-let [v (get data k)] (assoc acc k v) acc)) nil [:min :max])
        size (count children)]
    {:title "Schema Creation Error"
     :body [:group
            (v/-block "Invalid Schema" (v/-visit form printer) printer) :break :break
            (v/-block "Reason" [:group "Schema has " (v/-visit size printer)
                                (if (= 1 size) " child" " children")
                                ", expected " (v/-visit constraints printer)] printer) :break :break
            (v/-block "More information" (v/-link "https://cljdoc.org/d/metosin/malli/CURRENT" printer) printer)]}))

(defmethod v/-format ::m/invalid-entry [_ {:keys [entry]} printer]
  (let [wrap (if (sequential? entry) vec vector)
        wrapped (wrap entry)
        example (cond-> wrapped (= 1 (count wrapped)) (conj :any))]
    {:title "Schema Creation Error"
     :body [:group
            (v/-block "Invalid Entry" (v/-visit entry printer) printer) :break :break
            (v/-block "Did you mean" (v/-visit example printer) printer) :break :break
            (v/-block "More information" (v/-link "https://cljdoc.org/d/metosin/malli/CURRENT" printer) printer)]}))

(defmethod v/-format ::m/duplicate-keys [_ {:keys [arr]} printer]
  (let [keys (->> arr (vec) (take-nth 2))]
    {:title "Schema Creation Error"
     :body [:group
            (v/-block "Duplicate Keys" (v/-visit keys printer) printer) :break :break
            (v/-block "More information" (v/-link "https://cljdoc.org/d/metosin/malli/CURRENT" printer) printer)]}))

(defmethod v/-format :malli.edn/var-parsing-not-supported [_ {:keys [string var]} printer]
  (let [parse (fn [string]
                (try (edn/-parse-string string {:regex true, :fn true, :var edn/-var-symbol})
                     (catch #?(:clj Exception, :cljs js/Error) _ string)))]
    {:title "Deserialization Error"
     :body [:group
            (v/-block "Var" (v/-visit var printer) printer) :break :break
            (v/-block "Data" (v/-visit (parse string) printer) printer) :break :break
            (v/-block "Error" [:group
                               "Var deserialization is disabled by default, because:" :break :break
                               "- Vars don't work at runtime in ClojureScript" :break
                               "- Var resolutions has overhead with GraalVM Native Image"] printer) :break :break
            (v/-block "Resolution" [:group
                                    "To deserialize Var with Clojure:" :break :break
                                    (v/-visit `(malli.edn/read-string
                                                ~string
                                                {:malli.edn/edamame-options {:regex true, :fn true, :var resolve}})
                                              printer)] printer) :break :break
            (v/-block "More information" (v/-link "https://cljdoc.org/d/metosin/malli/CURRENT" printer) printer)]}))

;;
;; public api
;;

(defn reporter
  ([] (reporter (-printer)))
  ([printer]
   (fn [type data]
     (-> (ex-info (str type) {:type type :data data})
         (v/exception-document printer)
         (v/-print-doc printer)
         #?(:cljs (-> with-out-str println))))))

(defn thrower
  ([] (thrower (-printer)))
  ([printer]
   (let [report (reporter printer)]
     (fn [type data]
       (let [message (with-out-str (report type data))]
         (throw (ex-info message {:type type :data data})))))))

(defn prettifier [type title f options]
  (let [printer (assoc (or (::printer options) (assoc (-printer) :width 60)) :title title)
        actor (::actor options reporter)]
    (fn [& args] (when-let [res (apply f args)] ((actor printer) type res) res))))

(defn explain
  ([?schema value] (explain ?schema value nil))
  ([?schema value options]
   (let [explain (fn [] (->> (m/explain ?schema value options) (me/with-error-messages)))]
     ((prettifier ::m/explain "Validation Error" explain options)))))
