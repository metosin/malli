(ns malli.dev.pretty
  (:require [malli.core :as m]
            [malli.dev.virhe :as v]
            [malli.error :as me]
            [malli.registry :as mr]))

(defn -printer
  ([] (-printer nil))
  ([options]
   (v/-printer
    (merge {:title "Schema Error"
            :width 100
            :colors v/-dark-colors
            :unknown (fn [x] (when (m/schema? x) (m/form x)))
            :throwing-fn-top-level-ns-names ["malli" "clojure"]
            ::me/mask-valid-values '...}
           options))))

(defn -errors [explanation printer]
  (->> (for [error (->> explanation (me/with-error-messages) :errors)]
         (v/-visit (into {} error) printer)) (interpose :break)))

(defn -explain [schema value printer] (-errors (m/explain schema value) printer))

;;
;; formatters
;;

(defmethod v/-format ::m/explain [_ _ {:keys [schema] :as explanation} printer]
  {:body
   [:group
    (v/-block "Value:" (v/-visit (me/error-value explanation printer) printer) printer) :break :break
    (v/-block "Errors:" (v/-visit (me/humanize explanation) printer) printer) :break :break
    (v/-block "Schema:" (v/-visit schema printer) printer) :break :break
    (v/-block "More information:" (v/-link "https://cljdoc.org/d/metosin/malli/CURRENT" printer) printer)]})

(defmethod v/-format ::m/invalid-input [_ _ {:keys [args input fn-name]} printer]
  {:body
   [:group
    (v/-block "Invalid function arguments:" (v/-visit args printer) printer) :break :break
    (v/-block "Function Var:" (v/-visit fn-name printer) printer) :break :break
    (v/-block "Input Schema:" (v/-visit input printer) printer) :break :break
    (v/-block "Errors:" (-explain input args printer) printer) :break :break
    (v/-block "More information:" (v/-link "https://cljdoc.org/d/metosin/malli/CURRENT/doc/function-schemas" printer) printer)]})

(defmethod v/-format ::m/invalid-output [_ _ {:keys [value args output fn-name]} printer]
  {:body
   [:group
    (v/-block "Invalid function return value:" (v/-visit value printer) printer) :break :break
    (v/-block "Function Var:" (v/-visit fn-name printer) printer) :break :break
    (v/-block "Function arguments:" (v/-visit args printer) printer) :break :break
    (v/-block "Output Schema:" (v/-visit output printer) printer) :break :break
    (v/-block "Errors:" (-explain output value printer) printer) :break :break
    (v/-block "More information:" (v/-link "https://cljdoc.org/d/metosin/malli/CURRENT/doc/function-schemas" printer) printer)]})

(defmethod v/-format ::m/invalid-arity [_ _ {:keys [args arity schema fn-name]} printer]
  {:body
   [:group
    (v/-block (str "Invalid function arity (" arity "):") (v/-visit args printer) printer) :break :break
    (v/-block "Function Schema:" (v/-visit schema printer) printer) :break :break
    #?(:cljs (v/-block "Function Var:" (v/-visit fn-name printer) printer)) :break :break
    (v/-block "More information:" (v/-link "https://cljdoc.org/d/metosin/malli/CURRENT/doc/function-schemas" printer) printer)]})

(defmethod v/-format ::m/invalid-schema [_ _ {:keys [schema form]} printer]
  (let [proposals (seq (me/-most-similar-to #{schema} schema (set (keys (mr/schemas m/default-registry)))))]
    {:body
     [:group
      (v/-block "Invalid Schema" (v/-visit form printer) printer) :break :break
      (when proposals
        [:group (v/-block "Did you mean" (->> (for [proposal proposals] (v/-visit proposal printer)) (interpose :break)) printer)
         :break :break])
      (v/-block "More information:" (v/-link "https://cljdoc.org/d/metosin/malli/CURRENT" printer) printer)]}))

(defmethod v/-format ::m/child-error [_ _ {:keys [type children properties] :as data} printer]
  (let [form (m/-raw-form type properties children)
        constraints (reduce (fn [acc k] (if-let [v (get data k)] (assoc acc k v) acc)) nil [:min :max])
        size (count children)]
    {:body
     [:group
      (v/-block "Invalid Schema" (v/-visit form printer) printer) :break :break
      (v/-block "Reason" [:group "Schema has " (v/-visit size printer)
                          (if (= 1 size) " child" " children")
                          ", expected " (v/-visit constraints printer)] printer) :break :break
      (v/-block "More information:" (v/-link "https://cljdoc.org/d/metosin/malli/CURRENT" printer) printer)]}))

(defmethod v/-format ::m/invalid-entry [_ _ {:keys [entry naked-keys]} printer]
  {:body
   [:group
    (v/-block "Invalid Entry" (v/-visit (vec entry) printer) printer) :break :break
    (v/-block "More information:" (v/-link "https://cljdoc.org/d/metosin/malli/CURRENT" printer) printer)]})

;;
;; public api
;;

(defn reporter
  ([] (reporter (-printer)))
  ([printer]
   (fn [type data]
     (-> (ex-info (str type) {:type type :data data})
         (v/-exception-doc printer)
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
