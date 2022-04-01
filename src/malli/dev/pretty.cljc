(ns malli.dev.pretty
  (:require [malli.core :as m]
            [malli.dev.virhe :as v]
            [malli.error :as me]))

(defn -printer
  ([] (-printer nil))
  ([options]
   (v/-printer
    (merge {:title "Schema Error"
            :width 100
            :colors v/-dark-colors
            :unknown (fn [x] (when (m/schema? x) (m/form x)))
            :throwing-fn-top-level-ns-names ["malli" "clojure"]}
           options))))

(defn -errors [explanation printer]
  (->> (for [error (->> explanation (me/with-error-messages) :errors)]
         (v/-visit (into {} error) printer)) (interpose :break)))

(defn -explain [schema value printer] (-errors (m/explain schema value) printer))

(defn -block [text body printer]
  [:group (v/-text text printer) :break :break [:align 2 body]])

(defn -link [link printer]
  (v/-color :link link printer))

;;
;; formatters
;;

(defmethod v/-format ::m/explain [_ _ explanation printer]
  (let [{:keys [schema value]} explanation]
    {:body
     [:group
      (-block "Value:" (v/-visit value printer) printer) :break :break
      (-block "Errors:" (v/-visit (me/humanize explanation) printer) printer) :break :break
      (-block "Schema:" (v/-visit schema printer) printer) :break :break
      (-block "More information:" (-link "https://cljdoc.org/d/metosin/malli/CURRENT" printer) printer)]}))

(defmethod v/-format ::m/invalid-input [_ _ {:keys [args input fn-name]} printer]
  {:body
   [:group
    (-block "Invalid function arguments:" (v/-visit args printer) printer) :break :break
    #?(:cljs (-block "Function Var:" (v/-visit fn-name printer) printer)) :break :break
    (-block "Input Schema:" (v/-visit input printer) printer) :break :break
    (-block "Errors:" (-explain input args printer) printer) :break :break
    (-block "More information:" (-link "https://cljdoc.org/d/metosin/malli/CURRENT/doc/function-schemas" printer) printer)]})

(defmethod v/-format ::m/invalid-output [_ _ {:keys [value output fn-name] :as args} printer]
  {:body
   [:group
    (-block "Invalid function return value:" (v/-visit value printer) printer) :break :break
    #?(:cljs (-block "Function Var:" (v/-visit fn-name printer) printer)) :break :break
    (-block "Output Schema:" (v/-visit output printer) printer) :break :break
    (-block "Errors:" (-explain output value printer) printer) :break :break
    (-block "More information:" (-link "https://cljdoc.org/d/metosin/malli/CURRENT/doc/function-schemas" printer) printer)]})

(defmethod v/-format ::m/invalid-arity [_ _ {:keys [args arity schema fn-name]} printer]
  {:body
   [:group
    (-block (str "Invalid function arity (" arity "):") (v/-visit args printer) printer) :break :break
    (-block "Function Schema:" (v/-visit schema printer) printer) :break :break
    #?(:cljs (-block "Function Var:" (v/-visit fn-name printer) printer)) :break :break
    (-block "More information:" (-link "https://cljdoc.org/d/metosin/malli/CURRENT/doc/function-schemas" printer) printer)]})

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
