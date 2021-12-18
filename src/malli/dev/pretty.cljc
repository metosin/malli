(ns malli.dev.pretty
  (:require [malli.dev.virhe :as v]
            [malli.error :as me]
            [malli.core :as m]))

(defn -printer
  ([] (-printer nil))
  ([options]
   (v/-printer
     (merge
       {:title "Schema Error"
        :width 100
        :colors v/-dark-colors
        :unknown (fn [x] (when (m/schema? x) (m/form x)))
        :throwing-fn-top-level-ns-names ["malli" "clojure"]}
       options))))

(defn -errors [schema value printer]
  (->> (for [error (->> value (m/explain schema) (me/with-error-messages) :errors)]
         (v/-visit (into {} error) printer))
       (interpose :break)))

(defn -block [text body printer]
  [:group (v/-text text printer) :break :break [:align 2 body]])

(defn -link [link printer]
  (v/-color :link link printer))

;;
;; formatters
;;

(defmethod v/-format ::m/invalid-input [_ _ {:keys [args input]} printer]
  {:body
   [:group
    (-block "Invalid function arguments:" (v/-visit args printer) printer) :break :break
    (-block "Input Schema:" (v/-visit input printer) printer) :break :break
    (-block "Errors:" (-errors input args printer) printer) :break :break
    (-block "More information:" (-link "https://cljdoc.org/d/metosin/malli/CURRENT/doc/function-schemas" printer) printer)]})

(defmethod v/-format ::m/invalid-output [_ _ {:keys [value output]} printer]
  {:body
   [:group
    (-block "Invalid function return value:" (v/-visit value printer) printer) :break :break
    (-block "Output Schema:" (v/-visit output printer) printer) :break :break
    (-block "Errors:" (-errors output value printer) printer) :break :break
    (-block "More information:" (-link "https://cljdoc.org/d/metosin/malli/CURRENT/doc/function-schemas" printer) printer)]})

(defmethod v/-format ::m/invalid-arity [_ _ {:keys [args arity schema]} printer]
  {:body
   [:group
    (-block (str "Invalid function arity (" arity "):") (v/-visit args printer) printer) :break :break
    (-block "Function Schema:" (v/-visit schema printer) printer) :break :break
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
         (v/-print-doc printer)))))

(defn thrower
  ([] (thrower (-printer)))
  ([printer]
   (let [report (reporter printer)]
     (fn [type data]
       (let [message (with-out-str (report type data))]
         (throw (ex-info message {:type type :data data})))))))
