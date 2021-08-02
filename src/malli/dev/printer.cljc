(ns malli.dev.printer
  (:require [virhe.core :as v]
            [malli.error :as me]
            [malli.core :as m]))

(defn -printer
  ([] (-printer nil))
  ([options]
   (v/printer
     (merge
       {:title "Schema Error"
        :width 100
        :unknown (fn [x] (cond-> x (m/schema? x) (m/form)))
        :throwing-fn-name "malli.core$_fail_BANG_"}
       options))))

(defn -errors [schema value printer]
  (->> (for [error (->> value (m/explain schema) (me/with-error-messages) :errors)]
         (v/-format (into {} error) printer))
       (interpose :break)))

(defn -block [text body]
  [:group (v/-text text) :break :break [:align 2 body]])

;;
;; formatters
;;

(defmethod v/-format ::m/invalid-input [_ _ {:keys [args input]} printer]
  {:body
   [:group
    (-block "Invalid function arguments:" (v/-visit args printer)) :break :break
    (-block "Input Schema:" (v/-visit input printer)) :break :break
    (-block "Errors:" (-errors input args printer)) :break :break
    (-block "More information:" (v/-color :white "https://cljdoc.org/d/metosin/malli/0.6.0-SNAPSHOT/doc/function-schemas"))]})

(defmethod v/-format ::m/invalid-output [_ _ {:keys [value output]} printer]
  {:body
   [:group
    (-block "Invalid function return value:" (v/-visit value printer)) :break :break
    (-block "Output Schema:" (v/-visit output printer)) :break :break
    (-block "Errors:" (-errors output value printer)) :break :break
    (-block "More information:" (v/-color :white "https://cljdoc.org/d/metosin/malli/0.6.0-SNAPSHOT/doc/function-schemas"))]})

(defmethod v/-format ::m/invalid-arity [_ _ {:keys [args arity schema]} printer]
  {:body
   [:group
    (-block (str "Invalid function arity (" arity "):") (v/-visit args printer)) :break :break
    (-block "Function Schema:" (v/-visit schema printer)) :break :break
    (-block "More information:" (v/-color :white "https://cljdoc.org/d/metosin/malli/0.6.0-SNAPSHOT/doc/function-schemas"))]})

;;
;; public api
;;

(defn report [type data]
  (let [printer (-printer {:width 120})]
    (-> (v/-event-doc type data printer)
        (v/-print-doc printer))))

(defn throw! [type data]
  (let [printer (-printer {:width 120})
        exception (ex-info (str type) {:type type :data data})
        message (-> (v/-exception-doc exception printer)
                    (v/-print-doc printer)
                    (with-out-str))]
    (throw (ex-info message (ex-data exception)))))

;;
;; spike
;;

(def kikka
  (m/-instrument
    {:schema [:function
              [:=> [:cat :int] :string]
              [:=> [:cat :int :int] :string]]
     :report throw!}
    (fn [x] (str x))))

(kikka "1" "2" "3")

#_(try
  (kikka "1")
  (catch Exception e
    (let [printer (-printer)]
      (-> (v/-exception-doc e printer)
          (v/-print-doc printer)))))
