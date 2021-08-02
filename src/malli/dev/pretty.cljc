(ns malli.dev.pretty
  (:require [malli.dev.virhe :as v]
            [malli.error :as me]
            [malli.core :as m]))

(defn -printer
  ([] (-printer nil))
  ([options]
   (v/printer
     (merge
       {:title "Schema Error"
        :width 100
        :colors v/-dark-colors
        :unknown (fn [x] (cond-> x (m/schema? x) (m/form)))
        :throwing-fn-name "malli.core$_fail_BANG_"}
       options))))

(defn -errors [schema value printer]
  (->> (for [error (->> value (m/explain schema) (me/with-error-messages) :errors)]
         (v/-visit (into {} error) printer))
       (interpose :break)))

(defn -block [text body printer]
  [:group (v/-text text printer) :break :break [:align 2 body]])

;;
;; formatters
;;

(defmethod v/-format ::m/invalid-input [_ _ {:keys [args input]} printer]
  {:body
   [:group
    (-block "Invalid function arguments:" (v/-visit args printer) printer) :break :break
    (-block "Input Schema:" (v/-visit input printer) printer) :break :break
    (-block "Errors:" (-errors input args printer) printer) :break :break
    (-block "More information:" (v/-color :link "https://cljdoc.org/d/metosin/malli/0.6.0-SNAPSHOT/doc/function-schemas" printer) printer)]})

(defmethod v/-format ::m/invalid-output [_ _ {:keys [value output]} printer]
  {:body
   [:group
    (-block "Invalid function return value:" (v/-visit value printer) printer) :break :break
    (-block "Output Schema:" (v/-visit output printer) printer) :break :break
    (-block "Errors:" (-errors output value printer) printer) :break :break
    (-block "More information:" (v/-color :link "https://cljdoc.org/d/metosin/malli/0.6.0-SNAPSHOT/doc/function-schemas" printer) printer)]})

(defmethod v/-format ::m/invalid-arity [_ _ {:keys [args arity schema]} printer]
  {:body
   [:group
    (-block (str "Invalid function arity (" arity "):") (v/-visit args printer) printer) :break :break
    (-block "Function Schema:" (v/-visit schema printer) printer) :break :break
    (-block "More information:" (v/-color :link "https://cljdoc.org/d/metosin/malli/0.6.0-SNAPSHOT/doc/function-schemas" printer) printer)]})

;;
;; public api
;;

(defn reporter
  ([] (reporter (-printer)))
  ([printer]
   (fn [type data]
     (-> (v/-event-doc type data printer)
         (v/-print-doc printer)))))

(defn thrower
  ([] (thrower (-printer)))
  ([printer]
   (fn [type data]
     (let [exception (ex-info (str type) {:type type :data data})
           message (-> (v/-exception-doc exception printer)
                       (v/-print-doc printer)
                       (with-out-str))]
       (throw (ex-info message (ex-data exception)))))))

;;
;; spike
;;

(def kikka
  (m/-instrument
    {:schema [:function
              [:=> [:cat :int] :string]
              [:=> [:cat :int :int] :string]]
     :report (thrower)}
    (fn [x] x)))

(kikka "1" "3" "3")

#_(try
    (kikka "1")
    (catch Exception e
      (let [printer (-printer)]
        (-> (v/-exception-doc e printer)
            (v/-print-doc printer)))))
