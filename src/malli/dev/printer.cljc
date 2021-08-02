(ns malli.dev.printer
  (:require [virhe.pretty :as v]
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
         (v/-indent 2 [:group (v/-format (into {} error) printer)]))
       (v/-line-breaks)))

(defmethod v/-format-event ::m/invalid-input [_ _ {:keys [args input]} printer]
  {:body
   [:group
    (v/-text "Invalid function arguments:")
    [:break] [:break]
    (v/-indent 2 (v/-format args printer))
    [:break] [:break]
    (v/-text "Input Schema:")
    [:break] [:break]
    (v/-indent 2 (v/-format input printer))
    [:break] [:break]
    (v/-text "Errors:")
    [:break] [:break]
    (-errors input args printer)
    [:break] [:break]
    (v/-color :white "https://cljdoc.org/d/metosin/malli/0.6.0-SNAPSHOT/doc/function-schemas")]})

(defmethod v/-format-event ::m/invalid-output [_ _ {:keys [value output]} printer]
  {:body
   [:group
    (v/-text "Invalid function return value:")
    [:break] [:break]
    (v/-indent 2 (v/-format value printer))
    [:break] [:break]
    (v/-text "Output Schema:")
    [:break] [:break]
    (v/-indent 2 (v/-format output printer))
    [:break] [:break]
    (v/-text "Errors:")
    [:break] [:break]
    (-errors output value printer)
    [:break] [:break]
    (v/-color :white "https://cljdoc.org/d/metosin/malli/0.6.0-SNAPSHOT/doc/function-schemas")]})

;;
;; public api
;;

(defn report [type data]
  (let [printer (-printer {:width 120})]
    (-> (v/-event-section type data printer)
        (v/-print-doc printer))))

(defn throw! [type data]
  (let [printer (-printer {:width 120})
        message (-> (v/-event-section type data printer)
                    (v/-print-doc printer)
                    (with-out-str))]
    (throw (ex-info message {:type type, :data data}))))

;;
;; spike
;;

(def kikka
  (m/-instrument
    {:schema [:=> [:cat :int] :string]
     :report throw!}
    (fn [x] (str x))))

(kikka "1")

(try
  (kikka "1")
  (catch Exception e
    (let [printer (-printer)]
      (-> (v/-exception e printer)
          (v/-print-doc printer)))))
