(ns malli.experimental
  (:refer-clojure :exclude [defn])
  (:require [clojure.core :as c]
            [malli.core :as m]
            [malli.destructure :as md]))

(c/defn -schema [inline-schemas]
  (m/schema
   [:schema
    {:registry {"Schema" any?
                "Separator" (if inline-schemas [:= :-] md/Never)
                "Args" [:vector :any]
                "PrePost" [:map
                           [:pre {:optional true} [:sequential any?]]
                           [:post {:optional true} [:sequential any?]]]
                "Arity" [:catn
                         [:args "Args"]
                         [:prepost [:? "PrePost"]]
                         [:body [:* :any]]]
                "Params" [:catn
                          [:name symbol?]
                          [:return [:? [:catn
                                        [:- "Separator"]
                                        [:schema "Schema"]]]]
                          [:doc [:? string?]]
                          [:meta [:? :map]]
                          [:arities [:altn
                                     [:single "Arity"]
                                     [:multiple [:catn
                                                 [:arities [:+ [:schema "Arity"]]]
                                                 [:meta [:? :map]]]]]]]}}
    "Params"]))

(def SchematizedParams (-schema true))
(def Params (-schema false))

(c/defn -defn [schema args]
  (let [{:keys [name return doc meta arities] :as parsed} (m/parse schema args)
        _ (when (= ::m/invalid parsed) (m/-fail! ::parse-error {:schema schema, :args args}))
        parse (fn [{:keys [args] :as parsed}] (merge (md/parse args) parsed))
        ->schema (fn [{:keys [schema]}] [:=> schema (:schema return :any)])
        single (= :single (key arities))
        parglists (if single (->> arities val parse vector) (->> arities val :arities (map parse)))
        raw-arglists (map :raw-arglist parglists)
        schema (as-> (map ->schema parglists) $ (if single (first $) (into [:function] $)))]
    `(c/defn
       ~name
       ~@(some-> doc vector)
       ~(assoc meta :malli/schema schema, :raw-arglist (list 'quote raw-arglists))
       ~@(map (fn [{:keys [arglist prepost body]}] `(~arglist ~prepost ~@body)) parglists)
       ~@(when-not single (some->> arities val :meta vector)))))

;;
;; public api
;;

(defmacro defn [& args] (-defn SchematizedParams args))
