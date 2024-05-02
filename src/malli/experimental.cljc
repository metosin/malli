(ns malli.experimental
  (:refer-clojure :exclude [defn])
  #?(:cljs (:require-macros malli.experimental))
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
                "Binder" [:vector [:or
                                   simple-symbol?
                                   [:tuple simple-symbol? "Schema"]]]
                "Params" [:catn
                          [:poly [:? [:catn
                                      [:all [:enum :all :for-all]]
                                      [:binder "Binder"]]]]
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
  (let [{:keys [name poly return doc arities] body-meta :meta :as parsed} (m/parse schema args)
        var-meta (meta name)
        _ (when (= ::m/invalid parsed) (m/-fail! ::parse-error {:schema schema, :args args}))
        parse (fn [{:keys [args] :as parsed}] (merge (md/parse args) parsed))
        ->schema (fn [{:keys [schema]}] [:=> schema (:schema return :any)])
        single (= :single (key arities))
        parglists (if single (->> arities val parse vector) (->> arities val :arities (map parse)))
        raw-arglists (map :raw-arglist parglists)
        schema (as-> (map ->schema parglists) $ (if single (first $) (into [:function] $)))
        schema (if poly
                 `(m/all ~(:binder poly) ~schema)
                 schema)
        bodies (map (fn [{:keys [arglist prepost body]}] `(~arglist ~prepost ~@body)) parglists)
        validate? (or (:malli/always var-meta) (:malli/always body-meta))
        goptions (gensym 'options)
        options (::m/options body-meta)
        gschema (gensym 'schema)
        enriched-meta (assoc body-meta :raw-arglists (list 'quote raw-arglists) :schema gschema ::m/options goptions)
        let-around-def (if poly
                         `[~(m/-all-binder-names (:binder poly)) (m/-all-binder-defaults (second ~gschema))]
                         [])]
    (when (some #{name} let-around-def)
      (throw (ex-info ":all binder must not bind the same name as the var being defined" {})))
    `(let [~gschema ~schema
           ~goptions ~options
           defn# (let ~let-around-def
                   ~(if validate?
                      `(def
                         ~(with-meta name (merge var-meta
                                                 enriched-meta
                                                 {:arglists (list 'quote (map :arglist parglists))}))
                         ~@(some-> doc vector)
                         (m/-instrument {:schema ~gschema} (fn ~(gensym (str name "-instrumented")) ~@bodies) ~goptions))
                      `(c/defn
                         ~name
                         ~@(some-> doc vector)
                         ~enriched-meta
                         ~@bodies
                         ~@(when-not single (some->> arities val :meta vector)))))]
       (m/=> ~name ~schema ~goptions)
       defn#)))

;;
;; public api
;;

#?(:clj (defmacro defn [& args] (-defn SchematizedParams args)))
