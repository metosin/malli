(ns malli.experimental
  (:refer-clojure :exclude [defn])
  #?(:cljs (:require-macros malli.experimental))
  (:require [clojure.core :as c]
            [malli.core :as m]
            [malli.destructure :as md]
            [malli.dev.pretty :as pretty]))

(c/defn -defn-schema [inline-schemas]
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

(c/defn -def-schema [inline-schemas]
  (m/schema
   [:schema
    {:registry {"Schema" any?
                "Separator" (if inline-schemas [:= :-] md/Never)
                "Params" [:catn
                          [:name symbol?]
                          [:return [:? [:catn
                                        [:- "Separator"]
                                        [:schema "Schema"]]]]
                          [:doc [:? string?]]
                          [:body any?]]}}
    "Params"]))

(def SchematizedDefnParams (-defn-schema true))
(def DefnParams (-defn-schema false))

(def ^:depecated SchematizedParams SchematizedDefnParams)
(def ^:depecated Params DefnParams)

(def SchematizedDefParams (-def-schema true))
(def DefParams (-def-schema false))

(c/defn -defn [schema args]
  (let [{:keys [name return doc arities] body-meta :meta :as parsed} (m/parse schema args)
        var-meta (meta name)
        _ (when (= ::m/invalid parsed) (m/-fail! ::parse-error {:schema schema, :args args}))
        parse (fn [{:keys [args] :as parsed}] (merge (md/parse args) parsed))
        ->schema (fn [{:keys [schema]}] [:=> schema (:schema return :any)])
        single (= :single (key arities))
        parglists (if single (->> arities val parse vector) (->> arities val :arities (map parse)))
        raw-arglists (map :raw-arglist parglists)
        schema (as-> (map ->schema parglists) $ (if single (first $) (into [:function] $)))
        bodies (map (fn [{:keys [arglist prepost body]}] `(~arglist ~prepost ~@body)) parglists)
        validate? (or (:malli/always var-meta) (:malli/always body-meta))
        enriched-meta (assoc body-meta :raw-arglists (list 'quote raw-arglists) :schema schema)]
    `(let [defn# ~(if validate?
                    `(def
                       ~(with-meta name (merge var-meta
                                               enriched-meta
                                               {:arglists (list 'quote (map :arglist parglists))}))
                       ~@(some-> doc vector)
                       (m/-instrument {:schema ~schema} (fn ~(gensym (str name "-instrumented")) ~@bodies)))
                    `(c/defn
                       ~name
                       ~@(some-> doc vector)
                       ~enriched-meta
                       ~@bodies
                       ~@(when-not single (some->> arities val :meta vector))))]
       (m/=> ~name ~schema)
       defn#)))

(c/defn -def [schema args]
  (let [{:keys [name doc body] {:keys [schema]} :return :as parsed} (m/parse schema args)]
    (when (= ::m/invalid parsed) (m/-fail! ::parse-error {:schema schema, :args args}))
    `(let [def# (def
                  ~(with-meta name {:schema schema})
                  ~@(some-> doc vector))]
       (when (and ~schema (not (m/validate ~schema ~body)))
         (pretty/explain ~schema ~body)
         (m/-fail! ::invalid-data {:def ~name, :schema ~schema, :body ~body}))
       def#)))

;;
;; public api
;;

#?(:clj (defmacro defn [& args] (-defn SchematizedDefnParams args)))
#?(:clj (defmacro def [& args] (-def SchematizedDefParams args)))
