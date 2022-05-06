(ns malli.experimental
  #?@(:cljs
      [(:require-macros malli.experimental)
       (:require
         [malli.core :as m]
         [malli.dev.pretty])])
  #?@(:clj
      [(:refer-clojure :exclude [defn])
       (:require
         [clojure.core :as c]
         [malli.core :as m]
         [malli.dev.pretty]
         [malli.destructure :as md])]))

#?(:clj
   (c/defn -schema [inline-schemas]
     (m/schema
       [:schema
        {:registry {"Schema"    any?
                    "Separator" (if inline-schemas [:= :-] md/Never)
                    "Args"      [:vector :any]
                    "PrePost"   [:map
                                 [:pre {:optional true} [:sequential any?]]
                                 [:post {:optional true} [:sequential any?]]]
                    "Arity"     [:catn
                                 [:args "Args"]
                                 [:prepost [:? "PrePost"]]
                                 [:body [:* :any]]]
                    "Params"    [:catn
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
        "Params"]
       {:registry (m/default-schemas)})))

#?(:clj (def SchematizedParams (-schema true)))
#?(:clj (def Params (-schema false)))

#?(:clj
   (c/defn -defn [schema args]
     (let [{:keys [name return doc meta arities] :as parsed} (m/parse schema args)
           _            (when (= ::m/invalid parsed) (m/-fail! ::parse-error {:schema schema, :args args}))
           parse        (fn [{:keys [args] :as parsed}] (merge (md/parse args) parsed))
           ->schema     (fn [{:keys [schema]}] [:=> schema (:schema return :any)])
           single       (= :single (key arities))
           parglists    (if single (->> arities val parse vector) (->> arities val :arities (map parse)))
           raw-arglists (map :raw-arglist parglists)
           schema       (as-> (map ->schema parglists) $ (if single (first $) (into [:function] $)))]
       `(let [defn# (c/defn
                      ~name
                      ~@(some-> doc vector)
                      ~(assoc meta :raw-arglists (list 'quote raw-arglists), :schema schema)
                      ~@(map (fn [{:keys [arglist prepost body]}] `(~arglist ~prepost ~@body)) parglists)
                      ~@(when-not single (some->> arities val :meta vector)))]
          (m/=> ~name ~schema)
          defn#))))

#?(:clj
   (c/defn ->defn [schema report args]
     (let [{:keys [name return doc meta arities] :as parsed} (m/parse schema args)
           _            (when (= ::m/invalid parsed) (m/-fail! ::parse-error {:schema schema, :args args}))
           parse        (fn [{:keys [args] :as parsed}] (merge (md/parse args) parsed))
           ->schema     (fn [{:keys [schema]}] [:=> schema (:schema return :any)])
           single       (= :single (key arities))
           parglists    (if single (->> arities val parse vector) (->> arities val :arities (map parse)))
           raw-arglists (map :raw-arglist parglists)
           schema       (as-> (map ->schema parglists) $ (if single (first $) (into [:function] $)))
           instr-fn-sym (gensym "instr-fn")]
       `(let [~instr-fn-sym
              (m/-instrument {:schema ~schema :report ~report}
                (c/fn ~name
                  ~@(map (fn [{:keys [arglist prepost body]}] `(~arglist ~prepost ~@body)) parglists)
                  ~@(when-not single (some->> arities val :meta vector))))]
          (c/defn ~name
            ~@(some-> doc vector)
            ~(assoc meta :raw-arglists (list 'quote raw-arglists), :schema schema)
            ~@(map (fn [{:keys [arglist prepost]}] `(~arglist ~prepost (~instr-fn-sym ~@arglist))) parglists)
            ~@(when-not single (some->> arities val :meta vector)))))))
;;
;;
;; public api
;;

#?(:clj (defmacro defn [& args] (-defn SchematizedParams args)))

#?(:clj (defmacro >defn
          "Emits an instrumented function which throws for invalid args or output schemas."
          [& args]
          (let [report
                (if (:ns &env)
                  `(fn [type# data#] ((malli.dev.pretty/thrower) type#
                                      (assoc data# :fn-name '~(symbol (str (:name (:ns &env))) (str (first args))))))
                  `(malli.dev.pretty/thrower))]
            (->defn SchematizedParams report args))))
