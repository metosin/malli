(ns malli.experimental.validate
  "Experimental :validate schema that allows outputting custom errors."
  (:require [malli.core :as m]
            [malli.impl.util :as miu]))

;; This is mostly a copy of -fn-schema. If we decide to move
;; -validate-schema to the core, it could be a couple of branches
;; inside -fn-schema.
(defn -validate-schema []
  ^{:type ::into-schema}
  (reify
    m/AST
    (-from-ast [parent ast options] (m/-from-value-ast parent ast options))
    m/IntoSchema
    (-type [_] :fn)
    (-type-properties [_])
    (-into-schema [parent properties children options]
      (m/-check-children! :fn properties children 1 1)
      (let [children (vec children)
            f (m/eval (first children) options)
            form (delay (m/-simple-form parent properties children identity options))
            cache (m/-create-cache options)]
        ^{:type ::schema}
        (reify
          m/AST
          (-to-ast [this _] (m/-to-value-ast this))
          m/Schema
          (-validator [_] (m/-safe-pred (fn [val] (nil? (f val)))))
          (-explainer [this path]
            (fn explain [x in0 acc]
              (try
                (if-let [errors (seq (f x))]
                  (into acc (map (fn [{:keys [in type value]}] (miu/-error path (into in0 in) this value type))) errors)
                  acc)
                (catch #?(:clj Exception, :cljs js/Error) e
                  (conj acc (miu/-error path in0 this x (:type (ex-data e))))))))
          (-parser [this] (m/-simple-parser this))
          (-unparser [this] (m/-parser this))
          (-transformer [this transformer method options]
            (m/-intercepting (m/-value-transformer transformer this method options)))
          (-walk [this walker path options] (m/-walk-leaf this walker path options))
          (-properties [_] properties)
          (-options [_] options)
          (-children [_] children)
          (-parent [_] parent)
          (-form [_] @form)
          m/Cached
          (-cache [_] cache)
          m/LensSchema
          (-keep [_])
          (-get [_ key default] (get children key default))
          (-set [this key value] (m/-set-assoc-children this key value))
          m/ParserInfo
          (-parser-info [_ _] {:simple-parser true})
          #?@(:cljs [IPrintWithWriter (-pr-writer [this writer opts] (m/-pr-writer-schema this writer opts))]))))
    #?@(:cljs [IPrintWithWriter (-pr-writer [this writer opts] (m/-pr-writer-into-schema this writer opts))])))

(defn schemas []
  {:validate (-validate-schema)})
