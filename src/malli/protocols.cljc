(ns malli.protocols
  (:refer-clojure :exclude [-deref deref]))

(defprotocol IntoSchema
  (-type [this] "returns type of the schema")
  (-type-properties [this] "returns schema type properties")
  (-properties-schema [this options] "maybe returns :map schema describing schema properties")
  (-children-schema [this options] "maybe returns sequence schema describing schema children")
  (-into-schema [this properties children options] "creates a new schema instance"))

(defprotocol Schema
  (-validator [this] "returns a predicate function that checks if the schema is valid")
  (-explainer [this path] "returns a function of `x in acc -> maybe errors` to explain the errors for invalid values")
  (-parser [this] "return a function of `x -> parsed-x | ::m/invalid` to explain how schema is valid.")
  (-unparser [this] "return the inverse (partial) function wrt. `-parser`; `parsed-x -> x | ::m/invalid`")
  (-transformer [this transformer method options]
    "returns a function to transform the value for the given schema and method.
    Can also return nil instead of `identity` so that more no-op transforms can be elided.")
  (-walk [this walker path options] "walks the schema and it's children, ::m/walk-entry-vals, ::m/walk-refs, ::m/walk-schema-refs options effect how walking is done.")
  (-properties [this] "returns original schema properties")
  (-options [this] "returns original options")
  (-children [this] "returns schema children")
  (-parent [this] "returns the IntoSchema instance")
  (-form [this] "returns original form of the schema"))

(defprotocol MapSchema
  (-entries [this] "returns sequence of `key -val-schema` MapEntries"))

(defprotocol LensSchema
  (-keep [this] "returns truthy if schema contributes to value path")
  (-get [this key default] "returns schema at key")
  (-set [this key value] "returns a copy with key having new value"))

(defprotocol RefSchema
  (-ref [this] "returns the reference name")
  (-deref [this] "returns the referenced schema"))

(defprotocol RegexSchema
  (-regex-op? [this] "is this a regex operator (e.g. :cat, :*...)")
  (-regex-validator [this] "returns the raw internal regex validator implementation")
  (-regex-explainer [this path] "returns the raw internal regex explainer implementation")
  (-regex-unparser [this] "returns the raw internal regex unparser implementation")
  (-regex-parser [this] "returns the raw internal regex parser implementation")
  (-regex-transformer [this transformer method options] "returns the raw internal regex transformer implementation")
  (-regex-min-max [this] "returns size of the sequence as [min max] vector. nil max means unbuond."))

(defprotocol Walker
  (-accept [this schema path options])
  (-inner [this schema path options])
  (-outer [this schema path children options]))

(defprotocol Transformer
  (-transformer-chain [this] "returns transformer chain as a vector of maps with :name, :encoders, :decoders and :options")
  (-value-transformer [this schema method options] "returns an value transforming interceptor for the given schema and method"))
