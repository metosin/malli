(ns malli.poly-protocols)

(defprotocol AllSchema
  (-bounds [this] "return a vector of maps describing the binder")
  (-inst [this schemas] "replace variables in polymorphic schema with schemas, or their defaults if nil"))
