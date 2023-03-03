(ns malli.instrument.fn-schemas2)

(def VecOfStrings [:vector :string])

(def string :string)

(def small-int
  [:int {:max 6}])

(def int-arg :int)
