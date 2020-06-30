(ns malli.impl.cljs.dynaload
  {:no-doc true}
  (:require-macros
   [malli.impl.cljs.dynaload :refer [dynaload]]))

(deftype LazyVar [f ^:mutable cached]
  IDeref
  (-deref [this]
    (if-not (nil? cached)
      cached
      (let [x (f)]
        (when-not (nil? x)
          (set! cached x))
        x))))

(def eval-string
  (dynaload 'sci.core/eval-string))
