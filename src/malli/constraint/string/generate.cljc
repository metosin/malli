(ns malli.constraint.string.generate)

(defn solvers []
  {:alpha-string (fn [{:keys [constraint]} options]
                   (let [[s] (next constraint)]
                     (assert (not (false? s)))
                     [{:string-class [[:alpha]]}]))
   :edn-string (fn [{:keys [constraint]} options]
                 (let [[s] (next constraint)]
                   (assert (not (false? s)))
                   [{:string-class [(cond-> [:edn]
                                      s (conj s))]}]))
   })

(defn generators []
  {})
