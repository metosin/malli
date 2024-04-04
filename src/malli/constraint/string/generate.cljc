(ns malli.constraint.string.generate)

(defn solvers []
  {:alpha-string (fn [{:keys [constraint]} options]
                   (let [[s] (next constraint)]
                     (assert (not (false? s)))
                     [{:string-class #{:alpha}}]))
   :numeric-string (fn [{:keys [constraint]} options]
                     (let [[s] (next constraint)]
                       (assert (not (false? s)))
                       [{:string-class #{:numeric}}]))
   :alphanumeric-string (fn [{:keys [constraint]} options]
                          (let [[s] (next constraint)]
                            (assert (not (false? s)))
                            [{:string-class #{:alphanumeric}}]))
   :edn-string (fn [{:keys [constraint]} options]
                 (let [[s] (next constraint)]
                   (assert (not (false? s)))
                   [{:string-class #{(cond-> [:edn]
                                       s (conj s))}}]))
   })

(defn generators []
  {})
