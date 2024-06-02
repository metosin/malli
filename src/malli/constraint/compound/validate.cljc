(ns malli.constraint.compound.validate
  (:require [malli.impl.util :as miu]))


(defn validators []
  {:not (fn [{:keys [constraint constraint-validator] :as constraint-opts} options]
          (let [[p :as all] (next constraint)
                _ (when-not (= 1 (count all))
                    (miu/-fail! ::not-constraint-takes-one-child {:constraint constraint}))
                p (constraint-validator p)]
            (comp not p)))
   :and (fn [{:keys [constraint constraint-validator]} _]
          (let [ps (mapv constraint-validator (next constraint))]
            (when-not (seq ps)
              (miu/-fail! ::empty-and))
            #(every? (fn [p] (p %)) ps)))
   :or (fn [{:keys [constraint constraint-validator]} _]
         (let [ps (mapv constraint-validator (next constraint))]
            (when-not (seq ps)
              (miu/-fail! ::empty-or))
           #(boolean 
              (some (fn [p] (p %)) ps))))
   :xor (fn [{:keys [constraint constraint-validator]} _]
          (let [ps (mapv constraint-validator (next constraint))]
            (when-not (seq ps)
              (miu/-fail! ::empty-xor))
            #(let [rs (filter (fn [p] (p %)) ps)]
               (boolean
                 (and (seq rs) (not (next rs)))))))
   :iff (fn [{:keys [constraint constraint-validator]} _]
          (let [[p & ps] (mapv constraint-validator (next constraint))]
            (when-not p
              (miu/-fail! ::empty-iff))
            #(let [expect (p %)]
               (every? (fn [p] (identical? expect (p %))) ps))))
   :implies (fn [{:keys [constraint constraint-validator]} _]
              (let [[p & ps] (mapv constraint-validator (next constraint))]
                (when-not p
                  (miu/-fail! ::missing-implies-condition {:constraint constraint}))
                #(or (not (p %))
                     (every? (fn [p] (p %)) ps))))})
