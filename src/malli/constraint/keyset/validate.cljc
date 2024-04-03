(ns malli.constraint.keyset.validate
  (:require [clojure.set :as set]
            [malli.impl.util :as miu]))

(defn validators []
  {:contains (fn [{:keys [constraint]} _]
               (let [[k :as all] (subvec constraint 1)
                     _ (when-not (= 1 (count all))
                         (miu/-fail! ::contains-constraint-takes-one-child {:constraint constraint}))]
                 #(contains? % k)))
   :disjoint (fn [{:keys [constraint]} _]
               (let [ksets (next constraint)
                     ps (mapv (fn [ks]
                                (when (empty? ks)
                                  (miu/-fail! ::disjoint-keyset-must-be-non-empty {:constraint constraint}))
                                (when-not (apply distinct? ks)
                                  (miu/-fail! ::disjoint-keyset-must-be-distinct {:constraint constraint}))
                                (when-not (vector? ks)
                                  (miu/-fail! ::disjoint-constraint-takes-vectors-of-keys {:constraint constraint}))
                                #(boolean
                                   (some (fn [k]
                                           (contains? % k))
                                         ks)))
                              ksets)
                     _ (when (next ksets)
                         (let [in-multiple (apply set/intersection (map set ksets))]
                           (when (seq in-multiple)
                             (miu/-fail! ::disjoint-keyset-must-be-distinct {:in-multiple-keys in-multiple}))))]
                 #(let [rs (keep-indexed (fn [i p]
                                           (when (p %)
                                             i))
                                         ps)]
                    (or (empty? rs)
                        (not (next rs))))))
   })
