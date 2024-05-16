(ns malli.constraint.keyset.validate
  (:require [clojure.set :as set]
            [malli.core :as m]
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
   :dispatch (fn [{:keys [constraint constraint-validator]} options]
               (when-not (next constraint)
                 (miu/-fail! ::dispatch-constraint-must-supply-dispatch-fn {:constraint constraint}))
               (let [[_ dispatch & clauses] constraint
                     dispatch (m/eval dispatch options)
                     {::m/keys [default] :as dispatch-map} (into {} (map (fn [[k v]]
                                                                           [k (constraint-validator v)]))
                                                                 clauses)]
                 (fn [x] (if-let [validator (dispatch-map (dispatch x) default)] (validator x) false))))
   })
