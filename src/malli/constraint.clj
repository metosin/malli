(ns malli.constraint
  (:require [clojure.set :as set]
            [malli.impl.util :refer [-fail!]]))

(defn -contains-constraint-key [constraint]
  (if (or (symbol? constraint)
          (keyword? constraint)
          (string? constraint))
    [constraint]
    (when (and (vector? constraint)
               (= :contains (first constraint))
               (or (= 2 (count constraint))
                   (-fail! ::contains-constraint-takes-one-child {:constraint constraint})))
      (subvec constraint 1))))

(defn -constraint-validator [constraint options]
  (letfn [(-constraint-validator [constraint]
            (if-some [[k] (-contains-constraint-key constraint)]
              #(contains? % k)
              (let [op (when (vector? constraint)
                         (first constraint))]
                (case op
                  (:<= :< :>= :>) (let [[n :as all] (subvec constraint 1)
                                        _ (when-not (= 1 (count all))
                                            (-fail! ::numeric-constraint-takes-one-child {:constraint constraint}))
                                        _ (when-not (number? n)
                                            (-fail! ::numeric-constraint-takes-integer {:constraint constraint}))]
                                    (case op
                                      :<  #(<  n %)
                                      :<= #(<= n %)
                                      :>  #(>  n %)
                                      :>= #(<= n %)))
                  (:max :min) (let [[n :as all] (subvec constraint 1)
                                    _ (when-not (= 1 (count all))
                                        (-fail! ::min-max-constraint-takes-one-child {:constraint constraint}))
                                    _ (when-not (nat-int? n)
                                        (-fail! ::min-max-constraint-takes-integer {:constraint constraint}))]
                                (case op
                                  :max #(<= (count %) n)
                                  :min #(<= n (count %))))
                  :not (let [[p :as all] (next constraint)
                             _ (when-not (= 1 (count all))
                                 (-fail! ::not-constraint-takes-one-child {:constraint constraint}))
                             p (-constraint-validator p)]
                         #(not (p %)))
                  :and (let [ps (mapv -constraint-validator (next constraint))]
                         #(every? (fn [p] (p %)) ps))
                  :or (let [ps (mapv -constraint-validator (next constraint))]
                        #(boolean 
                           (some (fn [p] (p %)) ps)))
                  :xor (let [ps (mapv -constraint-validator (next constraint))]
                         #(let [rs (filter (fn [p] (p %)) ps)]
                            (boolean
                              (and (seq rs) (not (next rs))))))
                  :disjoint (let [ksets (next constraint)
                                  ps (mapv (fn [ks]
                                             (when (empty? ks)
                                               (-fail! ::disjoint-keyset-must-be-non-empty {:constraint constraint}))
                                             (when-not (apply distinct? ks)
                                               (-fail! ::disjoint-keyset-must-be-distinct {:constraint constraint}))
                                             (when-not (vector? ks)
                                               (-fail! ::disjoint-constraint-takes-vectors-of-keys {:constraint constraint}))
                                             #(boolean
                                                (some (fn [k]
                                                        (contains? % k))
                                                      ks)))
                                           ksets)
                                  _ (when (next ksets)
                                      (let [in-multiple (apply set/intersection (map set ksets))]
                                        (when (seq in-multiple)
                                          (-fail! ::disjoint-keyset-must-be-distinct {:in-multiple-keys in-multiple}))))]
                              #(let [rs (keep-indexed (fn [i p]
                                                        (when (p %)
                                                          i))
                                                      ps)]
                                 (or (empty? rs)
                                     (not (next rs)))))
                  :iff (let [[p & ps] (mapv -constraint-validator (next constraint))]
                         (when-not p
                           (-fail! ::empty-iff))
                         #(let [expect (p %)]
                            (every? (fn [p] (identical? expect (p %))) ps)))
                  :implies (let [[p & ps] (mapv -constraint-validator (next constraint))]
                             (when-not p
                               (-fail! ::missing-implies-condition {:constraint constraint}))
                             #(or (not (p %))
                                  (every? (fn [p] (p %)) ps)))
                  (-fail! ::unknown-keyset-constraint {:constraint constraint})))))]
    (-constraint-validator constraint)))

(defn -constraint-from-properties [properties options]
  (when-some [cs (-> []
                     (into (get properties :and))
                     (into (keep #(some->> (get properties %)
                                           (into [%]))
                                 (concat [:disjoint :iff :implies :or :xor]
                                         ;;TODO better name
                                         #_(::extra-constraint-properties-sugar options))))
                     not-empty)]
    (if (= 1 (count cs))
      (first cs)
      (into [:and] cs))))
