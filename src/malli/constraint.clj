(ns malli.constraint
  (:require [clojure.set :as set]
            [malli.constraint.string :as mc-str]
            [malli.constraint.string.validate :as mc-strv]
            [malli.constraint.util :refer [composite-constraint-types
                                           -add-gen-key
                                           -generator-types]]
            [malli.impl.util :as miu :refer [-fail!]]))

(def keyset-constraints
  (let [constraint-types (into {} (map (juxt identity identity))
                               (concat composite-constraint-types
                                       ;;TODO :sorted
                                       [:disjoint :max :min :contains]))
        generator-constraint-types (-generator-types (keys constraint-types))
        validator-constraint-types (-> constraint-types
                                       ;; :gen/foo :=> :any
                                       (into (map (fn [c] [c :any])) (keys generator-constraint-types))
                                       ;;TODO :disjoint :disjoint-keys
                                       (assoc :max :max-count
                                              :min :min-count))]
    {:keyword-sugar :contains
     :nested-property-keys (into #{} (mapcat -add-gen-key)
                                 (-> composite-constraint-types (disj :not) (conj :disjoint)))
     :validator-constraint-types validator-constraint-types
     :generator-constraint-types (into validator-constraint-types
                                       generator-constraint-types)}))

(def number-constraints
  (let [constraint-types (into {} (map (juxt identity identity))
                               (concat composite-constraint-types #{:max :min :< :> :<= :>=}))
        generator-constraint-types (-generator-types (keys constraint-types))
        validator-constraint-types (-> constraint-types
                                       ;; :gen/foo :=> :any
                                       (into (map (fn [c] [c :any])) (keys generator-constraint-types))
                                       (assoc :max :<=
                                              :min :>=))]
    {:flat-property-keys (into #{} (mapcat -add-gen-key)
                               #{:max :min :< :> :<= :>= :not
                                 ;;TODO
                                 ;:even? :odd? :pos? :neg? :multiple
                                 })
     :nested-property-keys (into #{} (mapcat -add-gen-key)
                                 (-> composite-constraint-types (disj :not)))
     :validator-constraint-types validator-constraint-types
     :generator-constraint-types (into validator-constraint-types
                                       generator-constraint-types)}))

(def sequential-constraints
  (let [constraint-types (into {} (map (juxt identity identity))
                               (concat composite-constraint-types #{:max :min :distinct :sorted}))
        generator-constraint-types (-generator-types (keys constraint-types))
        validator-constraint-types (-> constraint-types
                                       ;; :gen/foo :=> :any
                                       (into (map (fn [c] [c :any])) (keys generator-constraint-types))
                                       (assoc :max :max-count
                                              :min :min-count))]
    {:flat-property-keys (into #{} (mapcat -add-gen-key)
                               #{:max :min :distinct :sorted})
     :generator-constraint-types (into validator-constraint-types
                                       generator-constraint-types)
     :validator-constraint-types validator-constraint-types}))

;; TODO :qualified-keyword + :namespace
;; TODO add to options
(defn schema-constraints []
  (into {:map keyset-constraints
         :set keyset-constraints
         :map-of keyset-constraints
         :int number-constraints
         :double number-constraints
         :vector sequential-constraints
         :sequential sequential-constraints}
        (mc-str/schema-constraints)))

(defn -resolve-op [constraint constraint-types options]
  (let [op (when (vector? constraint)
             (first constraint))
        op (or (get constraint-types op)
               (-fail! ::disallowed-constraint {:type op :constraint constraint
                                                :allowed (keys constraint-types)}))]
    (loop [op op]
      (let [op' (get constraint-types op op)]
        (cond-> op
          (not (identical? op op')) recur)))))

(defn -resolve-constraint-sugar [constraint constraint-opts options]
  (if-some [kw-sugar (when (keyword? constraint)
                       (:keyword-sugar constraint-opts))]
    (conj [kw-sugar] constraint)
    constraint))

(defn -contains-constraint-key
  "If :contains is a valid constraint, return its key.
  Recognizes symbol/keyword/string sugar for key."
  [constraint constraint-types options]
  (when (keyword? constraint)
    (when-some [kw-sugar (:keyword-sugar constraint-types)]
      [(conj [:contains] kw-sugar)]
      (when (and (vector? constraint)
                 (= :contains (-resolve-op constraint constraint-types options))
                 (or (= 2 (count constraint))
                     (-fail! ::contains-constraint-takes-one-child {:constraint constraint})))
        (subvec constraint 1)))))

(defn ->constraint-opts [type-or-map]
  (if (map? type-or-map)
    type-or-map
    (get (schema-constraints) type-or-map)))

;; TODO add to options
(defn validators []
  (mc-strv/validators))

(defn -constraint-validator [constraint constraint-opts options]
  (let [{:keys [validator-constraint-types] :as constraint-opts} (->constraint-opts constraint-opts)
        validators (validators)]
    (letfn [(-constraint-validator [constraint]
              (let [constraint (-resolve-constraint-sugar constraint constraint-opts options)
                    op (-resolve-op constraint validator-constraint-types options)]
                (if-some [custom-validator (validators op)]
                  (custom-validator {:constraint constraint
                                     :constraint-opts constraint-opts}
                                    options)
                  (case op
                    :any any?
                    :contains (let [[k :as all] (subvec constraint 1)
                                    _ (when-not (= 1 (count all))
                                        (-fail! ::contains-constraint-takes-one-child {:constraint constraint}))]
                                #(contains? % k))
                    :sorted (let [[v :as all] (subvec constraint 1)
                                  _ (when-not (#{[] [true]} all)
                                      (-fail! ::sorted-constraint-takes-one-child {:constraint constraint}))]
                              #(or (sorted? %)
                                   (and (or (string? %) ;; TODO test string
                                            (sequential? %))
                                        (try (= (seq %) (sort %))
                                             (catch Exception _ false)))))
                    :palindrome (let [[v :as all] (subvec constraint 1)
                                      _ (when-not (#{[] [true]} all)
                                          (-fail! ::palindrome-constraint-takes-one-child {:constraint constraint}))]
                                  #(= (sequence %)
                                      (if (reversible? %)
                                        (-> % rseq sequence)
                                        (reverse %))))
                    :distinct (let [[v :as all] (subvec constraint 1)
                                    _ (when-not (#{[] [true]} all)
                                        (-fail! ::distinct-constraint-takes-one-child {:constraint constraint}))]
                                #(or (empty? %) (apply distinct? %)))
                    (:<= :< :>= :>) (let [[n :as all] (subvec constraint 1)
                                          _ (when-not (= 1 (count all))
                                              (-fail! ::numeric-constraint-takes-one-child {:constraint constraint}))
                                          _ (when-not (number? n)
                                              (-fail! ::numeric-constraint-takes-integer {:constraint constraint}))]
                                      (case op
                                        :<  #(<  % n)
                                        :<= #(<= % n)
                                        :>  #(>  % n)
                                        :>= #(>= % n)))
                    (:max-count :min-count) (let [[n :as all] (subvec constraint 1)
                                                  _ (when-not (= 1 (count all))
                                                      (-fail! ::min-max-constraint-takes-one-child {:constraint constraint}))
                                                  _ (when-not (nat-int? n)
                                                      (-fail! ::min-max-constraint-takes-integer {:constraint constraint}))]
                                              (case op
                                                :max-count #(<= (count %) n)
                                                :min-count #(<= n (count %))))
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
                    (-fail! ::unknown-constraint {:constraint constraint})))))]
      (-constraint-validator constraint))))

(defn -constraint-from-properties [properties constraint-opts options]
  (let [{:keys [flat-property-keys nested-property-keys]} (->constraint-opts constraint-opts)]
    (when-some [cs (-> []
                       (into (keep #(some->> (get properties %)
                                             (into [%])))
                             nested-property-keys)
                       (into (keep #(some->> (get properties %)
                                             (conj [%])))
                             flat-property-keys)
                       not-empty)]
      (if (= 1 (count cs))
        (first cs)
        (into [:and] cs)))))
