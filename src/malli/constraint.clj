(ns malli.constraint
  (:require [clojure.set :as set]
            [malli.impl.util :as miu :refer [-fail!]]))

(def composite-constraint-types
  #{:and :or :implies :xor :iff :not #_:in})

(defn -add-gen-key [k]
  [k (keyword "gen" (name k))])

(defn -generator-types [constraint-types]
  (into {} (map (juxt #(keyword "gen" (name %))
                      identity))
        constraint-types))

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
    {:nested-property-keys (into #{} (mapcat -add-gen-key)
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

(def string-constraints
  (let [constraint-types (into {} (map (juxt identity identity))
                               (concat composite-constraint-types #{:max
                                                                    :min
                                                                    :re
                                                                    :alphanumeric
                                                                    :non-alphanumeric
                                                                    :numeric
                                                                    :non-numeric
                                                                    :alpha
                                                                    :non-alpha
                                                                    #_:trim
                                                                    #_:triml
                                                                    #_:trimr
                                                                    #_:trim-newline
                                                                    #_:blank
                                                                    #_:non-blank
                                                                    #_:starts-with
                                                                    #_:ends-with
                                                                    #_:upper-case
                                                                    #_:lower-case
                                                                    #_:ends-with
                                                                    #_:capitalized
                                                                    #_[:lines [:and [:< 1] [:<= 10]]]
                                                                    #_[:includes "foo"]
                                                                    #_[:escape {\a "__a__"}]
                                                                    #_[:index-of "foo" [:< 7]]
                                                                    #_[:last-index-of "foo" [:< 7]]
                                                                    #_[:split ]
                                                                    #_:palindrome
                                                                    }))
        generator-constraint-types (-generator-types (keys constraint-types))
        validator-constraint-types (-> constraint-types
                                       ;; :gen/foo :=> :any
                                       (into (map (fn [c] [c :any])) (keys generator-constraint-types))
                                       (assoc :max :max-count
                                              :min :min-count
                                              :alphanumeric :alphanumeric-string
                                              :non-alphanumeric :non-alphanumeric-string
                                              :numeric :numeric-string
                                              :non-numeric :non-numeric-string
                                              :alpha :alpha-string
                                              :non-alpha :non-alpha-string
                                              :re :re-string))]
    {:flat-property-keys (into #{} (mapcat -add-gen-key)
                               #{:max
                                 :min
                                 :re
                                 :alphanumeric
                                 :non-alphanumeric
                                 :numeric
                                 :non-numeric
                                 :alpha
                                 :non-alpha
                                 :not})
     :generator-constraint-types (into validator-constraint-types
                                       generator-constraint-types)
     :validator-constraint-types validator-constraint-types}))

;; TODO :qualified-keyword + :namespace
(def schema-constraints
  {:map keyset-constraints
   :set keyset-constraints
   :map-of keyset-constraints
   :int number-constraints
   :double number-constraints
   :vector sequential-constraints
   :sequential sequential-constraints
   :string string-constraints})

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

(defn -contains-constraint-key
  "If :contains is a valid constraint, return its key.
  Recognizes symbol/keyword/string sugar for key."
  [constraint constraint-types options]
  (when (:contains constraint-types)
    (if (or (symbol? constraint)
            (keyword? constraint)
            (string? constraint))
      [constraint]
      (when (and (vector? constraint)
                 (= :contains (-resolve-op constraint constraint-types options))
                 (or (= 2 (count constraint))
                     (-fail! ::contains-constraint-takes-one-child {:constraint constraint})))
        (subvec constraint 1)))))

(defn ->constraint-opts [type-or-map]
  (if (map? type-or-map)
    type-or-map
    (get schema-constraints type-or-map)))

(defn -constraint-validator [constraint constraint-opts options]
  (let [{:keys [validator-constraint-types]} (->constraint-opts constraint-opts)]
    (letfn [(-constraint-validator [constraint]
              (if-some [[k] (when (= :contains (:contains validator-constraint-types))
                              (-contains-constraint-key constraint constraint-opts options))]
                #(contains? % k)
                (let [op (-resolve-op constraint validator-constraint-types options)]
                  (case op
                    :alpha-string (fn [s] (every? #(Character/isLetter (int %)) s))
                    :non-alpha-string (fn [s] (not-any? #(Character/isLetter (int %)) s))
                    :numeric-string (fn [s] (every? #(Character/isDigit (int %)) s))
                    :non-numeric-string (fn [s] (not-any? #(Character/isDigit (int %)) s))
                    :alphanumeric-string (fn [s] (every? #(Character/isLetterOrDigit (int %)) s))
                    :non-alphanumeric-string (fn [s] (not-any? #(Character/isLetterOrDigit (int %)) s))
                    :any any?
                    :sorted (let [[v :as all] (subvec constraint 1)
                                  _ (when-not (= [true] all)
                                      (-fail! ::sorted-in-constraint-takes-one-child {:constraint constraint}))]
                              #(or (sorted? %)
                                   (and (sequential? %)
                                        (try (= % (sort %))
                                             (catch Exception _ false)))))
                    :distinct (let [[v :as all] (subvec constraint 1)
                                    _ (when-not (= [true] all)
                                        (-fail! ::distinct-in-constraint-takes-one-child {:constraint constraint}))]
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
