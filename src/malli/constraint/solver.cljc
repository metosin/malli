(ns malli.constraint.solver
  (:require [clojure.math.combinatorics :as comb]
            [clojure.set :as set]
            [malli.constraint.string.generate :as mcg-str]
            [malli.constraint :as mc]
            [malli.impl.util :as miu]))

(defn default-constraint-solvers []
  (merge (mcg-str/solvers)))

(defn -conj-number-constraints [[sol1 & sols :as all-sols]]
  (if (empty? all-sols)
    [{}]
    (let [lt (some->> (seq (keep :< all-sols)) (apply min))
          gt (some->> (seq (keep :> all-sols)) (apply max))
          lte (some->> (seq (keep :<= all-sols)) (apply min))
          gte (some->> (seq (keep :>= all-sols)) (apply max))
          max-count (some->> (seq (keep :max-count all-sols)) (apply min))
          min-count (some->> (seq (keep :min-count all-sols)) (apply max))
          [upper upper-op] (when (or lt lte)
                             (if (and lt lte)
                               (if (<= lt lte)
                                 [lt :<]
                                 [lte :<=])
                               (if lt
                                 [lt :<]
                                 [lte :<=])))
          [lower lower-op] (when (or gt gte)
                             (if (and gt gte)
                               (if (<= gt gte)
                                 [gte :>=]
                                 [gt :>])
                               (if gt
                                 [gt :>]
                                 [gte :>=])))
          number-solutions (lazy-seq
                             (if (and lower upper)
                               (if (< lower upper)
                                 [{lower-op lower
                                   upper-op upper}]
                                 (if (and (= lower upper)
                                          (and (#{:>= :>} lower-op)
                                               (= :<= upper-op)))
                                   [{lower-op lower
                                     upper-op upper}]
                                   []))
                               (if lower
                                 [{lower-op lower}]
                                 (if upper
                                   [{upper-op upper}]
                                   [{}]))))
          count-solutions (lazy-seq
                            (if (and min-count max-count)
                              (if (<= min-count max-count)
                                [{:min-count min-count
                                  :max-count max-count}]
                                [])
                              (if min-count
                                [{:min-count min-count}]
                                (if max-count
                                  [{:max-count max-count}]
                                  [{}]))))
          all-solutions [number-solutions count-solutions]]
      (lazy-seq
        (->> (apply comb/cartesian-product all-solutions)
             (map #(apply merge %)))))))

(comment
 (map #(apply merge %) (comb/cartesian-product [{:< 1} {:> 2}] [{:max-count 1} {:min-count 3}]))
 (assert (= (-conj-number-constraints [{:max-count 10} {:min-count 1}])
            [{:max-count 10 :min-count 1}]))
 (assert (= (-conj-number-constraints [{:max-count 1} {:min-count 10}])
            []))
 (assert (= (-conj-number-constraints [{:max-count 1} {:min-count 10}])
            []))
 (assert (= (-conj-number-constraints [{:max-count 5 :> 3} {:min-count 4 :< 4}])
            [{:> 3, :< 4, :min-count 4, :max-count 5}]))
 (assert (= (-conj-number-constraints [{:max-count 5 :> 3} {:min-count 4 :> 4}])
            [{:> 4, :min-count 4, :max-count 5}]))

)

(defn -conj-solutions [& sols]
  (letfn [(rec [cart-sols]
            (lazy-seq
              (when-some [[[sol1 & sols :as all-sols]] (seq cart-sols)]
                (when-some [unsupported-keys (not-empty
                                               (disj (into #{} (mapcat keys) all-sols)
                                                     :< :> :<= :>= :max-count :min-count
                                                     :present :order :string-class))]
                  (miu/-fail! ::unsupported-conj-solution {:unsupported-keys unsupported-keys}))
                (let [string-solutions
                      (if (not-any? :string-class all-sols)
                        [{}]
                        (let [sc (into #{} (mapcat :string-class) all-sols)
                              sc (cond-> sc
                                   ;; numeric/alpha subsumes alphanumeric
                                   (and (:alphanumeric sc)
                                        (or (:numeric sc)
                                            (:alpha sc)))
                                   (disj :alphanumeric))]
                          (if (and (:numeric sc)
                                   (:alpha sc))
                            ;; unsatisfiable
                            []
                            [{:string-class sc}])))
                      number-solutions
                      (if (not-any? (some-fn :< :> :<= :>= :max-count :min-count) all-sols)
                        [{}]
                        (-conj-number-constraints all-sols))
                      keyset-solutions
                      (if (not-any? (some-fn :present :order) all-sols)
                        [{}]
                        (if-some [present (reduce (fn [p1 {p2 :present}]
                                                    (reduce-kv (fn [acc k present1]
                                                                 (if-some [[_ present2] (find acc k)]
                                                                   (if (identical? present1 present2)
                                                                     acc
                                                                     (reduced (reduced nil)))
                                                                   (assoc acc k present1)))
                                                               p1 p2))
                                                  (:present sol1) sols)]
                          [(-> sol1
                               (update :order into (mapcat :order) sols)
                               (assoc :present present))]
                          []))
                      combined-sols (comb/cartesian-product
                                      string-solutions number-solutions keyset-solutions)]
                  (if (empty? combined-sols)
                    []
                    (concat (map #(apply merge %) combined-sols)
                            (rec (rest cart-sols))))))))]
    (distinct (rec (apply comb/cartesian-product (distinct sols))))))

(comment
  (assert (= (-conj-solutions)
             []))
  (assert (= (-conj-solutions [{:order [:a], :present {:a true}}])
             [{:order [:a], :present {:a true}}]))
  (assert (= (-conj-solutions [] [{:order [:a], :present {:a true}}])
             []))
  (assert (= (-conj-solutions [{:order [:a], :present {:a true}}] [{:order [:a], :present {:a true}}])
             [{:order [:a], :present {:a true}}]))
  (assert (= (-conj-solutions '({:order [:a], :present {:a true}})
                              '({:order [:b], :present {:b false}}))
             [{:order [:a :b] :present {:a true :b false}}]))
  (assert (= (-conj-solutions '({:< 2})
                              '({:> 0}))
             [{:> 0 :< 2}]))
  (assert (= (-conj-solutions '({:< 2})
                              '({:< 0}))
             [{:< 0}]))
  (assert (= (-conj-solutions '({:<= 2})
                              '({:<  2}))
             [{:< 2}]))
  (assert (= (-conj-solutions '({:<= 2})
                              '({:<  2}))
             [{:< 2}]))
  (assert (= (-conj-solutions '({:<= 2})
                              '({:<= 0}))
             [{:<= 0}]))
  (assert (= (-conj-solutions '({:<= 2})
                              '({:< 0}))
             [{:< 0}]))
  (assert (= (-conj-solutions '({:< 2})
                              '({:<= 0}))
             [{:< 0}]))
  (assert (= (-conj-solutions '({:< 2})
                              '({:> 2}))
             []))
  (assert (= (-conj-solutions '({:> 2})
                              '({:> 0}))
             [{:> 2}]))
  (assert (= (-conj-solutions '({:>= 2})
                              '({:> 0}))
             [{:>= 2}]))
  (assert (= (-conj-solutions '({:> 2})
                              '({:>= 0}))
             [{:> 2}]))
  (assert (= (-conj-solutions '({:>= 2})
                              '({:>= 0}))
             [{:>= 2}]))
  (assert (= (-conj-solutions '({:>= 2})
                              '({:> 2}))
             [{:>= 2}]))
  (assert (= (-conj-solutions '({:>= 2})
                              '({:> 2}))
             [{:>= 2}]))
  (assert (= (-conj-solutions '({:>= 2})
                              '({:> 1}))
             [{:>= 2}]))
  (assert (= (-conj-solutions '({:>= 3})
                              '({:> 2}))
             [{:>= 3}]))
  (assert (= (-conj-solutions '({:>= 2})
                              '({:< 2}))
             [{:>= 2}]))
  )

;; math.combinatorics
(defn- unchunk
  "Given a sequence that may have chunks, return a sequence that is 1-at-a-time
lazy with no chunks. Chunks are good for efficiency when the data items are
small, but when being processed via map, for example, a reference is kept to
every function result in the chunk until the entire chunk has been processed,
which increases the amount of memory in use that cannot be garbage
collected."
  [s]
  (lazy-seq
    (when (seq s)
      (cons (first s) (unchunk (rest s))))))

;; math.combinatorics
(defn- join
  "Lazily concatenates a collection of collections into a flat sequence,
  because Clojure's `apply concat` is insufficiently lazy."
  [colls]
  (lazy-seq
   (when-let [s (seq colls)]
     (concat (first s) (join (rest s))))))

;; math.combinatorics
(defn- mapjoin
  "Uses join to achieve lazier version of mapcat (on one collection)"
  [f coll]
  (join (map f coll)))

(defn -constraint-solutions [constraint constraint-opts options]
  {:post [(every? map? %)]}
  (let [{:keys [generator-constraint-types] :as constraint-opts} (mc/->constraint-opts constraint-opts)
        ;;TODO parameterize
        solvers (default-constraint-solvers)]
    (letfn [(-constraint-solutions
              ([constraint] (-constraint-solutions constraint options))
              ([constraint options]
               (lazy-seq
                 (let [constraint (mc/-resolve-constraint-sugar constraint constraint-opts options)
                       op (mc/-resolve-op constraint generator-constraint-types options)]
                   (if-some [solver (solvers op)]
                     (solver {:constraint (if (= :any op) [:any] constraint)
                              :constraint-opts constraint-opts}
                             options)
                     (case op
                       (:max-count :min-count) [{op (second constraint)}]
                       :contains (let [[k] (next constraint)]
                                   [{:order [k]
                                     :present {k true}}])
                       (:<= :< :>= :>) (let [[n :as all] (subvec constraint 1)]
                                         [{op n}])
                       :not (let [[c] (next constraint)
                                  sol (-constraint-solutions c)]
                              (if (empty? sol)
                                [{:order [] :present {}}]
                                (if (some #(= 0 (:min-count %)) sol)
                                  [] ;;unsatisfiable
                                  (sequence (comp (map (fn [s]
                                                         (when-some [unsupported-keys
                                                                     (not-empty
                                                                       (disj (set (keys s))
                                                                             :max-count :min-count
                                                                             :< :> :<= :>=
                                                                             :string-class))]
                                                           (miu/-fail! ::unsupported-negated-solution
                                                                       {:unsupported-keys unsupported-keys
                                                                        :solution s}))
                                                         (-> s
                                                             (set/rename-keys {:< :>=
                                                                               :> :<=
                                                                               :<= :>
                                                                               :>= :<})
                                                             (cond->
                                                               (:max-count s) (-> (assoc :min-count (inc (:max-count s)))
                                                                                  (dissoc :max-count))
                                                               (:min-count s) (-> (assoc :max-count (dec (:min-count s)))
                                                                                  (dissoc :min-count))

                                                               (:string-class s)
                                                               (update
                                                                 :string-class
                                                                 (fn [string-class]
                                                                   (into #{} (map mcg-str/negate-string-class)
                                                                         string-class)))
                                                               (:present s)
                                                               (update :present update-vals not)))))
                                                  (distinct))
                                            (unchunk sol)))))
                       :and (apply -conj-solutions (map -constraint-solutions (unchunk (next constraint))))
                       (:or :xor) (let [cs (subvec constraint 1)
                                        ndisjuncts (count cs)
                                        base-id (vec (range ndisjuncts))]
                                    (when (pos? ndisjuncts)
                                      (let [solution-cache (atom {})
                                            solve-combination (fn solve-combination [id]
                                                                (if-some [[_ res] (find @solution-cache id)]
                                                                  res
                                                                  (let [nid (count id)
                                                                        sol (if (= 1 nid)
                                                                              (let [i (nth id 0)]
                                                                                (-constraint-solutions
                                                                                  ;; [0 1 2 3 4] === solve all
                                                                                  ;; [-1 -2 -3 -4 -5] === negate all
                                                                                  (if (neg? i)
                                                                                    [:not (nth cs (- (inc i)))]
                                                                                    (nth cs i))))
                                                                              (let [mid (quot nid 2)]
                                                                                (or (when-some [left (not-empty
                                                                                                       (solve-combination
                                                                                                         (subvec id 0 mid)))]
                                                                                      (when-some [right (not-empty
                                                                                                          (solve-combination
                                                                                                            (subvec id mid)))]
                                                                                        (-conj-solutions left right)))
                                                                                    [])))]
                                                                    (swap! solution-cache assoc id sol)
                                                                    sol)))]
                                        (when-some [satisfiable-disjuncts (seq
                                                                            (keep #(some->> (not-empty (solve-combination [%]))
                                                                                            (vector %))
                                                                                  (unchunk (range ndisjuncts))))]
                                          (distinct
                                            (case op
                                              :xor (let [perm-coll (into [true] (repeat (dec ndisjuncts) false))
                                                         nperm (comb/count-permutations perm-coll)]
                                                     (mapjoin #(solve-combination (into []
                                                                                        (map-indexed (fn [i pos-neg]
                                                                                                       (cond-> i
                                                                                                         (not pos-neg) (-> - dec))))
                                                                                        (comb/nth-permutation perm-coll %)))
                                                              (unchunk (range nperm))))
                                              :or (concat
                                                    ;;first just satisfy each disjunct to reduce search space of comb/selections
                                                    ;;also the smallest solutions will go first, since negating the other disjuncts
                                                    ;;could increase the size of the solutions.
                                                    (mapjoin second satisfiable-disjuncts)

                                                    ;; now satisfy combinations of disjuncts
                                                    ;; true = solve
                                                    ;; false = ignore
                                                    (lazy-seq
                                                      (let [base-id (mapv first satisfiable-disjuncts)
                                                            idx->id (into {} (map-indexed (fn [i id] {i id})) base-id)]
                                                        (mapjoin
                                                          (fn [[i msk]]
                                                            (let [solution-id (into [] (keep-indexed
                                                                                         (fn [i need-to-satisfy]
                                                                                           (when need-to-satisfy
                                                                                             (nth base-id i))))
                                                                                    msk)]
                                                              (solve-combination solution-id)))
                                                          (map vector (unchunk (range)) (next (comb/selections [false true] (count base-id)))))))
                                                    ;;TODO
                                                    ;; true = solve
                                                    ;; false = solve negation
                                                    #_
                                                    (lazy-seq
                                                      (when-some [satisfiable-negated-disjuncts
                                                                  (seq
                                                                    (keep #(some->> (not-empty (solve-combination [%]))
                                                                                    (vector (dec (- i))))
                                                                          (unchunk (range ndisjuncts))))]
                                                        (keep-indexed
                                                          (fn [i msk]
                                                            (if (< ndisjuncts i)
                                                              (when (every? identity (map (fn [satisfiable must-satisfy]
                                                                                            (or (not must-satisfy)
                                                                                                satisfiable))
                                                                                          @seen-satisfiable msk))
                                                                [i ])
                                                              ;;FIXME right-to-left?
                                                              [i (map (fn []) cs msk)]))
                                                          (next (comb/selections [false true] (dec (count solvable))))))))))))))
                       :disjoint (let [ksets (subvec constraint 1)
                                       order (into [] (mapcat identity) ksets)
                                       base {:order order :present (zipmap order (repeat false))}]
                                   (cons base
                                         (mapjoin (fn [kset]
                                                    (map #(update base :present into (zipmap kset %))
                                                         (next (comb/selections [false true] (count kset)))))
                                                  (unchunk ksets))))
                       :iff (let [cs (subvec constraint 1)]
                              (concat (-constraint-solutions (into [:and] (map #(do [:not %])) cs))
                                      (lazy-seq
                                        (-constraint-solutions (into [:and] cs)))))
                       :implies (let [[c & cs] (next constraint)
                                      not-c-sol (seq (-constraint-solutions [:not c]))]
                                  (concat
                                    not-c-sol
                                    (lazy-seq
                                      (concat
                                        (-conj-solutions (-constraint-solutions c)
                                                         (-constraint-solutions (into [:and] cs)))
                                        (lazy-seq
                                          (let [or-cs-sol (-constraint-solutions (into [:or] cs))]
                                            (concat
                                              or-cs-sol
                                              (-conj-solutions not-c-sol or-cs-sol))))))))
                       (miu/-fail! ::unknown-constraint {:constraint constraint})))))))]
     (-constraint-solutions constraint))))

