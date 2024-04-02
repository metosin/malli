;; See also `malli.generator-ast` for viewing generators as data
(ns malli.generator
  (:require [clojure.core :as cc]
            [clojure.math.combinatorics :as comb]
            [clojure.spec.gen.alpha :as ga]
            [clojure.set :as set]
            [clojure.string :as str]
            [clojure.test.check :as check]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.random :as random]
            [clojure.test.check.rose-tree :as rose]
            [malli.constraint :as mc]
            [malli.core :as m]
            [malli.registry :as mr]
            [malli.impl.util :as miu :refer [-last -merge]]
            #?(:clj [borkdude.dynaload :as dynaload])))

(declare generator generate -create)

(defprotocol Generator
  (-generator [this options] "returns generator for schema"))

;;
;; generators
;;


;; # Notes for implementors
;;
;; For the most part, -schema-generator is a pretty direct translation from schemas to generators.
;; However, the naive implementation of recursive ref's (creating a generator for the dereferenced ref
;; and cutting off the generation at a certain depth) tends to create exponentially large test cases.
;;
;; We use a more sophisticated algorithm to achieve linearly sized test cases with recursive refs.
;; The next section describes the strategy implementors should use to participate in this improved behavior.
;; The theory behind this strategy is described in the section below ("Approach for recursive generators").
;;
;; ## Implementation strategy
;;
;; Say you have a composite schema you want to generate values for. You should extend `-schema-generator` and
;; call `generator` recursively on the `m/children`. Now, for every child generator, you need to consider the case
;; that the child generator generates no values, and how this might change the final generator.
;;
;; Use `-unreachable-gen?` to test whether your child generator generates no values (we'll call this an "unreachable" schema/generator).
;; If your parent generator cannot generate values, use `-never-gen` to return an unreachable generator.
;; 
;; Here are a few examples---compare them with the logic in their respective -schema-generator methods:
;;   [:maybe M] would generate like :nil if M were unreachable.
;;   [:map [:a M]] would itself be unreachable if M were unreachable.
;;   [:map [:a {:optional true} M]] would generate like [:map] if M were unreachable.
;;   [:vector M] would generate like [:= []] if M were unreachable.
;;   [:vector {:min 1} M] would itself be unreachable if M were unreachable.

(def nil-gen (gen/return nil))

(defn -never-gen
  "Return a generator of no values that is compatible with -unreachable-gen?."
  [{::keys [original-generator-schema] :as _options}]
  (with-meta (gen/such-that (fn [_]
                              (throw (ex-info
                                      (str "Cannot generate values due to infinitely expanding schema: "
                                           (if original-generator-schema
                                             (m/form original-generator-schema)
                                             "<no schema form>"))
                                      (cond-> {}
                                        original-generator-schema (assoc :schema (m/form original-generator-schema))))))
                            gen/any)
             {::never-gen true
              ::original-generator-schema original-generator-schema}))

(defn -unreachable-gen?
  "Returns true iff generator g generators no values."
  [g] (-> (meta g) ::never-gen boolean))

(defn -not-unreachable [g] (when-not (-unreachable-gen? g) g))

(defn- -random [seed] (if seed (random/make-random seed) (random/make-random)))

(defn ^:deprecated -recur [_schema options]
  (println (str `-recur " is deprecated, please update your generators. See instructions in malli.generator."))
  [true options])

(defn ^:deprecated -maybe-recur [_schema options]
  (println (str `-maybe-recur " is deprecated, please update your generators. See instructions in malli.generator."))
  options)

(defn -min-max [schema options]
  (let [{:keys [min max] gen-min :gen/min gen-max :gen/max} (m/properties schema options)]
    (when (and min gen-min (< gen-min min))
      (m/-fail! ::invalid-property {:key :gen/min, :value gen-min, :min min}))
    (when (and max gen-max (> gen-max max))
      (m/-fail! ::invalid-property {:key :gen/max, :value gen-min, :max min}))
    {:min (or gen-min min)
     :max (or gen-max max)}))

(defn- -double-gen [options] (gen/double* (merge {:infinite? false, :NaN? false} options)))

(defn- gen-vector-min [gen min options]
  (cond-> (gen/sized #(gen/vector gen min (+ min %)))
    (::generator-ast options) (vary-meta assoc ::generator-ast
                                         {:op :vector-min
                                          :generator gen
                                          :min min})))

(defn- -string-gen [schema options]
  (let [{:keys [min max]} (-min-max schema options)]
    (cond
      (and min (= min max)) (gen/fmap str/join (gen/vector gen/char-alphanumeric min))
      (and min max) (gen/fmap str/join (gen/vector gen/char-alphanumeric min max))
      min (gen/fmap str/join (gen-vector-min gen/char-alphanumeric min options))
      max (gen/fmap str/join (gen/vector gen/char-alphanumeric 0 max))
      :else gen/string-alphanumeric)))

(defn- -coll-gen [schema f options]
  (let [{:keys [min max]} (-min-max schema options)
        child (-> schema m/children first)
        gen (generator child options)]
    (if (-unreachable-gen? gen)
      (if (= 0 (or min 0))
        (gen/fmap f (gen/return []))
        (-never-gen options))
      (gen/fmap f (cond
                    (and min (= min max)) (gen/vector gen min)
                    (and min max) (gen/vector gen min max)
                    min (gen-vector-min gen min options)
                    max (gen/vector gen 0 max)
                    :else (gen/vector gen))))))

(defn -mentioned-constraint-keys [constraint constraint-types options]
  (letfn [(-mentioned-constraint-keys [constraint]
            (or (mc/-contains-constraint-key constraint constraint-types options)
                (case (mc/-resolve-op constraint constraint-types options)
                  :any []
                  (:not :and :or :xor :iff :implies) (mapcat -mentioned-constraint-keys (next constraint))
                  :disjoint (apply concat (next constraint))
                  (or (when-some [mentioned-constraint-keys (::mentioned-constraint-keys options)]
                        (mentioned-constraint-keys constraint options))
                      (m/-fail! ::unknown-keyset-constraint {:constraint constraint})))))]
    (-mentioned-constraint-keys constraint)))

(defn -valid-map-keysets [schema options]
  {:pre [(= :map (m/type schema))]}
  (when-some [constraint (mc/-constraint-from-properties
                           (m/properties schema)
                           :map
                           options)]
    (let [{required false
           optional true} (group-by #(-> % -last m/properties :optional boolean)
                                    (m/entries schema))
          ;; add keys only mentioned in :keyset constraints
          optional (into [] (distinct)
                         (concat (map first optional)
                                 (-mentioned-constraint-keys
                                   constraint
                                   (:generator-constraint-types
                                     (mc/->constraint-opts (m/type schema)))
                                   options)))
          base (into {} (map (fn [[k]]
                               {k :required}))
                     required)
          p (mc/-constraint-validator constraint :map options)]
      (into [] (comp (keep (fn [optionals]
                             (let [example (-> base
                                               (into (map (fn [k]
                                                            {k :required}))
                                                     optionals))]
                               (when (p example)
                                 (into example
                                       (map (fn [k]
                                              (when-not (example k)
                                                {k :never})))
                                       optional)))))
                     (distinct))
            (comb/subsets optional)))))

(defn -conj-number-constraints [[sol1 & sols :as all-sols]]
  (let [lt (some->> (seq (keep :< all-sols)) (apply min))
        gt (some->> (seq (keep :> all-sols)) (apply max))
        lte (some->> (seq (keep :<= all-sols)) (apply min))
        gte (some->> (seq (keep :>= all-sols)) (apply max))
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
                               [gte :>=])))]
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
          [{}])))))

(defn -conj-solutions [& sols]
  (letfn [(rec [cart-sols]
            (lazy-seq
              (when-some [[[sol1 & sols :as all-sols]] (seq cart-sols)]
                (if (some (some-fn :< :> :<= :>=) all-sols)
                  (concat (-conj-number-constraints all-sols)
                          (rec (rest cart-sols)))
                  (if-some [present (reduce (fn [p1 {p2 :present}]
                                              (reduce-kv (fn [acc k v]
                                                           (if-some [[_ present1] (find acc k)]
                                                             (if (identical? acc v)
                                                               acc
                                                               (reduced (reduced nil)))
                                                             (assoc acc k v)))
                                                         p1 p2))
                                            (:present sol1) sols)]
                    (cons (-> sol1
                              (update :order into (mapcat :order) sols)
                              (assoc :present present))
                          (rec (rest cart-sols)))
                    (rec (rest cart-sols)))))))]
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

(defn- join
  "Lazily concatenates a collection of collections into a flat sequence,
  because Clojure's `apply concat` is insufficiently lazy."
  [colls]
  (lazy-seq
   (when-let [s (seq colls)]
     (concat (first s) (join (rest s))))))

(defn- mapjoin
  "Uses join to achieve lazier version of mapcat (on one collection)"
  [f coll]
  (join (map f coll)))

(defn -constraint-solutions [constraint constraint-opts options]
  (let [{:keys [generator-constraint-types] :as constraint-opts} (mc/->constraint-opts constraint-opts)]
    (letfn [(-constraint-solutions
              ([constraint] (-constraint-solutions constraint options))
              ([constraint options]
               (lazy-seq
                 (if-some [[k] (mc/-contains-constraint-key constraint generator-constraint-types options)]
                   [{:order [k]
                     :present {k true}}]
                   (let [op (mc/-resolve-op constraint generator-constraint-types options)]
                     (case op
                       (:<= :< :>= :>) (let [[n :as all] (subvec constraint 1)]
                                         [{op n}])
                       :not (let [[c] (next constraint)
                                  sol (-constraint-solutions c)]
                              (if (empty? sol)
                                [{:order [] :present {}}]
                                (sequence (comp (map (fn [s]
                                                       (assert (not-any? s [:max-count :min-count]))
                                                       (-> s
                                                           (set/rename-keys {:< :>=
                                                                             :> :<=
                                                                             :<= :>
                                                                             :>= :<})
                                                           (update :present update-vals not))))
                                                (distinct))
                                          (unchunk sol))))
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
                                                         nperm (comb/permutations perm-coll)]
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
                                         (apply interleave
                                                (mapjoin (fn [kset]
                                                           (map #(update base :present into (zipmap kset %))
                                                                (next (comb/selections [false true] (count kset)))))
                                                         (unchunk ksets)))))
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
                       (m/-fail! ::unknown-keyset-constraint {:constraint constraint})))))))]
     (-constraint-solutions constraint))))

(comment
  (assert (= (-constraint-solutions
               [:and :a :b]
               nil)
             '({:order [:a :b], :present {:a true, :b true}})))
  (assert (= (-constraint-solutions
               [:and [:xor :a :c] :b]
               nil)
             '({:order [:a :c :b], :present {:a true, :c false, :b true}}
               {:order [:a :c :b], :present {:a false, :c true, :b true}})))
  (assert (= (-constraint-solutions
               [:or :a :b]
               nil)
             '({:order [:a], :present {:a true}}
               {:order [:b], :present {:b true}}
               {:order [:a :b], :present {:a true, :b true}})))
  (assert (= (-constraint-solutions
               [:or :a :b :c]
               nil)
             '({:order [:a], :present {:a true}}
               {:order [:b], :present {:b true}}
               {:order [:c], :present {:c true}}
               {:order [:b :c], :present {:b true, :c true}}
               {:order [:a :c], :present {:a true, :c true}}
               {:order [:a :b], :present {:a true, :b true}}
               {:order [:a :b :c], :present {:a true, :b true, :c true}})))
  (assert (= (-constraint-solutions
               [:xor :a :b]
               nil)
             '({:order [:a :b], :present {:a true, :b false}}
               {:order [:a :b], :present {:a false, :b true}})))
  (assert (= (-constraint-solutions
               [:disjoint [:a] [:b]]
               nil)
             '({:order [:a :b], :present {:a false, :b false}}
               {:order [:a :b], :present {:a true, :b false}}
               {:order [:a :b], :present {:a false, :b true}})))
  (assert (= (-constraint-solutions
               [:iff :a :b]
               nil)
             '({:order [:a :b], :present {:a false, :b false}}
               {:order [:a :b], :present {:a true, :b true}})))
  (assert (= (-constraint-solutions
               [:implies :a :b]
               nil)
             '({:order [:a], :present {:a false}}
               {:order [:a :b], :present {:a true, :b true}}
               {:order [:b], :present {:b true}}
               {:order [:a :b], :present {:a false, :b true}})))

  (-constraint-solutions [:and
                          [:or :a :b]
                          ;[:or :hint3 [:not :hint3]]
                          [:disjoint [:hint1 :hint2 :hint3]]]
                         nil)
  (-constraint-solutions [:or :a :b]
                         nil)
  (-constraint-solutions [:and
                          [:or :a :b]
                          [:or :hint3 [:not :hint3]]
                          [:disjoint [:hint1 :hint2 :hint3]]
                          ]
                         nil)
  (-constraint-solutions [:and
                          :a
                          :hint3
                          [:disjoint [:hint3]]
                          ]
                         nil)
  (-constraint-solutions [:disjoint [:hint1 :hint2 :hint3]]
                         nil)
  (-constraint-solutions [:and [:disjoint [:hint1 :hint2 :hint3]]]
                         nil)
)

(defn- cycled-nth-fn
  "Given a sequential collection, returns a function
  that returns the nth element of the infinitely cycled collection.
  
  Guarantees to traverse the collection no more than twice per nth,
  converging to once."
  [lazy]
  (let [f (volatile! nil)]
    (vreset! f (fn [i]
                 (loop [count-so-far 0
                        s (seq lazy)]
                   (if s
                     (if (= i count-so-far)
                       (first s)
                       (recur (inc count-so-far) (next s)))
                     (let [single-cycle #(nth lazy (mod % count-so-far))]
                       (vreset! f single-cycle)
                       (single-cycle i))))))
    (fn [i] (@f i))))

(defn- -coll-distinct-gen [schema f options]
  (let [{:keys [min max]} (-min-max schema options)
        constraint (mc/-constraint-from-properties (m/properties schema) (m/type schema) options)
        child (-> schema m/children first)
        child-validator (m/validator child)
        mentioned (some-> constraint
                          (-mentioned-constraint-keys
                            (:generator-constraint-types
                              (mc/->constraint-opts (m/type schema)))
                            options)
                          (->> (into [] (comp (distinct) (filter child-validator)))))
        gen (generator child options)]
    (if (-unreachable-gen? gen)
      (if (= 0 (or min 0))
        (gen/return (f []))
        (-never-gen options))
      (gen/fmap f (if-not constraint
                    (gen/vector-distinct gen {:min-elements min, :max-elements max, :max-tries 100
                                              :ex-fn #(ex-info (str "Could not generate enough distinct elements for schema "
                                                                    (pr-str (m/form schema))
                                                                    ". Consider providing a custom generator.")
                                                               %)})
                    (let [nth-sol (or (some-> (filter #(every? (fn [[k present?]]
                                                                 (or (not present?)
                                                                     (child-validator k)))
                                                               (:present %))
                                                      (-constraint-solutions constraint (m/type schema) options))
                                              seq
                                              cycled-nth-fn)
                                      (m/-fail! ::unsatisfiable-keyset))]
                      (gen/bind gen/nat
                                (fn [i]
                                  (let [{:keys [present]} (nth-sol i)
                                        ;;TODO if count greater than :max, skip solution. might need to bake into -constraint-solutions
                                        base (reduce-kv (fn [acc k v]
                                                          (cond-> acc
                                                            v (conj k)))
                                                        [] present)
                                        base (cond-> base
                                               ;; try and fill in some values if :min needs it
                                               (some->> min (< (count base)))
                                               (into (remove #(contains? present %))
                                                     mentioned))
                                        nbase (count base)
                                        ;; don't generate keys that already exist or are absent in solution
                                        sentinel (Object.)]
                                    (assert (zero? (or max 0)) "TODO")
                                    (gen/bind (if (or min max)
                                                ;; we might be one value short if this generator succeeds.
                                                ;; this is to handle the case where sentinel is never used
                                                ;; and we have enough valid values to complete the keyset.
                                                ;;TODO custom :ex-fn that prints schema
                                                (gen/vector-distinct-by
                                                  (fn [v] (if (contains? present v)
                                                            sentinel
                                                            v))
                                                  gen
                                                  {:min-elements (some-> min (- nbase))
                                                   :max-elements (some-> max (- nbase))
                                                   :max-tries 100})
                                                ;;TODO custom :ex-fn that prints schema
                                                (gen/vector-distinct gen {:max-tries 100}))
                                              (fn [extra-ks]
                                                (let [base (into base (remove #(false? (present %)))
                                                                 extra-ks)]
                                                  (if (some->> min (< (count base)))
                                                    ;; still short, need to generate one more value
                                                    ;;TODO custom :ex-fn that prints schema
                                                    (gen/bind (gen/vector-distinct-by
                                                                (fn [v] (if (or (contains? present v)
                                                                                (contains? base v))
                                                                          sentinel
                                                                          v))
                                                                gen
                                                                {;; last ditch effort to generate a useful value.
                                                                 :min-elements 2
                                                                 :max-elements 2
                                                                 :max-tries 100})
                                                              (fn [extra-ks]
                                                                (let [extra-ks (take 1 (remove #(or (contains? present %)
                                                                                                    (contains? base %))
                                                                                               extra-ks))
                                                                      _ (when (empty? extra-ks)
                                                                          (m/-fail! ::could-not-satisfy-min {:schema schema}))
                                                                      base (into base extra-ks)]
                                                                  (assert (= min (count base)) base)
                                                                  (gen/return base))))
                                                    (gen/return base)))))))))
                    ;;brute force
                    #_
                    (gen/bind (let [;; TODO if only two keys are mentioned in constraints and min is 3, we say it's unsatisfiable.
                                    ;; but we should try to generate more keys from the child.
                                    valid-keysets (when constraint
                                                    (or (not-empty
                                                          (into [] (comp (mapcat #(comb/combinations mentioned %))
                                                                         (map f)
                                                                         (filter #(constraint-validator %)))
                                                                (range (or min 0)
                                                                       (inc (or max (count mentioned))))))
                                                        (m/-fail! ::unsatisfiable-keyset {:schema (m/form schema)})))]
                                (fn [extra]
                                  (gen/bind (gen/tuple gen/nat gen/nat gen/nat gen/nat)
                                            (fn [[i j k]]
                                              (gen/return
                                                (let [base (nth valid-keysets (mod i (count valid-keysets)))
                                                      remaining-mentioned (remove (set base) mentioned)
                                                      candidate (cond-> base
                                                                  (zero? (rem j 3))
                                                                  (into (take (cc/max 0 (if max
                                                                                          (- max (count base))
                                                                                          k)))
                                                                        extra))]
                                                  (if (constraint-validator candidate)
                                                    candidate
                                                    base)))))))))))))

(defn -and-gen [schema options]
  (if-some [gen (-not-unreachable (-> schema (m/children options) first (generator options)))]
    (gen/such-that (m/validator schema options) gen
                   {:max-tries 100
                    :ex-fn #(ex-info (str "Could not generate a value for schema "
                                          (pr-str (m/form schema))
                                          ". Consider providing a custom generator.")
                                     %)})
    (-never-gen options)))

(defn- gen-one-of [gs]
  (if (= 1 (count gs))
    (first gs)
    (gen/one-of gs)))

(defn -or-gen [schema options]
  (if-some [gs (not-empty
                (into [] (keep #(-not-unreachable (generator % options)))
                      (m/children schema options)))]
    (gen-one-of gs)
    (-never-gen options)))

(defn -multi-gen [schema options]
  (if-some [gs (not-empty
                (into [] (keep #(-not-unreachable (generator (last %) options)))
                      (m/entries schema options)))]
    (gen-one-of gs)
    (-never-gen options)))

(defn- -build-map [kvs]
  (persistent!
   (reduce
    (fn [acc [k v]]
      (cond (and (= k ::m/default) (map? v)) (reduce-kv assoc! acc v)
            (nil? k) acc
            :else (assoc! acc k v)))
    (transient {}) kvs)))

(defn- -value-gen [k s options]
  (let [g (generator s options)]
    (cond->> g (-not-unreachable g) (gen/fmap (fn [v] [k v])))))

(defn -map-gen* [schema classify-entry keyset options]
  (loop [[[k s :as e] & entries] (m/entries schema)
         keyset keyset
         gens []]
    (if (nil? e)
      (let [gens (concat gens
                         ;; leftover keys only mentioned in :keyset constraints
                         (keep #(when-not (= :never (val %))
                                  (let [g (-value-gen (key %) :any options)]
                                    (case (val %)
                                      :optional (gen-one-of [nil-gen g])
                                      :required g)))
                               keyset))]
        (gen/fmap -build-map (apply gen/tuple gens)))
      (case (classify-entry e)
        :never (recur entries (dissoc keyset k) gens)
        :optional
        (recur
          entries
          (dissoc keyset k)
          (conj gens
                (if-let [g (-not-unreachable (-value-gen k s options))]
                  (gen-one-of [nil-gen g])
                  nil-gen)))
        :required
        (let [g (-value-gen k s options)]
          (if (-unreachable-gen? g)
            (-never-gen options)
            (recur entries (dissoc keyset k) (conj gens g))))))))

(defn -map-gen-no-keys [schema options]
  (-map-gen* schema
             (fn [e]
               (if (-> e -last m/properties :optional)
                 :optional
                 :required))
             nil
             options))

(defn -map-gen [schema options]
  (if-some [keysets (-valid-map-keysets schema options)]
    (if (empty? keysets)
      (m/-fail! ::unsatisfiable-keys {:schema (m/form schema)})
      (gen/bind gen/nat
                (fn [i]
                  (let [keyset (nth keysets (mod i (count keysets)))]
                    (-map-gen* schema
                               (fn [[k]]
                                 (get keyset k :never))
                               keyset
                               options)))))
    (-map-gen-no-keys schema options)))

(defn -map-of-gen [schema options]
  (let [{:keys [min max]} (-min-max schema options)
        [k-gen v-gen :as gs] (map #(generator % options) (m/children schema options))]
    (if (some -unreachable-gen? gs)
      (if (= 0 (or min 0))
        (gen/return {})
        (-never-gen options))
      (let [opts (-> (cond
                       (and min (= min max)) {:num-elements min}
                       (and min max) {:min-elements min :max-elements max}
                       min {:min-elements min}
                       max {:max-elements max})
                     (assoc :ex-fn #(ex-info (str "Could not generate enough distinct keys for schema "
                                                  (pr-str (m/form schema))
                                                  ". Consider providing a custom generator.")
                                             %)))]
        (gen/fmap #(into {} %) (gen/vector-distinct-by first (gen/tuple k-gen v-gen) opts))))))

#?(:clj
   (defn -re-gen [schema options]
     ;; [com.gfredericks/test.chuck "0.2.10"+]
     (if-let [string-from-regex @(dynaload/dynaload 'com.gfredericks.test.chuck.generators/string-from-regex {:default nil})]
       (let [re (or (first (m/children schema options)) (m/form schema options))]
         (string-from-regex (re-pattern (str/replace (str re) #"^\^?(.*?)(\$?)$" "$1"))))
       (m/-fail! :test-chuck-not-available))))

;; # Approach for recursive generators
;;
;; `-ref-gen` is the only place where recursive generators can be created, and we use `gen/recursive-gen`
;; to handle the recursion. The challenge is that gen/recursive-gen requires _two_ arguments: the base
;; case (scalar gen) and the recursive case (container gen). We need to automatically split the schema argument into
;; these two cases.
;;
;; The main insight we use is that a base case for the schema cannot contain recursive references to itself.
;; A particularly useful base case is simply to "delete" all recursive references. To simulate this, we have the concept of
;; an "unreachable" generator, which represents a "deleted" recursive reference.
;;
;; For infinitely expanding schemas, this will return an unreachable generator--when the base case generator is used,
;; the error message in `-never-gen` will advise users that their schema is infinite.
;; 
;; 
;; Examples of base cases of some recursive schemas:
;;
;; Schema:    [:schema {:registry {::cons [:maybe [:vector [:tuple pos-int? [:ref ::cons]]]]}} ::cons]
;; Base case: [:schema {:registry {::cons [:nil                                            ]}} ::cons]
;;
;; Schema:    [:schema
;;             {:registry {::ping [:tuple [:= "ping"] [:maybe [:ref ::pong]]]
;;                         ::pong [:tuple [:= "pong"] [:maybe [:ref ::ping]]]}}
;;             ::ping]
;; Base case: [:schema
;;             {:registry {::ping [:tuple [:= "ping"] [:maybe [:ref ::pong]]]
;;                         ::pong [:tuple [:= "pong"] :nil                  ]}}
;;             ::ping]
;;
;; Once we have the base case, we first need determine if the schema is recursive---it's recursive
;; if more than one recursive reference was successfully "deleted" while creating the base case (see below for how we determine recursive references).
;; We can then construct the recursive case by providing `gen/recursive-gen` the base case
;; (this is why this particular base case is so useful) and then propagate the (smaller) generator
;; supplied by `gen/recursive-gen` to convert recursive references.

;; ## Identifying schema recursion
;; 
;; Refs are uniquely identified by their paired name and scope. If we see a ref with the
;; same name and scope as another ref we've dereferenced previously, we know that this is a recursion
;; point back to the previously seen ref. The rest of this section explains why.
;; 
;; Refs resolve via dynamic scope, which means its dereferenced value is the latest binding found
;; while expanding the schema until the point of finding the ref.
;; This makes the (runtime) scope at the ref's location part of a ref's identity---if the scope
;; is different, then it's (possibly) not the same ref because scope determines how schemas
;; transitively expand.
;;
;; To illustrate why a ref's name is an insufficient identifier, here is a schema that is equivalent to `[:= 42]`:
;; 
;;   [:schema {:registry {::a [:schema {:registry {::a [:= 42]}}
;;                             ;; (2)
;;                             [:ref ::a]]}}
;;    ;; (1)
;;    [:ref ::a]]
;;
;; If we identify refs just by name, we would have incorrectly detected (2) to be an (infinitely expanding) recursive
;; reference.
;;
;; In studying the previous example, we might think that since (1) and (2) deref to different schemas, it might sufficient to identify refs just by their derefs.
;; Unfortunately this just pushes the problem elsewhere.
;;
;; For example, here is another schema equivalent to `[:= 42]`:
;;
;;   [:schema {:registry {::a [:ref ::b] ;; (2)
;;                        ::b [:schema {:registry {::a [:ref ::b] ;; (4)
;;                                                 ::b [:= 42]}}
;;                             ;; (3)
;;                             [:ref ::a]]}}
;;    ;; (1)
;;    [:ref ::a]]
;;
;; If we identified ::a by its deref, it would look like (3) deref'ing to (4)
;; is a recursion point after witnessing (1) deref'ing to (2), since (2) == (4). Except this
;; is wrong since it's a different ::b at (2) and (4)! OTOH, if we identified (2) and (4) with their
;; dynamic scopes along with their form, they would be clearly different. Indeed, this
;; is another way to identify refs: pairing their derefs with their deref's scopes.
;; It is slightly more direct to use the ref's direct name and scope, which is why
;; we choose that identifier. The more general insight is that any schema is identified by its form+scope
;; (note: but only after trimming the scope of irrelevant bindings, see next pararaph).
;; That insight may be useful for detecting recursion at places other than refs.
;; 
;; Ref identifiers could be made smarter by trimming irrelevant entries in identifying scope.
;; Not all scope differences are relevant, so generators may expand more than strictly necessary
;; in the quest to find the "same" ref schema again. It could skip over refs that generate exactly the
;; same values, but their scopes are uninterestingly different (eg., unused bindings are different).
;;
;; For example, the following schema is recursive "in spirit" between (1) and (2), but since ::b
;; changes, the scope will differ, so the recursion will be detected between (2) and itself instead
;; (where the scope is constant):
;;
;;   [:schema {:registry {::a [:schema {:registry {::b :boolean}}
;;                             ;; (2)
;;                             [:or [:ref ::a] [:ref ::b]]]}}
;;    [:schema {:registry {::b :int}}
;;     ;; (1)
;;     [:or [:ref ::a] [:ref ::b]]]]

(defn- -identify-ref-schema [schema]
  {:scope (-> schema m/-options m/-registry mr/-schemas)
   :name (m/-ref schema)})

(defn -ref-gen [schema options]
  (let [ref-id (-identify-ref-schema schema)]
    (or (force (get-in options [::rec-gen ref-id]))
        (let [scalar-ref-gen (delay (-never-gen options))
              dschema (m/deref schema)]
          (cond->> (generator dschema (assoc-in options [::rec-gen ref-id] scalar-ref-gen))
            (realized? scalar-ref-gen) (gen/recursive-gen
                                        #(generator dschema (assoc-in options [::rec-gen ref-id] %))))))))

(defn -=>-gen [schema options]
  (let [output-generator (generator (:output (m/-function-info schema)) options)]
    (gen/return (m/-instrument {:schema schema} (fn [& _] (generate output-generator options))))))

(defn -function-gen [schema options]
  (gen/return (m/-instrument {:schema schema, :gen #(generate % options)} options)))

(defn -regex-generator [schema options]
  (if (m/-regex-op? schema)
    (generator schema options)
    (let [g (generator schema options)]
      (cond-> g
        (-not-unreachable g) gen/tuple))))

(defn- entry->schema [e] (if (vector? e) (get e 2) e))

(defn -cat-gen [schema options]
  (let [gs (->> (m/children schema options)
                (map #(-regex-generator (entry->schema %) options)))]
    (if (some -unreachable-gen? gs)
      (-never-gen options)
      (->> gs
           (apply gen/tuple)
           (gen/fmap #(apply concat %))))))

(defn -alt-gen [schema options]
  (let [gs (->> (m/children schema options)
                (keep #(-regex-generator (entry->schema %) options)))]
    (if (every? -unreachable-gen? gs)
      (-never-gen options)
      (gen-one-of (into [] (keep -not-unreachable) gs)))))

(defn -?-gen [schema options]
  (let [child (m/-get schema 0 nil)]
    (if-some [g (-not-unreachable (generator child options))]
      (if (m/-regex-op? child)
        (gen/one-of [g (gen/return ())])
        (gen/vector g 0 1))
      (gen/return ()))))

(defn -*-gen [schema options]
  (let [child (m/-get schema 0 nil)
        mode (::-*-gen-mode options :*)
        options (dissoc options ::-*-gen-mode)]
    (if-some [g (-not-unreachable (generator child options))]
      (cond->> (case mode
                 :* (gen/vector g)
                 :+ (gen-vector-min g 1 options))
        (m/-regex-op? child)
        (gen/fmap #(apply concat %)))
      (case mode
        :* (gen/return ())
        :+ (-never-gen options)))))

(defn -+-gen [schema options]
  (-*-gen schema (assoc options ::-*-gen-mode :+)))

(defn -repeat-gen [schema options]
  (let [child (m/-get schema 0 nil)]
    (if-some [g (-not-unreachable (-coll-gen schema identity options))]
      (cond->> g
        (m/-regex-op? child)
        (gen/fmap #(apply concat %)))
      (gen/return ()))))

(defn -qualified-ident-gen [schema mk-value-with-ns value-with-ns-gen-size pred gen]
  (if-let [namespace-unparsed (:namespace (m/properties schema))]
    (gen/fmap (fn [k] (mk-value-with-ns (name namespace-unparsed) (name k))) value-with-ns-gen-size)
    (gen/such-that pred gen {:ex-fn #(ex-info (str "Could not generate a value for schema "
                                                   (pr-str (m/form schema))
                                                   ". Consider providing a custom generator.")
                                              %)})))

(defn -qualified-keyword-gen [schema]
  (-qualified-ident-gen schema keyword gen/keyword qualified-keyword? gen/keyword-ns))

(defn -qualified-symbol-gen [schema]
  (-qualified-ident-gen schema symbol gen/symbol qualified-symbol? gen/symbol-ns))

(defn- gen-elements [es]
  (if (= 1 (count es))
    (gen/return (first es))
    (gen/elements es)))

(defmulti -schema-generator (fn [schema options] (m/type schema options)) :default ::default)

(defmethod -schema-generator ::default [schema options] (ga/gen-for-pred (m/validator schema options)))

(defmethod -schema-generator :> [schema options] (-double-gen {:min (-> schema (m/children options) first inc)}))
(defmethod -schema-generator :>= [schema options] (-double-gen {:min (-> schema (m/children options) first)}))
(defmethod -schema-generator :< [schema options] (-double-gen {:max (-> schema (m/children options) first dec)}))
(defmethod -schema-generator :<= [schema options] (-double-gen {:max (-> schema (m/children options) first)}))
(defmethod -schema-generator := [schema options] (gen/return (first (m/children schema options))))
(defmethod -schema-generator :not= [schema options] (gen/such-that #(not= % (-> schema (m/children options) first)) gen/any-printable
                                                                   {:max-tries 100
                                                                    :ex-fn #(ex-info (str "Could not generate a value for schema "
                                                                                          (pr-str (m/form schema))
                                                                                          ". Consider providing a custom generator.")
                                                                                     %)}))
(defmethod -schema-generator 'pos? [_ _] (gen/one-of [(-double-gen {:min 0.00001}) (gen/fmap inc gen/nat)]))
(defmethod -schema-generator 'neg? [_ _] (gen/one-of [(-double-gen {:max -0.0001}) (gen/fmap (comp dec -) gen/nat)]))

(defmethod -schema-generator :not [schema options] (gen/such-that (m/validator schema options) (ga/gen-for-pred any?)
                                                                  {:max-tries 100
                                                                   :ex-fn #(ex-info (str "Could not generate a value for schema "
                                                                                         (pr-str (m/form schema))
                                                                                         ". Consider providing a custom generator.")
                                                                                    %)}))
(defmethod -schema-generator :and [schema options] (-and-gen schema options))
(defmethod -schema-generator :or [schema options] (-or-gen schema options))
(defmethod -schema-generator :orn [schema options] (-or-gen (m/into-schema :or (m/properties schema) (map last (m/children schema)) (m/options schema)) options))
(defmethod -schema-generator ::m/val [schema options] (generator (first (m/children schema)) options))
(defmethod -schema-generator :map [schema options] (-map-gen schema options))
(defmethod -schema-generator :map-of [schema options] (-map-of-gen schema options))
(defmethod -schema-generator :multi [schema options] (-multi-gen schema options))
(defmethod -schema-generator :vector [schema options] (-coll-gen schema identity options))
(defmethod -schema-generator :sequential [schema options] (-coll-gen schema identity options))
(defmethod -schema-generator :set [schema options] (-coll-distinct-gen schema set options))
(defmethod -schema-generator :enum [schema options] (gen-elements (m/children schema options)))

(defmethod -schema-generator :maybe [schema options]
  (let [g (-> schema (m/children options) first (generator options) -not-unreachable)]
    (gen-one-of (cond-> [nil-gen]
                  g (conj g)))))

(defmethod -schema-generator :tuple [schema options]
  (let [gs (map #(generator % options) (m/children schema options))]
    (if (not-any? -unreachable-gen? gs)
      (apply gen/tuple gs)
      (-never-gen options))))
#?(:clj (defmethod -schema-generator :re [schema options] (-re-gen schema options)))
(defmethod -schema-generator :any [_ _] (ga/gen-for-pred any?))
(defmethod -schema-generator :some [_ _] gen/any-printable)
(defmethod -schema-generator :nil [_ _] nil-gen)
(defmethod -schema-generator :string [schema options] (-string-gen schema options))
(defmethod -schema-generator :int [schema options]
  (let [constraint (mc/-constraint-from-properties (m/properties schema) :int options)
        ;; add generator-specific keys
        constraint (if-some [{:keys [min max]} (not-empty (-min-max schema options))]
                     (cond-> [:and constraint]
                       min (conj [:>= min])
                       max (conj [:<= max]))
                     constraint)
        solutions (some-> (-constraint-solutions constraint :int options)
                          seq -conj-number-constraints)]
    (when (empty? solutions)
      (m/-fail! ::unsatisfiable-int-schema {:schema schema}))
    (gen-one-of
      (mapv (fn [{:keys [<= >= < >]}]
              (let [g (gen/large-integer* {:min (or >= >)
                                           ;;TODO unit test this case
                                           :max (or < <=)})]
                (if (or < >)
                  (gen/such-that (miu/-every-pred
                                   (cond-> []
                                     > (conj #(not (== % >)))
                                     < (conj #(not (== % <)))))
                                 g 100)
                  g)))
            solutions))))
(defmethod -schema-generator :double [schema options]
  (gen/double* (merge (let [props (m/properties schema options)]
                        {:infinite? (get props :gen/infinite? false)
                         :NaN? (get props :gen/NaN? false)})
                      (-min-max schema options))))
(defmethod -schema-generator :boolean [_ _] gen/boolean)
(defmethod -schema-generator :keyword [_ _] gen/keyword)
(defmethod -schema-generator :symbol [_ _] gen/symbol)
(defmethod -schema-generator :qualified-keyword [schema _] (-qualified-keyword-gen schema))
(defmethod -schema-generator :qualified-symbol [schema _] (-qualified-symbol-gen schema))
(defmethod -schema-generator :uuid [_ _] gen/uuid)

(defmethod -schema-generator :=> [schema options] (-=>-gen schema options))
(defmethod -schema-generator :function [schema options] (-function-gen schema options))
(defmethod -schema-generator 'ifn? [_ _] gen/keyword)
(defmethod -schema-generator :ref [schema options] (-ref-gen schema options))
(defmethod -schema-generator :schema [schema options] (generator (m/deref schema) options))
(defmethod -schema-generator ::m/schema [schema options] (generator (m/deref schema) options))

(defmethod -schema-generator :merge [schema options] (generator (m/deref schema) options))
(defmethod -schema-generator :union [schema options] (generator (m/deref schema) options))
(defmethod -schema-generator :select-keys [schema options] (generator (m/deref schema) options))

(defmethod -schema-generator :cat [schema options] (-cat-gen schema options))
(defmethod -schema-generator :catn [schema options] (-cat-gen schema options))
(defmethod -schema-generator :alt [schema options] (-alt-gen schema options))
(defmethod -schema-generator :altn [schema options] (-alt-gen schema options))

(defmethod -schema-generator :? [schema options] (-?-gen schema options))
(defmethod -schema-generator :* [schema options] (-*-gen schema options))
(defmethod -schema-generator :+ [schema options] (-+-gen schema options))
(defmethod -schema-generator :repeat [schema options] (-repeat-gen schema options))

;;
;; Creating a generator by different means, centralized under [[-create]]
;;

(defn- -create-from-return [props]
  (when (contains? props :gen/return)
    (gen/return (:gen/return props))))

(defn- -create-from-elements [props]
  (some-> (:gen/elements props) gen-elements))

(extend-protocol Generator
  #?(:clj Object, :cljs default)
  (-generator [schema options]
    (-schema-generator schema (assoc options ::original-generator-schema schema))))

(defn- -create-from-gen
  [props schema options]
  (or (:gen/gen props)
      (when-not (:gen/elements props)
        (-generator schema options))))

(defn- -create-from-schema [props options]
  (some-> (:gen/schema props) (generator options)))

(defn- -create-from-fmap [props schema options]
  (when-some [fmap (:gen/fmap props)]
    (gen/fmap (m/eval fmap (or options (m/options schema)))
              (or (-create-from-return props)
                  (-create-from-elements props)
                  (-create-from-schema props options)
                  (-create-from-gen props schema options)
                  nil-gen))))

(defn- -create [schema options]
  (let [props (-merge (m/type-properties schema)
                      (m/properties schema))]
    (or (-create-from-fmap props schema options)
        (-create-from-return props)
        (-create-from-elements props)
        (-create-from-schema props options)
        (-create-from-gen props schema options)
        (m/-fail! ::no-generator {:options options
                                  :schema schema}))))

;;
;; public api
;;

(defn generator
  ([?schema]
   (generator ?schema nil))
  ([?schema options]
   (if (::rec-gen options)
     ;; disable cache while calculating recursive schemas. caches don't distinguish options.
     (-create (m/schema ?schema options) options)
     (m/-cached (m/schema ?schema options) :generator #(-create % options)))))

(defn generate
  ([?gen-or-schema]
   (generate ?gen-or-schema nil))
  ([?gen-or-schema {:keys [seed size] :or {size 30} :as options}]
   (let [gen (if (gen/generator? ?gen-or-schema) ?gen-or-schema (generator ?gen-or-schema options))]
     (rose/root (gen/call-gen gen (-random seed) size)))))

(defn sample
  ([?gen-or-schema]
   (sample ?gen-or-schema nil))
  ([?gen-or-schema {:keys [seed size] :or {size 10} :as options}]
   (let [gen (if (gen/generator? ?gen-or-schema) ?gen-or-schema (generator ?gen-or-schema options))]
     (->> (gen/make-size-range-seq size)
          (map #(rose/root (gen/call-gen gen %1 %2))
               (gen/lazy-random-states (-random seed)))
          (take size)))))

;;
;; functions
;;

(defn function-checker
  ([?schema] (function-checker ?schema nil))
  ([?schema {::keys [=>iterations] :or {=>iterations 100} :as options}]
   (let [schema (m/schema ?schema options)
         -try (fn [f] (try [(f) true] (catch #?(:clj Exception, :cljs js/Error) e [e false])))
         check (fn [schema]
                 (let [{:keys [input output guard]} (m/-function-info schema)
                       input-generator (generator input options)
                       valid-output? (m/validator output options)
                       valid-guard? (if guard (m/validator guard options) (constantly true))
                       validate (fn [f args] (as-> (apply f args) $ (and (valid-output? $) (valid-guard? [args $]))))]
                   (fn [f]
                     (let [{:keys [result shrunk]} (->> (prop/for-all* [input-generator] #(validate f %))
                                                        (check/quick-check =>iterations))
                           smallest (-> shrunk :smallest first)]
                       (when-not (true? result)
                         (let [explain-input (m/explain input smallest)
                               [result success] (when-not explain-input (-try (fn [] (apply f smallest))))
                               explain-output (when (and success (not explain-input)) (m/explain output result))
                               explain-guard (when (and success guard (not explain-output)) (m/explain guard [smallest result]))]
                           (cond-> (assoc shrunk ::m/result result)
                             explain-input (assoc ::m/explain-input explain-input)
                             explain-output (assoc ::m/explain-output explain-output)
                             explain-guard (assoc ::m/explain-guard explain-guard)
                             (ex-message result) (-> (update :result ex-message) (dissoc :result-data)))))))))]
     (condp = (m/type schema)
       :=> (check schema)
       :function (let [checkers (map #(function-checker % options) (m/-children schema))]
                   (fn [x] (->> checkers (keep #(% x)) (seq))))
       (m/-fail! ::invalid-function-schema {:type (m/-type schema)})))))

(defn check
  ([?schema f] (check ?schema f nil))
  ([?schema f options]
   (let [schema (m/schema ?schema options)]
     (m/explain (m/-update-options schema #(assoc % ::m/function-checker function-checker)) f))))
