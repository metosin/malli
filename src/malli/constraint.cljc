(ns malli.constraint
  (:require [clojure.set :as set]
            [malli.constraint.keyset :as mc-keys]
            [malli.constraint.number :as mc-num]
            [malli.constraint.sequential :as mc-seq]
            [malli.constraint.string :as mc-str]
            [malli.constraint.atomic.validate :as mcv-atomic]
            [malli.constraint.compound.validate :as mcv-comp]
            [malli.constraint.countable.validate :as mcv-count]
            [malli.constraint.keyset.validate :as mcv-keys]
            [malli.constraint.number.validate :as mcv-num]
            [malli.constraint.seqable.validate :as mcv-seqable]
            [malli.constraint.sequential.validate :as mcv-sequential]
            [malli.constraint.sortable.validate :as mcv-sort]
            [malli.constraint.string.validate :as mcv-str]
            [malli.impl.util :as miu :refer [-fail!]]))

;; TODO :qualified-keyword + :namespace
;; TODO add to options
(defn default-schema-constraints []
  (merge (mc-seq/schema-constraints)
         (mc-str/schema-constraints)
         (mc-num/schema-constraints)
         (mc-keys/schema-constraints)))

;; TODO add to options
(defn default-validators []
  (merge (mcv-atomic/validators)
         (mcv-comp/validators)
         (mcv-count/validators)
         (mcv-keys/validators)
         (mcv-num/validators)
         (mcv-seqable/validators)
         (mcv-sequential/validators)
         (mcv-sort/validators)
         (mcv-str/validators)))

(defn -resolve-op [constraint constraint-types options]
  (let [op (when (vector? constraint)
             (first constraint))
        op (or (get constraint-types op)
               (-fail! ::disallowed-constraint {:type op :constraint constraint
                                                :allowed (keys constraint-types)}))]
    (loop [op op
           seen #{}]
      (assert (not (seen op))
              {:constraint constraint :constraint-types constraint-types
               :seen seen
               :options options})
      (let [op' (get constraint-types op op)]
        (cond-> op'
          (not= op op') (recur (conj seen op)))))))

(defn -resolve-constraint-sugar [constraint {:keys [keyword-sugar]} options]
  (if (keyword? constraint)
    (if keyword-sugar
      (conj [keyword-sugar] constraint)
      ;;FIXME not sure we should do this.
      [constraint])
    constraint))

(defn -contains-constraint-key
  "If :contains is a valid constraint, return its key.
  Recognizes symbol/keyword/string sugar for key."
  [constraint constraint-types options]
  (when (and (vector? constraint)
             (= :contains (-resolve-op constraint constraint-types options))
             (or (= 2 (count constraint))
                 (-fail! ::contains-constraint-takes-one-child {:constraint constraint})))
    (subvec constraint 1)))

(defn ->constraint-opts [type-or-map]
  (if (map? type-or-map)
    type-or-map
    (get (default-schema-constraints) type-or-map)))

(defn -constraint-validator [constraint constraint-opts options]
  (let [{:keys [validator-constraint-types] :as constraint-opts} (->constraint-opts constraint-opts)
        validators (default-validators)]
    (letfn [(-constraint-validator [constraint]
              (let [constraint (-resolve-constraint-sugar constraint constraint-opts options)
                    op (-resolve-op constraint validator-constraint-types options)]
                (if-some [custom-validator (validators op)]
                  (custom-validator {:constraint (if (= :any op) [:any] constraint)
                                     :constraint-opts constraint-opts
                                     ;;TODO other arities
                                     :constraint-validator (fn ([constraint] (-constraint-validator constraint)))}
                                    options)
                  (-fail! ::unknown-constraint {:constraint constraint}))))]
      (-constraint-validator constraint))))

(defn -constraint-from-properties [properties constraint-opts options]
  (let [{:keys [flat-property-keys nested-property-keys]} (->constraint-opts constraint-opts)]
    (when-some [cs (-> []
                       (into (keep #(when-some [[_ v] (find properties %)]
                                      (into [%] v)))
                             nested-property-keys)
                       (into (keep #(when-some [[_ v] (find properties %)]
                                      (conj [%] v)))
                             flat-property-keys)
                       not-empty)]
      (if (= 1 (count cs))
        (first cs)
        (into [:and] cs)))))
