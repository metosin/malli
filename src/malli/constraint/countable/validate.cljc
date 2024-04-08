(ns malli.constraint.countable.validate
  (:require [malli.impl.util :as miu]))

(defn- -first-child [f]
  (fn [{:keys [constraint]} _]
    (let [[n :as all] (subvec constraint 1)
          _ (when-not (= 1 (count all))
              (miu/-fail! ::min-max-constraint-takes-one-child {:constraint constraint}))
          _ (when-not (nat-int? n)
              (miu/-fail! ::min-max-constraint-takes-integer {:constraint constraint}))]
      (f n))))

(defn- -count [v]
  (if (string? v)
    (.codePointCount ^String v 0 (count v))
    (count v)))

(defn validators []
  {:max-count (-first-child (fn [n] #(<= (-count %) n)))
   :min-count (-first-child (fn [n] #(<= n (-count %))))})
