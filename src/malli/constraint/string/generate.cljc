(ns malli.constraint.string.generate
  (:require [clojure.test.check.generators :as gen]
            [clojure.string :as str]
            [malli.impl.util :as miu]))

(defn- -string-class [k]
  (fn [{:keys [constraint]} options]
    (let [[s] (next constraint)]
      (assert (not (false? s)))
      [{:string-class {k #{}}}])))

(defn solvers []
  {:alpha-string (-string-class :alpha)
   :non-alpha-string (-string-class :non-alpha)
   :numeric-string (-string-class :numeric)
   :non-numeric-string (-string-class :non-numeric)
   :alphanumeric-string (-string-class :alphanumeric)
   :edn-string (fn [{:keys [constraint]} options]
                 (let [[s] (next constraint)]
                   (assert (not (false? s)))
                   [{:string-class {:edn (cond-> #{}
                                           s (conj s))}}]))
   :includes-string (fn [{:keys [constraint]} options]
                      (let [[s] (next constraint)]
                        [{:string-class {:includes #{s}}}]))})

;; TODO calculate compatibility with set intersection
(defn string-class->char-codes [cls]
  {:pre [(vector? cls)]}
  (case (first cls)
    :alphanumeric (concat (string-class->char-codes [:numeric])
                          (string-class->char-codes [:alpha]))
    :alpha [{:min (int \A) :max (int \Z)}
            {:min (int \a) :max (int \z)}]
    :numeric [{:min (int \0) :max (int \9)}]))

(defn conj-string-class-solutions [all-sols]
  (if-some [string-classes (seq (keep :string-class all-sols))] 
    (let [sc (apply merge-with into string-classes)
          sc (cond-> sc
               ;; numeric/alpha subsumes alphanumeric
               (and (:alphanumeric sc)
                    (or (:numeric sc)
                        (:alpha sc)))
               (dissoc :alphanumeric))]
      (if (and (:numeric sc)
               (:alpha sc))
        ;; unsatisfiable
        []
        [{:string-class sc}]))
    [{}]))

(defn negate-string-class [[cls args]]
  (assert (keyword? cls) (pr-str cls))
  (assert (set? args) (pr-str args))
  (or (when (empty? args)
        (when-some [flip (case cls
                           :alpha :not-alpha
                           :not-alpha :alpha
                           :non-alpha :not-non-alpha
                           :not-non-alpha :non-alpha
                           :numeric :not-numeric
                           :not-numeric :numeric
                           :non-numeric :not-non-numeric
                           :not-non-numeric :non-numeric
                           :alphanumeric :not-alphanumeric
                           :not-alphanumeric :alphanumeric
                           nil)]
          {flip #{}}))
      (miu/-fail! ::unsupported-negated-class
                  {:string-class cls})))

(defn generators []
  {})
