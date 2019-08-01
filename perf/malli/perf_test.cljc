(ns malli.perf-test
  (:require [clojure.spec.alpha :as s]
            [criterium.core :as cc]
            [malli.core :as m]))

(s/def ::x boolean?)
(s/def ::y int?)
(s/def ::z string?)

(defn map-perf []

  (let [valid {:x true, :y 1, :z "kikka"}]

    ;; 18ns
    (let [valid? (fn [m]
                   (and (if-let [v (:x m)] (boolean? v) false)
                        (if-let [v (:y m)] (int? v) true)
                        (if-let [v (:z m)] (string? v) false)))]
      (assert (valid? valid))
      (cc/quick-bench
        (valid? valid)))

    ;; 37ns
    (let [valid? (m/validator [:map
                               [:x boolean?]
                               [[:opt :y] int?]
                               [:z string?]])]
      (assert (valid? valid))
      (cc/quick-bench
        (valid? valid)))

    ;; 400ns
    (let [spec (s/keys :req-un [::x ::z] :opt-un [::y])]
      (assert (s/valid? spec valid))
      (cc/quick-bench
        (s/valid? spec valid)))))

(defn composite-perf []

  ;; 3ns
  (let [valid? (fn [x] (and (int? x) (or (pos-int? x) (neg-int? x))))]
    (assert (= [true false true] (map valid? [-1 0 1])))
    (cc/quick-bench
      (valid? 0)))

  ;; 5ns
  (let [valid? (m/validator [:and int? [:or pos-int? neg-int?]])]
    (assert (= [true false true] (map valid? [-1 0 1])))
    (cc/quick-bench
      (valid? 0)))

  ;; 40ns
  (let [spec (s/and int? (s/or :pos-int pos-int? :neg-int neg-int?))]
    (assert (= [true false true] (map (partial s/valid? spec) [-1 0 1])))
    (cc/quick-bench
      (s/valid? spec 0))))

(defn composite-perf2 []
  (let [assert! (fn [f]
                  (doseq [[expected data] [[true [-1]]
                                           [true [-1 1 2]]
                                           [false [-1 0 2]]
                                           [false [-1 -1 -1 -1]]]]
                    (assert (= expected (f data)))))]

    ;; 155ns
    (let [valid? (fn [x]
                   (and (vector? x)
                        (<= (count x) 3)
                        (every? #(and (int? %) (or (pos-int? %) (neg-int? %))) x)))]
      (assert! valid?)
      (cc/quick-bench
        (valid? [-1 1 2])))

    ;; 27ns
    (let [valid? (m/validator
                   [:vector {:max 3}
                    [:and int? [:or pos-int? neg-int?]]])]
      (assert! valid?)
      (cc/quick-bench
        (valid? [-1 1 2])))

    ;; 506ns
    (let [spec (s/coll-of
                 (s/and int? (s/or :pos-int pos-int? :neg-int neg-int?))
                 :kind vector?
                 :max-count 3)
          valid? (partial s/valid? spec)]
      (assert! valid?)
      (cc/quick-bench
        (valid? [-1 1 2])))))

(comment
  (map-perf)
  (composite-perf)
  (composite-perf2))
