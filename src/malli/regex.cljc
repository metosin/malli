(ns malli.regex
  (:refer-clojure :exclude [fn cat repeat ? * +]))

(defprotocol RegexSchema
  (-matcher [_]))

(defprotocol Match
  (-end [_])
  (-result [_])
  (-continue [_]))

(defn -tagged [k v]
  #?(:clj (clojure.lang.MapEntry. k v), :cljs (MapEntry. tag ret nil)))

(defn -parse [rs]
  (reduce
    (clojure.core/fn [[ms im] [i ?kr]]
      (let [[k r] (if (vector? ?kr) [(first ?kr) (second ?kr)] [nil ?kr])]
        [(conj ms (-matcher r)) (cond-> im k (assoc i k))]))
    [[]] (->> rs (vec) (map-indexed vector))))

(defn -static [result end]
  #_(prn "-static" result end)
  (reify Match
    (-end [_] end)
    (-result [_] result)
    (-continue [_])))

(defn fn [f]
  (reify RegexSchema
    (-matcher [_]
      (clojure.core/fn [data size start]
        #_(prn "FN:" data start f)
        (if (< -1 start size)
          (let [x (nth data start)]
            (if (f x) (-static x start))))))))

(defn lazy [f]
  (reify RegexSchema
    (-matcher [_]
      (clojure.core/fn [data size start]
        ((-matcher (f)) data size start)))))

(defn alt [& rs]
  (let [[matchers i->k] (-parse rs)
        matchers-size (count matchers)
        -key (or i->k identity)
        -value (clojure.core/fn -value [res i f]
                 (reify Match
                   (-end [_] (-end res))
                   (-result [_] (-tagged (-key i) (-result res)))
                   (-continue [_] (f (inc i)))))
        -continue (clojure.core/fn -continue [data size start i]
                    (loop [i i]
                      (if-not (= i matchers-size)
                        (let [m (nth matchers i)]
                          (or (some-> (m data size start) (-value i (partial -continue data size start)))
                              (recur (inc i)))))))]
    (reify RegexSchema
      (-matcher [_] (clojure.core/fn [data size start] (-continue data size start 0))))))

(defn cat [& rs]
  (let [[matchers i->k] (-parse rs)
        -result (if i->k #(->> % (map (clojure.core/fn [[k v]] [(i->k k) (-result v)])) (into {}))
                         #(->> % (mapv (comp -result val))))
        matchers-size (count matchers)
        -value (clojure.core/fn -value [m i]
                 (reify Match
                   (-end [_] (-end (m i)))
                   (-result [_] (-result m))
                   (-continue [_]
                     (loop [si i]
                       (when-not (neg? si)
                         (if-let [sr (some-> si m -continue)]
                           (-value (assoc m si sr) i)
                           (recur (dec si))))))))]
    (reify RegexSchema
      (-matcher [_]
        (clojure.core/fn [data size start]
          (loop [i 0, splits [start], results {}]
            #_(prn "CAT:" (vals i->k) i splits results)
            (cond
              (neg? i) nil
              (results i) (if-let [res (-continue (results i))]
                            (recur (inc i) (conj splits (inc (-end res))) (assoc results i res))
                            (recur (dec i) (pop splits) (dissoc results i)))
              (>= i matchers-size) (-value results (dec i))
              :else (do
                      #_(prn "looking..." ((nth matchers i) data size (nth splits i)))
                      (if-let [res ((nth matchers i) data size (nth splits i))]
                        (recur (inc i) (conj splits (inc (-end res))) (assoc results i res))
                        (if (pos? i) (recur (dec i) (pop splits) (dissoc results i))))))))))))

(defn repeat [min max r]
  (let [child-matcher (-matcher r)
        -value (clojure.core/fn -value [v i]
                 (reify Match
                   (-end [_] (-end (v i)))
                   (-result [_] (mapv -result v))
                   (-continue [this]
                     (loop [si i]
                       (when-not (neg? si)
                         (let [sr (some-> si v -continue)]
                           (cond
                             sr (-value (assoc v si sr) (dec i))
                             (= i min 0) (-static [] (dec (-end this)))
                             (>= i min) (-value (pop v) (dec i))
                             :else (recur (dec si)))))))))]
    (reify RegexSchema
      (-matcher [_]
        (clojure.core/fn [data size start]
          (loop [i 0, splits [start], results []]
            #_(prn "REP:" min max i data splits results)
            (cond
              (contains? results i) (if-let [res (-continue (results i))]
                                      (recur (inc i) (conj splits (inc (-end res))) (assoc results i res))
                                      (recur (dec i) (pop splits) (pop results)))
              (>= i max) (-value results (dec i))
              (and (>= i min) (= (last splits) size)) (if (zero? i) (-static results (dec size)) (-value results (dec i)))
              (>= i 0) (do
                         #_(prn "rep..." (nth splits i) (child-matcher data size (nth splits i)))
                         (let [res (child-matcher data size (nth splits i))]
                           (cond
                             res (recur (inc i) (conj splits (inc (-end res))) (assoc results i res))
                             ;(zero? i) nil
                             (>= i min) (if (zero? i) (-static results (dec (nth splits i))) (-value results (dec i)))
                             (pos? i) (recur (dec i) (pop splits) (if (contains? results i) (pop results) results))))))))))))


(defn ? [r] (repeat 0 1 r))
(defn * [r] (repeat 0 ##Inf r))
(defn + [r] (repeat 1 ##Inf r))

(defn validator [re]
  (let [matcher (-matcher re)]
    (clojure.core/fn [x]
      (if (sequential? x)
        (let [x (vec x), size (count x)]
          (boolean (some-> (matcher x size 0) (-end) (= (dec size)))))))))

(defn validate [re data] ((validator re) data))

(defn parser [re]
  (let [matcher (-matcher re)]
    (clojure.core/fn [x]
      (if (sequential? x)
        (let [x (vec x)
              size (count x)
              result (matcher x size 0)]
          (if (and result (= (-end result) (dec size)))
            (-result result)))))))

(defn parse [re data] ((parser re) data))

(defn describer [re]
  (let [matcher (-matcher re)]
    (clojure.core/fn [x]
      (let [x (vec x)]
        (if-let [m (matcher x (count x) 0)]
          {:result (-result m)
           :rest (subvec x (inc (-end m)))})))))

(defn describe [re data] ((describer re) data))
