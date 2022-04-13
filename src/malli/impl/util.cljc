(ns malli.impl.util
  #?(:clj (:import (clojure.lang MapEntry LazilyPersistentVector)
                   (java.util.concurrent TimeoutException TimeUnit FutureTask))))

(def ^:const +max-size+ #?(:clj Long/MAX_VALUE, :cljs (.-MAX_VALUE js/Number)))

(defn -tagged [k v] #?(:clj (MapEntry. k v), :cljs (MapEntry. k v nil)))
(defn -tagged? [v] (instance? MapEntry v))

(defn -invalid? [x] #?(:clj (identical? x :malli.core/invalid), :cljs (keyword-identical? x :malli.core/invalid)))
(defn -map-valid [f v] (if (-invalid? v) v (f v)))
(defn -map-invalid [f v] (if (-invalid? v) (f v) v))
(defn -reduce-kv-valid [f init coll] (reduce-kv (comp #(-map-invalid reduced %) f) init coll))

(defn -error
  ([path in schema value] {:path path, :in in, :schema schema, :value value})
  ([path in schema value type] {:path path, :in in, :schema schema, :value value, :type type}))

(defn -vmap
  ([os] (-vmap identity os))
  ([f os] #?(:clj  (let [c (count os)]
                     (if-not (zero? c)
                       (let [oa (object-array c), iter (.iterator ^Iterable os)]
                         (loop [n 0] (when (.hasNext iter) (aset oa n (f (.next iter))) (recur (unchecked-inc n))))
                         (LazilyPersistentVector/createOwning oa)) []))
             :cljs (into [] (map f) os))))

#?(:clj
   (defn ^:no-doc -run [^Runnable f ms]
     (let [task (FutureTask. f), t (Thread. task)]
       (try
         (.start t) (.get task ms TimeUnit/MILLISECONDS)
         (catch TimeoutException _ (.cancel task true) (.stop t) ::timeout)
         (catch Exception e (.cancel task true) (.stop t) (throw e))))))

#?(:clj
   (defmacro -combine-n
     [c n xs]
     (let [syms (repeatedly n gensym)
           g (gensym "preds__")
           bs (interleave syms (map (fn [n] `(nth ~g ~n)) (range n)))
           arg (gensym "arg__")
           body `(~c ~@(map (fn [sym] `(~sym ~arg)) syms))]
       `(let [~g (-vmap ~xs) ~@bs]
          (fn [~arg] ~body)))))

#?(:clj
   (defmacro -pred-composer
     [c n]
     (let [preds (gensym "preds__")
           f (gensym "f__")
           cases (mapcat (fn [i] [i `(-combine-n ~c ~i ~preds)]) (range 2 (inc n)))
           else `(let [p# (~f (take ~n ~preds)) q# (~f (drop ~n ~preds))]
                   (fn [x#] (~c (p# x#) (q# x#))))]
       `(fn ~f [~preds]
          (case (count ~preds)
            0 (constantly true)
            1 (first ~preds)
            ~@cases
            ~else)))))

(def ^{:arglists '([[& preds]])} -every-pred
  #?(:clj (-pred-composer and 16)
     :cljs (fn [preds] (fn [m] (boolean (reduce #(or (%2 m) (reduced false)) true preds))))))

(def ^{:arglists '([[& preds]])} -some-pred
  #?(:clj (-pred-composer or 16)
     :cljs (fn [preds] (fn [x] (boolean (some #(% x) preds))))))
