(ns malli.impl.util
  #?(:clj (:import (java.util.concurrent TimeoutException TimeUnit FutureTask)
                   (clojure.lang MapEntry))))

(def ^:const +max-size+ #?(:clj Long/MAX_VALUE, :cljs (.-MAX_VALUE js/Number)))

(defn -tagged [k v] #?(:clj (MapEntry. k v), :cljs (MapEntry. k v nil)))
(defn -tagged? [v] (instance? MapEntry v))

(defn -invalid? [x] #?(:clj (identical? x :malli.core/invalid), :cljs (keyword-identical? x :malli.core/invalid)))
(defn -map-valid [f v] (if (-invalid? v) v (f v)))
(defn -map-invalid [f v] (if (-invalid? v) (f v) v))

(defrecord SchemaError [path in schema value type message])

(defn -error
  ([path in schema value] (->SchemaError path in schema value nil nil))
  ([path in schema value type] (->SchemaError path in schema value type nil)))

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
       `(let [~g (into [] ~xs) ~@bs]
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

#?(:clj (def ^{:arglists '([[& preds]])} -every-pred (-pred-composer and 16)))
#?(:clj (def ^{:arglists '([[& preds]])} -some-pred (-pred-composer or 16)))

(defn -comp
  ([] identity)
  ([f] f)
  ([f g] (fn [x] (f (g x))))
  ([f g h] (fn [x] (f (g (h x)))))
  ([f1 f2 f3 f4] (fn [x] (-> x f4 f3 f2 f1)))
  ([f1 f2 f3 f4 f5] (fn [x] (-> x f5 f4 f3 f2 f1)))
  ([f1 f2 f3 f4 f5 f6] (fn [x] (-> x f6 f5 f4 f3 f2 f1)))
  ([f1 f2 f3 f4 f5 f6 f7] (fn [x] (-> x f7 f6 f5 f4 f3 f2 f1)))
  ([f1 f2 f3 f4 f5 f6 f7 f8] (fn [x] (-> x f8 f7 f6 f5 f4 f3 f2 f1)))
  ([f1 f2 f3 f4 f5 f6 f7 f8 & fs]
   (-comp
    (apply -comp fs)
    (fn [x] (-> x f8 f7 f6 f5 f4 f3 f2 f1)))))
