(ns malli.impl.util
  #?(:clj (:import (java.util.concurrent TimeoutException TimeUnit FutureTask)
                   (malli.impl NoStackException)
                   (clojure.lang MapEntry))))

#?(:clj
   (do
     (defonce fail-modes #{:default :fast :fast-no-stacktrace})
     (defonce ^:private fail-mode (atom :default))
     (defn set-fail-mode!
       [mode]
       (assert (fail-modes mode))
       (reset! fail-mode mode))

     (defn very-fast-ex-info
       ([msg map]
        (NoStackException. msg map)))

     (defn fast-ex-info
       ([msg map]
        (clojure.lang.ExceptionInfo. msg map)))

     (defn -ex-info
       [msg map]
       (condp identical? @fail-mode
         :default (ex-info msg map)
         :fast (fast-ex-info msg map)
         :fast-no-stacktrace (very-fast-ex-info msg map))))

   :cljs
   (defn -ex-info
     [msg map]
     (ex-info msg map)))

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
