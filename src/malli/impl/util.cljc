(ns malli.impl.util
  #?(:clj (:import (java.util.concurrent TimeoutException TimeUnit FutureTask)
                   (malli.impl NoStackException)
                   (clojure.lang MapEntry))))

#?(:clj
   (defn no-stack-ex-info
     "Allocate an exception without a stack trace."
     [msg map]
     (NoStackException. msg map)))

#?(:clj
   (defn raw-ex-info
     "Like [[clojure.core/ex-info]] but does not elide top frames."
     [msg map]
     (clojure.lang.ExceptionInfo. msg map)))

#?(:clj (def +ex-info+ (atom ex-info)))

#?(:clj
   (defn set-ex-info!
     "Set ex-info function to one of three modes:
  - default: [[clojure.core/ex-info]]
  - raw: [[raw-ex-info]], does not elide top frames
  - no-stacktrace: [[no-stack-ex-info]], fastest, does not create stack trace."
     [mode]
     (reset!
      +ex-info+
      (case mode
        :default ex-info
        :raw raw-ex-info
        :no-stacktrace no-stack-ex-info))))

(defn -ex-info
  [msg map]
  (#?(:clj @+ex-info+ :cljs ex-info) msg map))

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
