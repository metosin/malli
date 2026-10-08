(ns malli.impl.util
  #?(:lpy (:refer-clojure :exclude [reify]))
  #?(:clj (:import #?(:bb  (clojure.lang MapEntry)
                      :clj (clojure.lang MapEntry LazilyPersistentVector))
                   (java.util.concurrent TimeoutException TimeUnit FutureTask))
     :cljr (:import (clojure.lang MapEntry LazilyPersistentVector))))

;; Basilisp's `reify` requires every method of every protocol to be implemented,
;; only accepts symbols as method parameters, and defines a `-method` as attribute
;; `_method` while protocol dispatch looks up `method`. Malli reifies protocols
;; partially (unimplemented methods are never called) and destructures parameters,
;; so on Basilisp this `reify` fills missing methods with no-op stubs, moves
;; destructuring into a `let`, and aliases each `_method` attribute as `method`.
#?(:lpy
   (defmacro reify [& specs]
     (let [protocol-methods (fn [s] (let [v (when (symbol? s) (resolve s))
                                          p (when v @v)]
                                      (when (and (map? p) (:interface p))
                                        (map name (keys (:methods p))))))
           plain-params (fn [[mname params & body]]
                          (let [params' (mapv #(if (symbol? %) % (gensym "p")) params)
                                binds (mapcat (fn [p p'] (when-not (symbol? p) [p p'])) params params')]
                            (if (seq binds)
                              `(~mname ~params' (let [~@binds] ~@body))
                              `(~mname ~params ~@body))))
           groups (reduce (fn [acc x]
                            (if (symbol? x)
                              (conj acc [x []])
                              (update acc (dec (count acc)) (fn [[s ms]] [s (conj ms (plain-params x))]))))
                          [] specs)
           o (gensym "o")
           c (gensym "c")
           a (gensym "a")]
       `(let [~o (basilisp.core/reify
                   ~@(mapcat (fn [[s ms]]
                               (let [defined (set (map (comp name first) ms))
                                     missing (remove defined (protocol-methods s))]
                                 (concat [s] ms (map (fn [n] `(~(symbol n) [~'this & ~'args] nil)) missing))))
                             groups))
              ~c (python/type ~o)]
          (when-not (python/hasattr ~c "__malli_aliased__")
            (doseq [~a (python/dir ~c)]
              (when (and (.startswith ~a "_")
                         (not (.startswith ~a "__"))
                         (not (python/hasattr ~c (subs ~a 1))))
                (python/setattr ~c (subs ~a 1) (python/getattr ~c ~a))))
            (python/setattr ~c "__malli_aliased__" true))
          ~o))))

(def ^:const +max-size+ #?(:clj Long/MAX_VALUE, :cljs (.-MAX_VALUE js/Number), :cljr Int64/MaxValue, :default 9223372036854775807))

(defn -entry [k v] #?(:clj (MapEntry. k v), :cljs (MapEntry. k v nil), :cljr (MapEntry. k v), :lpy (map-entry k v), :cljrs [k v]))

(defn -invalid? [x] #?(:cljs (keyword-identical? x :malli.core/invalid), :default (identical? x :malli.core/invalid)))
(defn -map-valid [f v] (if (-invalid? v) v (f v)))
(defn -map-invalid [f v] (if (-invalid? v) (f v) v))
(defn -reduce-kv-valid [f init coll] (reduce-kv (comp #(-map-invalid reduced %) f) init coll))

(defn -last [x] (if (vector? x) (peek x) (last x)))
(defn -some [pred coll] (reduce (fn [ret x] (if (pred x) (reduced true) ret)) nil coll))
(defn -merge [m1 m2] (if m1 (persistent! (reduce-kv assoc! (transient m1) m2)) m2))

(defn -error
  ([path in schema value] {:path path, :in in, :schema schema, :value value})
  ([path in schema value type] {:path path, :in in, :schema schema, :value value, :type type}))

(defn -vmap
  ([os] (-vmap identity os))
  ([f os] #?(:clj  (let [c (count os)]
                     (if-not (zero? c)
                       (let [oa (object-array c), iter (.iterator ^Iterable os)]
                         (loop [n 0] (when (.hasNext iter) (aset oa n (f (.next iter))) (recur (unchecked-inc n))))
                         #?(:bb  (vec oa)
                            :clj (LazilyPersistentVector/createOwning oa))) []))
             :cljs (into [] (map f) os)
             :cljr (into [] (map f) os)
             :default (into [] (map f) os))))

#?(:clj
   (defn ^:no-doc -run [^Runnable f ms]
     (let [task (FutureTask. f), t (Thread. task)]
       (try
         (.start t) (.get task ms TimeUnit/MILLISECONDS)
         (catch TimeoutException _ (.cancel task true) ::timeout)
         (catch Exception e (.cancel task true) (throw e))))))

#?(:cljs nil
   :default
   (defmacro -combine-n
     [c n xs]
     (let [syms (repeatedly n gensym)
           g (gensym "preds__")
           bs (interleave syms (map (fn [n] `(nth ~g ~n)) (range n)))
           arg (gensym "arg__")
           body `(~c ~@(map (fn [sym] `(~sym ~arg)) syms))]
       `(let [~g (-vmap ~xs) ~@bs]
          (fn [~arg] ~body)))))

#?(:cljs nil
   :default
   (defmacro -pred-composer
     [c n]
     (let [preds (gensym "preds__")
           f (gensym "f__")
           cases (mapcat (fn [i] [i `(-combine-n ~c ~i ~preds)]) (range 2 (inc n)))
           else `(let [p# (~f (take ~n ~preds)) q# (~f (drop ~n ~preds))]
                   (fn [x#] (~c (p# x#) (q# x#))))]
       `(fn ~f [~preds]
          (case (count ~preds)
            0 (constantly (boolean (~c)))
            1 (first ~preds)
            ~@cases
            ~else)))))

(def ^{:arglists '([[& preds]])} -every-pred
  #?(:clj  (-pred-composer and 16)
     :cljs (fn [preds] (fn [m] (boolean (reduce #(or (%2 m) (reduced false)) true preds))))
     :cljr (-pred-composer and 16)
     :default (fn [preds] (fn [m] (every? #(% m) preds)))))

(def ^{:arglists '([[& preds]])} -some-pred
  #?(:clj  (-pred-composer or 16)
     :cljs (fn [preds] (fn [x] (boolean (some #(% x) preds))))
     :cljr (-pred-composer or 16)
     :default (fn [preds] (fn [x] (boolean (some #(% x) preds))))))

(defmacro predicate-schemas* [var-syms]
  `(-> {}
       ~@(for [vsym var-syms]
           `(malli.core/-register-var '~vsym ~vsym))))
