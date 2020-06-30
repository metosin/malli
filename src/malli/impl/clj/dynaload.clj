(ns malli.impl.clj.dynaload
  {:no-doc true})

(defonce ^:private dynalock (Object.))

(defmacro ^:private locking2
  "Executes exprs in an implicit do, while holding the monitor of x.
  Will release the monitor of x in all circumstances."
  {:added "1.0"}
  [x & body]
  `(let [lockee# ~x]
     (try
       (let [locklocal# lockee#]
         (monitor-enter locklocal#)
         (try
           ~@body
           (finally
             (monitor-exit locklocal#)))))))

(defn dynaload
  [s]
  (delay
    (let [ns (namespace s)]
      (assert ns)
      (locking2 dynalock
                (require (symbol ns)))
      (let [v (resolve s)]
        (if v
          @v
          (throw (RuntimeException. (str "Var " s " is not on the classpath"))))))))

(def eval-string
  (dynaload 'sci.core/eval-string))
