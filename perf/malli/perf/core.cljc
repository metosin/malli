(ns malli.perf.core
  (:require [criterium.core :as cc]
            [clj-async-profiler.core :as prof]))

(defn serve! []
  (with-out-str (prof/serve-ui 8080))
  nil)

(defn clear! []
  (prof/clear-results))

(defmacro -bench [& body]
  `(cc/quick-bench ~@body))

(defmacro profile [& body]
  `(let [start# (System/currentTimeMillis)]
     (dotimes [_# 10000] ~@body)
     (let [ms# (- (System/currentTimeMillis) start#)
           times# (int (/ 100000000 ms#))]
       (println "invoking" times# "times")
       (time (prof/profile (dotimes [_# times#] ~@body))))))

(defmacro bench [& body]
  `(do (serve!) (-bench ~@body) (profile ~@body)))

(comment (clear!))

(defmacro profile-for
  [n & body]
  `(let [res# (cc/quick-benchmark (do ~@body) {})
         mean# (first (:mean res#))
         k# (long (/ ~n mean#))]
     (println (* 1e9 mean#))
     (println
      (with-out-str
        (cc/quick-bench ~@body)))
     (println "Profiling for" ~n "seconds" k# "times")
     (time
      (prof/profile
       (dotimes [_# k#]
         ~@body)))))

(defmacro bench->
  [data & expr]
  `(doseq [[name# data#] ~data]
     (let [mean# (first (:mean (cc/quick-benchmark (-> data# ~expr) {})))
           [scale# unit#] (cc/scale-time mean#)]
       (println name# (* mean# scale#) unit#))))

(comment
 (clear!)
 (serve!))
