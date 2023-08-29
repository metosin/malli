(ns malli.perf.core
  (:require [criterium.core :as cc]
            [clj-memory-meter.core :as mm]
            [clj-async-profiler.core :as prof]))

(defn serve! []
  (with-out-str (prof/serve-files 8080))
  nil)

(defn clear! []
  (prof/clear-results))

(defmacro -bench [& body]
  `(cc/quick-bench ~@body))

(defmacro profile [& body]
  `(let [start# (System/nanoTime)]
     (dotimes [_# 10000] ~@body)
     (let [ns# (- (System/nanoTime) start#)
           times# (long (/ 100000000000000 ns#))]
       (println "invoking" times# "times")
       (time (prof/profile {:event :cpu} (dotimes [_# times#] ~@body))))))

(defmacro bench [& body]
  `(do (serve!) (-bench ~@body) (profile ~@body)))

(def measure mm/measure)

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
