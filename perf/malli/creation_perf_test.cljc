(ns malli.creation-perf-test
  (:require [criterium.core :as cc]
            [clj-async-profiler.core :as prof]
            [malli.core :as m]
            [malli.util :as mu]))

(defmacro bench [& body]
  `(cc/quick-bench ~@body))

(defmacro profile [& body]
  `(let [start# (System/currentTimeMillis)]
     (dotimes [_# 100000] ~@body)
     (let [ms# (- (System/currentTimeMillis) start#)
           times# (int (/ 1000000000 ms#))]
       (println "invoking" times# "times")
       (time (prof/profile (dotimes [_# times#] ~@body))))))

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

  (bench->
   [[:or-2-types [:or :int :string]]
    [:int :int]]
   m/schema))

(comment

  ;;
  ;; validation
  ;;

  ;; 5.2µs
  (bench (m/validate [:or :int :string] 42))
  (profile (m/validate [:or :int :string] 42))

  ;; 3.0µs
  (bench (m/schema [:or :int :string]))
  (profile (m/schema [:or :int :string]))

  ;; 1.7µs
  (let [schema (m/schema [:or :int :string])]
    (bench (m/validator schema))
    #_(profile (m/validator schema)))

  ;; 4ns
  (let [validate (m/validator [:or :int :string])]
    (bench (validate 42))
    #_(profile (validate 42))))

(def ?schema
  [:map
   [:x boolean?]
   [:y {:optional true} int?]
   [:z [:map
        [:x boolean?]
        [:y {:optional true} int?]]]])

(def schema (m/schema ?schema))

(def leaf-schema (m/schema :int))

(comment

  ;;
  ;; schema creation
  ;;

  ;; 480ns
  (bench (m/schema :int))
  (profile (m/schema :int))

  ;; 44µs
  (bench (m/schema ?schema))
  (profile (m/schema ?schema)))

(comment

  ;;
  ;; schema transformation
  ;;

  ;; 271ns
  ;; 14ns (-set-children, -set-properties)
  (bench (m/walk leaf-schema (m/schema-walker identity)))
  (profile (m/walk leaf-schema (m/schema-walker identity)))

  ;; 26µs
  ;; 1.3µs (-set-children, -set-properties)
  (bench (m/walk schema (m/schema-walker identity)))
  (profile (m/walk schema (m/schema-walker identity)))

  ;; 51µs
  ;; 44µs (-set-children, -set-properties)
  (bench (mu/closed-schema schema))
  (profile (mu/closed-schema schema)))

(comment

  (let [t ::or, p {:a 1}, c (mapv m/schema [:int :int])]
    ;; 480ns
    ;; 221ns (faster impl)
    (bench (m/-create-form t p c))))

(comment
  (prof/serve-files 8080)
  (prof/clear-results))
