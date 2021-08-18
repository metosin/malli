(ns malli.creation-perf-test
  (:require [criterium.core :as cc]
            [clj-async-profiler.core :as prof]
            [malli.core :as m]
            [malli.util :as mu])
  (:import (java.util.concurrent.atomic AtomicReference AtomicLong)))

(defmacro profile [& body]
  `(let [start# (System/currentTimeMillis)]
     (dotimes [_# 100000] ~@body)
     (let [ms# (- (System/currentTimeMillis) start#)
           times# (int (/ 1000000000 ms#))]
       (printf "invoking %s times\n" times#)
       (time (prof/profile (dotimes [_# times#] ~@body))))))

(comment

  ;;
  ;; validation
  ;;

  ;; 5.2µs
  (cc/quick-bench (m/validate [:or :int :string] 42))
  (profile (m/validate [:or :int :string] 42))

  ;; 3.0µs
  (cc/quick-bench (m/schema [:or :int :string]))
  (profile (m/schema [:or :int :string]))

  ;; 1.7µs
  (let [schema (m/schema [:or :int :string])]
    (cc/quick-bench (m/validator schema))
    #_(profile (m/validator schema)))

  ;; 4ns
  (let [validate (m/validator [:or :int :string])]
    (cc/quick-bench (validate 42))
    #_(profile (validate 42))))

(def ?schema
  [:map
   [:x boolean?]
   [:y {:optional true} int?]
   [:z [:map
        [:x boolean?]
        [:y {:optional true} int?]]]])

(def schema (m/schema ?schema))

(comment

  ;;
  ;; schema creation
  ;;

  ;; 480ns
  (cc/quick-bench (m/schema :int))
  (profile (m/schema :int))

  ;; 44µs
  (cc/quick-bench (m/schema ?schema))
  (profile (m/schema ?schema)))

(comment

  ;;
  ;; schema transformation
  ;;

  ;; 26µs
  (cc/quick-bench (m/walk schema (m/schema-walker identity)))
  (profile (m/walk schema (m/schema-walker identity)))

  ;; 51µs
  (cc/quick-bench (mu/closed-schema schema))
  (profile (mu/closed-schema schema)))

(comment
  (prof/serve-files 8080)
  (prof/clear-results))
