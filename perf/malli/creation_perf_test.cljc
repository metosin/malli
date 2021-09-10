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
  ;; 3.6µs
  ;; 3.0µs (map childs)
  ;; 3.2µs (mapv childs)
  ;; 2.5µs (...)
  (bench (m/validate [:or :int :string] 42))
  (profile (m/validate [:or :int :string] 42))

  ;; 3.0µs
  ;; 500ns (delayed mapv childs)
  ;; 1.7µs
  ;; 510ns (map childs)
  ;; 310ns (schema)
  ;; 300ns (simple-schema)
  ;; 180ns (fast parse)
  ;; 1.1µs (mapv childs)
  ;; 750ns (...)
  (bench (m/schema [:or :int :string]))
  (profile (m/schema [:or :int :string]))

  ;; 1.7µs
  ;; 470ns (map childs)
  ;; 310ns (schema)
  ;; 300ns (simple-schema)
  ;; 190ns (fast parse)
  ;; 1.1µs (mapv childs)
  ;; 750ns (...)
  (bench (m/schema [:and :int :string]))
  (profile (m/schema [:and :int :string]))

  ;; 1.7µs
  ;; 1.5µs (fast parse)
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

  ;; 480ns -> 400ns -> 340ns -> 280ns -> 240ns -> 170ns (registry) -> 160ns (recur)
  (bench (m/schema :int))
  (profile (m/schema :int))

  ;; 44µs -> 31µs -> 18µs -> 11µs -> 9.4µs -> 9.0µs -> 8.5µs -> 7.0µs -> 6.4µs (registry) -> 5.7µs
  (bench (m/schema ?schema))
  (profile (m/schema ?schema))

  ;; does not work with direct linking
  (with-redefs [m/-check-children? (constantly false)]
    (bench (m/schema ?schema))
    (profile (m/schema ?schema))))

(def ref-schema (m/schema [:schema :int]))

(comment

  ;; 14ns -> 5ns
  (bench (m/deref ref-schema))
  (profile (m/deref ref-schema))

  ;; 5µs -> 28ns
  (bench (m/deref-all ref-schema))
  (profile (m/deref-all ref-schema)))

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
  ;; 1.2µs (protocols, registry, recur)
  (bench (m/walk schema (m/schema-walker identity)))
  (profile (m/walk schema (m/schema-walker identity)))

  ;; 51µs
  ;; 44µs (-set-children, -set-properties)
  ;; 29µs (lot's of stuff)
  ;; 21µs (faster parsing)
  ;; 7.5µs (ever faster parsing)
  ;; 7.2µs (compact parsing)
  ;; 6.5µs (schema)
  ;; 5.8µs (protocols, registry, recur, parsed)
  (bench (mu/closed-schema schema))
  (profile (mu/closed-schema schema))

  ;; 3.8µs
  ;; 3.4µs (satisfies?)
  ;; 2.2µs (-set-entries)
  ;; 830ns (-update-parsed)
  (bench (mu/assoc schema :y :string))
  (profile (mu/assoc schema :y :string))

  ;; 4.2µs
  ;; 3.8µs (satisfies?)
  ;; 820ns (-update-parsed)
  (bench (mu/assoc schema :w :string))
  (profile (mu/assoc schema :w :string))

  ;; 205ns
  ;; 195ns
  (bench (mu/get schema :y))
  (profile (mu/get schema :y))

  ;; 13µs
  ;; 2.4µs (satisfies?)
  (bench (mu/required-keys schema))

  ;; 134µs
  ;; 15µs (satisfies?)
  (bench (mu/merge schema schema)))

(comment

  (let [t ::or, p {:a 1}, c (mapv m/schema [:int :int])]
    ;; 480ns
    ;; 221ns (faster impl)
    (bench (m/-create-form t p c))))

(comment
  (let [s (m/schema :int)]
    ;; 440ns
    ;; 341ns (-create-form)
    ;; 150ns (delayed form)
    ;;  30ns (don't -check-children)
    (bench (m/-val-schema s nil))
    (profile (m/-val-schema s nil))))

(comment
  (prof/serve-files 8080)
  (prof/clear-results))

(comment
  "clojurescript perf tests"

  ; shadow-cljs browser-repl

  (require '[malli.core :as m])
  (require '[malli.util :as mu])

  (def ?schema
    [:map
     [:x boolean?]
     [:y {:optional true} int?]
     [:z [:map
          [:x boolean?]
          [:y {:optional true} int?]]]])

  (def schema (m/schema ?schema))

  (def ref-schema (m/schema [:schema :int]))

  ;;
  ;; benchmarks (0.6.1 vs LATEST)
  ;;

  (simple-benchmark [] (m/schema :int) 100000)
  ; [], (m/schema :int), 100000 runs, 181 msecs
  ; [], (m/schema :int), 100000 runs, 55 msecs (3x)

  (simple-benchmark [] (m/schema [:or :int :string]) 100000)
  ; [], (m/schema [:or :int :string]), 100000 runs, 654 msecs
  ; [], (m/schema [:or :int :string]), 100000 runs, 185 msecs (4x)

  (simple-benchmark [] (m/schema ?schema) 10000)
  ; [], (m/schema ?schema), 10000 runs, 896 msecs
  ; [], (m/schema ?schema), 10000 runs, 156 msecs (6x)

  (simple-benchmark [] (m/walk schema (m/schema-walker identity)) 10000)
  ; [], (m/walk schema (m/schema-walker identity)), 10000 runs, 544 msecs
  ; [], (m/walk schema (m/schema-walker identity)), 10000 runs, 41 msecs (13x)

  (simple-benchmark [] (mu/closed-schema schema) 10000)
  ; [], (mu/closed-schema schema), 10000 runs, 1046 msecs
  ; [], (mu/closed-schema schema), 10000 runs, 163 msecs (6x)

  (simple-benchmark [] (m/deref ref-schema) 1000000)
  ; [], (m/deref ref-schema), 1000000 runs, 53 msecs
  ; [], (m/deref ref-schema), 1000000 runs, 53 msecs

  (simple-benchmark [] (m/deref-all ref-schema) 1000000)
  ; [], (m/deref-all ref-schema), 1000000 runs, 104 msecs
  ; [], (m/deref-all ref-schema), 1000000 runs, 55 msecs
  )
