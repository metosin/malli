(ns malli.perf.creation-perf-test
  (:require [malli.perf.core :as p]
            [malli.core :as m]
            [malli.generator :as mg]
            [malli.util :as mu]))

(comment

  ;;
  ;; validation
  ;;

  ;; 5.2µs
  ;; 3.6µs
  ;; 3.0µs (map childs)
  ;; 3.2µs (mapv childs)
  ;; 2.5µs (...)
  ;; 2.3µs (-vmap, don't check children)
  ;; 1.1µs
  ;; 270ns (M1-JDK17)
  (p/bench (m/validate [:or :int :string] 42))

  ;; 2.6µs
  ;; 1.3µs
  ;; 380ns (M1-JDK17)
  (p/bench (m/validate (m/from-ast {:type :or, :children [{:type :int} {:type :string}]}) 42))

  ;; 15ns
  ;; 7.2ns (M1-JDK17)
  (let [schema (m/schema [:or :int :string])]
    (p/bench (m/validate schema 42)))

  ;; 3.0µs
  ;; 500ns (delayed mapv childs)
  ;; 1.7µs
  ;; 510ns (map childs)
  ;; 310ns (schema)
  ;; 300ns (simple-schema)
  ;; 180ns (fast parse)
  ;; 1.1µs (mapv childs)
  ;; 750ns (...)
  ;; 680ns (-vmap, don't check children)
  ;; 180ns (M1-JDK17)
  ;; 160ns (-type-cache)
  (p/bench (m/schema [:or :int :string]))

  ;; 730ns
  ;; 230ns (M1-JDK17)
  (p/bench (m/from-ast {:type :or, :children [{:type :int} {:type :string}]}))

  ;; 1.7µs
  ;; 470ns (map childs)
  ;; 310ns (schema)
  ;; 300ns (simple-schema)
  ;; 190ns (fast parse)
  ;; 1.1µs (mapv childs)
  ;; 750ns (...)
  ;; 680ns (-vmap, don't check children)
  ;; 190ns (M1-JDK17)
  ;; 160ns (-type-cache)
  (p/bench (m/schema [:and :int :string]))

  ;; 730ns
  ;; 240ns (M1-JDK17)
  (p/bench (m/from-ast {:type :and, :children [{:type :int} {:type :string}]}))

  ;; 1.7µs
  ;; 1.5µs (fast parse)
  ;; 540ns (non-distinct)
  ;; 13ns (-cache)
  ;; 6.5ns (M1-JDK17)
  (let [schema (m/schema [:or :int :string])]
    (p/bench (m/validator schema)))

  ;; 16ns
  ;; 7.0ns (M1-JDK17)
  (let [schema (m/schema [:or :int :string])]
    (p/bench (m/validate schema 42)))

  ;; 3ns
  ;; 2.3ns (M1-JDK17)
  (let [validate (m/validator [:or :int :string])]
    (p/bench (validate 42))))

(def ?schema
  [:map
   [:x boolean?]
   [:y {:optional true} int?]
   [:z [:map
        [:x boolean?]
        [:y {:optional true} int?]]]])

(def schema (m/schema ?schema))

(def ast (m/ast ?schema))

(def leaf-schema (m/schema :int))

(comment

  ;;
  ;; schema creation
  ;;

  ;; 480ns -> 400ns -> 340ns -> 280ns -> 240ns -> 170ns (registry) -> 160ns (recur)
  ;; 55ns (M1-JDK17)
  ;; 34ns (-type-cache)
  (p/bench (m/schema :int))

  ;; 180ns
  ;; 78ns (M1-JDK17)
  (p/bench (m/from-ast {:type :int}))

  ;; 44µs -> 31µs -> 18µs -> 11µs -> 9.4µs -> 9.0µs -> 8.5µs -> 7.0µs -> 6.4µs (registry) -> 5.7µs
  ;; 3.4µs
  ;; 2.9µs (-entry-parser)
  ;; 2.5µs (no entries, object-arraus)
  ;; 1.0µs (M1-JDK17)
  ;; 840ns (-type-cache)
  (p/bench (m/schema ?schema))

  ;; 44µs -> 240ns
  ;; 110ns (M1-JDK17)
  ;;  95ns (-type-cache)
  (p/bench (m/schema ?schema {::m/lazy-entries true}))

  ;; 147ns
  ;; 84ns (M1-JDK17)
  (p/bench (m/from-ast ast))

  ;; 3.7µs
  ;; 1.4µs (M1-JDK17)
  ;; 1.3ns (-type-cache)
  (p/bench (m/validator (m/schema ?schema)))

  ;; 2.5µs
  ;; 900ns (M1-JDK17)
  (p/bench (m/validator (m/from-ast ast)))

  ;; 1.6µs -> 64ns
  ;; 40ns (M1-JDK17)
  (p/bench (m/validate schema {:x true, :z {:x true}}))

  ;; 1.6µs -> 450ns
  ;; 120ns (M1-JDK17)
  (p/bench (m/explain schema {:x true, :z {:x true}}))

  ;; does not work with direct linking
  (with-redefs [m/-check-children? (constantly false)]
    (p/bench (m/schema ?schema))))

(def ref-schema (m/schema [:schema :int]))

(comment

  ;; 14ns -> 5ns
  ;; 3.3ns (M1-JDK17)
  (p/bench (m/deref ref-schema))

  ;; 5µs -> 28ns
  ;; 10ns (M1-JDK17)
  (p/bench (m/deref-all ref-schema)))

(comment

  ;;
  ;; schema transformation
  ;;

  ;; 271ns
  ;; 14ns (-set-children, -set-properties)
  ;; 12ns (-entry-parser)
  ;; 7.2ns (M1-JDK17)
  (p/bench (m/walk leaf-schema (m/schema-walker identity)))

  ;; 26µs
  ;; 1.3µs (-set-children, -set-properties)
  ;; 1.2µs (protocols, registry, recur)
  ;; 700ns (M1-JDK17)
  (p/bench (m/walk schema (m/schema-walker identity)))

  ;; 51µs
  ;; 44µs (-set-children, -set-properties)
  ;; 29µs (lot's of stuff)
  ;; 21µs (faster parsing)
  ;; 7.5µs (ever faster parsing)
  ;; 7.2µs (compact parsing)
  ;; 6.5µs (schema)
  ;; 5.8µs (protocols, registry, recur, parsed)
  ;; 3.9µs (-parsed)
  ;; 3.6µs (-entry-parser)
  ;; 3.4µs (object-array)
  ;; 1.4µs (M1-JDK17)
  (p/bench (mu/closed-schema schema))

  ;; 3.8µs
  ;; 3.4µs (satisfies?)
  ;; 2.2µs (-set-entries)
  ;; 830ns (-update-parsed)
  ;; 560ns (-entry-parser)
  ;; 190ns (M1-JDK17)
  (p/bench (mu/assoc schema :y :string))

  ;; 4.2µs
  ;; 3.8µs (satisfies?)
  ;; 820ns (-update-parsed)
  ;; 540ns (-entry-parser)
  ;; 180ns (M1-JDK17)
  (p/bench (mu/assoc schema :w :string))

  ;; 205ns
  ;; 195ns
  ;;  73ns (M1-JDK17)
  (p/bench (mu/get schema :y))

  ;; 13µs
  ;; 2.4µs (satisfies?)
  ;; 1.8µs
  ;; 690ns (M1-JDK17)
  (p/bench (mu/required-keys schema))

  ;; 134µs
  ;; 15µs (satisfies?)
  ;;  9µs (fast merge)
  ;; 2.7µs (M1-JDK17)
  (p/bench (mu/merge schema schema)))

(comment
  ;; 119µs
  ;; 16µs (cache generator)
  ;; 4.6µs (M1-JDK17)
  (p/bench (mg/generate schema)))

(comment

  (let [t ::or, p {:a 1}, c (mapv m/schema [:int :int])]
    ;; 480ns
    ;; 221ns (faster impl)
    ;; 120ns (M1-JDK17)
    (p/bench (m/-create-form t p c nil))))

(comment
  (let [s (m/schema :int)]
    ;; 440ns
    ;; 341ns (-create-form)
    ;; 150ns (delayed form)
    ;;  30ns (don't -check-children)
    ;; 120ns (M1-JDK17)
    (p/bench (m/-val-schema s nil))))

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
  ; [], (m/schema ?schema), 10000 runs, 94 msecs (9.5x)

  (simple-benchmark [] (m/walk schema (m/schema-walker identity)) 10000)
  ; [], (m/walk schema (m/schema-walker identity)), 10000 runs, 544 msecs
  ; [], (m/walk schema (m/schema-walker identity)), 10000 runs, 41 msecs (13x)

  (simple-benchmark [] (mu/closed-schema schema) 10000)
  ; [], (mu/closed-schema schema), 10000 runs, 1046 msecs
  ; [], (mu/closed-schema schema), 10000 runs, 163 msecs (6x)
  ; [], (mu/closed-schema schema), 10000 runs, 104 msecs (10x)

  (simple-benchmark [] (m/deref ref-schema) 1000000)
  ; [], (m/deref ref-schema), 1000000 runs, 53 msecs
  ; [], (m/deref ref-schema), 1000000 runs, 53 msecs

  (simple-benchmark [] (m/deref-all ref-schema) 1000000)
  ; [], (m/deref-all ref-schema), 1000000 runs, 104 msecs
  ; [], (m/deref-all ref-schema), 1000000 runs, 55 msecs
  )
