(ns malli.perf-test
  (:require [clojure.spec.alpha :as s]
            [criterium.core :as cc]
            [clj-async-profiler.core :as prof]
            [minimallist.helper :as mh]
            [minimallist.core :as mc]
            [net.cgrand.seqexp :as se]
            [malli.core :as m]
            [spec-tools.core :as st]
            [malli.transform :as transform]))

(s/def ::x boolean?)
(s/def ::y int?)
(s/def ::z string?)

(defn map-perf []

  (let [valid {:x true, :y 1, :z "kikka"}]

    ;; 18ns
    (let [valid? (fn [m]
                   (and (if-let [v (:x m)] (boolean? v) false)
                        (if-let [v (:y m)] (int? v) true)
                        (if-let [v (:z m)] (string? v) false)))]
      (assert (valid? valid))
      (cc/quick-bench
        (valid? valid)))

    ;; 37ns
    (let [valid? (m/validator [:map
                               [:x boolean?]
                               [:y {:optional true} int?]
                               [:z string?]])]
      (assert (valid? valid))
      (cc/quick-bench
        (valid? valid)))

    ;; 400ns
    (let [spec (s/keys :req-un [::x ::z] :opt-un [::y])]
      (assert (s/valid? spec valid))
      (cc/quick-bench
        (s/valid? spec valid)))))

(defn composite-perf []

  ;; 3ns
  (let [valid? (fn [x] (and (int? x) (or (pos-int? x) (neg-int? x))))]
    (assert (= [true false true] (map valid? [-1 0 1])))
    (cc/quick-bench
      (valid? 0)))

  ;; 5ns
  (let [valid? (m/validator [:and int? [:or pos-int? neg-int?]])]
    (assert (= [true false true] (map valid? [-1 0 1])))
    (cc/quick-bench
      (valid? 0)))

  ;; 40ns
  (let [spec (s/and int? (s/or :pos-int pos-int? :neg-int neg-int?))]
    (assert (= [true false true] (map (partial s/valid? spec) [-1 0 1])))
    (cc/quick-bench
      (s/valid? spec 0))))

(defn composite-perf2 []
  (let [assert! (fn [f]
                  (doseq [[expected data] [[true [-1]]
                                           [true [-1 1 2]]
                                           [false [-1 0 2]]
                                           [false [-1 -1 -1 -1]]]]
                    (assert (= expected (f data)))))]

    ;; 155ns
    (let [valid? (fn [x]
                   (and (vector? x)
                        (<= (count x) 3)
                        (every? #(and (int? %) (or (pos-int? %) (neg-int? %))) x)))]
      (assert! valid?)
      (cc/quick-bench
        (valid? [-1 1 2])))

    ;; 27ns
    (let [valid? (m/validator
                   [:vector {:max 3}
                    [:and int? [:or pos-int? neg-int?]]])]
      (assert! valid?)
      (cc/quick-bench
        (valid? [-1 1 2])))

    ;; 506ns
    (let [spec (s/coll-of
                 (s/and int? (s/or :pos-int pos-int? :neg-int neg-int?))
                 :kind vector?
                 :max-count 3)
          valid? (partial s/valid? spec)]
      (assert! valid?)
      (cc/quick-bench
        (valid? [-1 1 2])))))

(s/def ::id string?)
(s/def ::tags (s/coll-of keyword? :kind set? :into #{}))
(s/def ::street string?)
(s/def ::city string?)
(s/def ::zip int?)
(s/def ::lonlat (s/coll-of double? :min-count 2, :max-count 2))
(s/def ::address (s/keys
                   :req-un [::street ::city ::zip]
                   :opt-un [::lonlat]))
(s/def ::place (s/keys :req-un [::id ::tags ::address]))

(def Place
  [:map
   [:id string?]
   [:tags [:set keyword?]]
   [:address
    [:map
     [:street string?]
     [:city string?]
     [:zip int?]
     [:lonlat [:tuple double? double?]]]]])

(defn composite-explain-perf []
  (let [valid {:id "Metosin"
               :tags #{:clj :cljs}
               :address {:street "Hämeenkatu 14"
                         :city "Tampere"
                         :zip 33800
                         :lonlat [61.4983866 23.7644223]}}
        invalid {:id "Metosin"
                 :tags #{"clj" "cljs"}
                 :address {:street "Hämeenkatu 14"
                           :zip 33800
                           :lonlat [61.4983866 nil]}}]

    (let [explain #(s/explain-data ::place %)]

      ;; 5.0µs
      (cc/quick-bench (explain valid))

      ;; 19µs
      (cc/quick-bench (explain invalid)))

    (let [explain (m/explainer Place)]
      (assert (not (explain valid)))
      (assert (explain invalid))

      ;; 1.2µs
      (cc/quick-bench (explain valid))

      ;; 1.4µs
      (cc/quick-bench (explain invalid)))))

(defn transform-test []
  (let [json {:id "Metosin"
              :tags #{"clj" "cljs"}
              :address {:street "Hämeenkatu 14"
                        :zip 33800
                        :lonlat [61.4983866 23.7644223]}}]

    (let [json->place #(st/coerce ::place % st/json-transformer)]
      (clojure.pprint/pprint (json->place json))

      ;; 74µs
      (cc/quick-bench (json->place json)))

    (let [json->place (m/decoder Place transform/json-transformer)]
      (clojure.pprint/pprint (json->place json))

      ;; 1µs
      (cc/quick-bench (json->place json)))))

(defn transform-test2 []

  ;;
  ;; predicate coercion
  ;;

  ;; 6µs
  (let [string->edn #(st/coerce int? % st/string-transformer)]
    (assert (= 1
               (string->edn "1")
               (string->edn 1)))
    (cc/quick-bench
      (string->edn "1")))

  ;; 4ns
  (let [string->edn (m/decoder int? transform/string-transformer)]
    (assert (= 1
               (string->edn "1")
               (string->edn 1)))
    (cc/quick-bench
      (string->edn "1")))

  ;;
  ;; simple map coercion
  ;;

  (s/def ::id int?)
  (s/def ::name string?)

  ;; 14µs
  (let [spec (s/keys :req-un [::id ::name])
        string->edn #(st/coerce spec % st/string-transformer)]
    (assert (= {:id 1, :name "kikka"}
               (string->edn {:id 1, :name "kikka"})
               (string->edn {:id "1", :name "kikka"})))
    (cc/quick-bench
      (string->edn {:id "1", :name "kikka"})))

  ;; 140ns
  (let [schema [:map [:id int?] [:name string?]]
        string->edn (m/decoder schema transform/string-transformer)]
    (assert (= {:id 1, :name "kikka"}
               (string->edn {:id 1, :name "kikka"})
               (string->edn {:id "1", :name "kikka"})))
    (cc/quick-bench
      (string->edn {:id "1", :name "kikka"})))

  ;;
  ;; no-op coercion
  ;;

  ;; 15µs
  (let [spec (s/keys :req-un [::id ::name])
        string->edn #(st/coerce spec % st/json-transformer)]
    (assert (= {:id 1, :name "kikka"}
               (string->edn {:id 1, :name "kikka"})))
    (cc/quick-bench
      (string->edn {:id 1, :name "kikka"})))

  ;; 3.0ns
  (let [schema [:map [:id int?] [:name string?]]
        string->edn (m/decoder schema transform/json-transformer)]
    (assert (= {:id 1, :name "kikka"}
               (string->edn {:id 1, :name "kikka"})))
    (cc/quick-bench
      (string->edn {:id 1, :name "kikka"}))))

(def tests
  [;; 1.7ns
   [int? 1]
   ;; 5.8ns
   [[:and int? [:> 2]] 3]
   ;; 107ns
   [[:vector int?] [1 2 3]]
   ;; 17ns
   [[:map [:x int?] [:y boolean?]] {:x 1, :y true}]])

(defn basic-perf []
  (doseq [[schema value] tests
          :let [validator (m/validator schema)]]
    (println)
    (println (m/form schema))
    (println "-------------")
    (cc/quick-bench (validator value))))

(defn fn-test []
  (let [f (fn [x] (> x 10))
        f2 (eval '(fn [x] (> x 10)))
        f3 (m/eval '(fn [x] (> x 10)))]

    ;; 4ns
    (cc/quick-bench (f 12))

    ;; 8ns
    (cc/quick-bench (f2 12))

    ;; 7000ns
    (cc/quick-bench (f3 12))))

(defn map-transform-test []
  (doseq [transformer [transform/json-transformer
                       (transform/transformer
                         transform/strip-extra-keys-transformer
                         transform/json-transformer)]]

    ;; 3ns -> 3ns
    ;; 520ns -> 130ns
    (let [>> (m/decoder [:map [:x string?] [:y int?]] transformer)]
      (cc/quick-bench (>> {:x "1", :y 1})))))

(defn select-keys-perf-test []
  (let [ks #{:a :b}
        quick-select-keys (fn [x] (reduce (fn [acc k] (if-not (ks k) (dissoc acc k) acc)) x (keys x)))
        normal-select-keys (fn [x] (select-keys x ks))]

    (assert (= {:a 1, :b 2} (normal-select-keys {:a 1, :b 2, :c 3})))
    (assert (= {:a 1, :b 2} (quick-select-keys {:a 1, :b 2, :c 3})))

    ;; 370ns
    (cc/quick-bench (normal-select-keys {:a 1, :b 2}))
    (cc/quick-bench (normal-select-keys {:a 1, :b 2, :c 3, :d 4}))

    ;; 110ns
    (cc/quick-bench (quick-select-keys {:a 1, :b 2}))
    (cc/quick-bench (quick-select-keys {:a 1, :b 2, :c 3, :d 4}))))

(defn sequence-perf-test []
  ;; 27µs
  (let [valid? (partial s/valid? (s/* int?))]
    (cc/quick-bench (valid? (range 10))))

  ;; 2.7µs
  (let [valid? (m/validator [:* int?])]
    (cc/quick-bench (valid? (range 10)))))

(defn simple-regex []
  (let [data ["-server" "foo" "-verbose" "-verbose" "-user" "joe"]

        seqxp (se/*
                (se/as [:opts]
                       (se/cat
                         (se/as [:opts :prop] string?)
                         (se/as [:opts :val] (se/|
                                               (se/as [:opts :val :s] string?)
                                               (se/as [:opts :val :b] boolean?))))))
        valid-seqxp? (partial se/exec-tree seqxp)

        spec (s/* (s/cat :prop string?,
                         :val (s/alt :s string?
                                     :b boolean?)))
        valid-spec? (partial s/valid? spec)

        minimallist (mh/* (mh/cat [:prop (mh/fn string?)]
                                  [:val (mh/alt [:s (mh/fn string?)]
                                                [:b (mh/fn boolean?)])]))
        valid-minimallist? (partial mc/valid? minimallist)

        malli [:* [:cat* [:prop string?]
                   [:val [:alt*
                          [:s string?]
                          [:b boolean?]]]]]
        valid-malli? (m/validator malli)]

    ;; 90µs
    (cc/quick-bench (valid-seqxp? data))

    ;; 40µs
    (cc/quick-bench (valid-spec? data))

    ;; 12µs
    (cc/quick-bench (valid-minimallist? data))

    ;; 2µs
    (cc/quick-bench (valid-malli? data))))

(defn schema-flames []

  ;; "Elapsed time: 10472.153783 msecs"
  ;; "Elapsed time: 524.153783 msecs"
  (time
    (prof/profile
      (dotimes [_ 50000]
        (m/validate [:map [:street :string]] {:street "hämeenkatu"}))))

  ;; "Elapsed time: 231.093848 msecs"
  (let [schema (m/schema [:map [:street :string]])]
    (time
      (prof/profile
        (dotimes [_ 500000]
          (m/validate schema {:street "hämeenkatu"})))))

  ;; "Elapsed time: 59.743646 msecs"
  (let [validate (m/validator [:map [:street :string]])]
    (time
      (prof/profile
        (dotimes [_ 500000]
          (validate {:street "hämeenkatu"}))))))

(defn address-flame []
  (time
    (prof/profile
      (dotimes [_ 1000]
        (m/schema
          [:schema
           {:registry {"Country" [:map
                                  {:closed true}
                                  [:name [:enum :FI :PO]]
                                  [:neighbors
                                   {:optional true}
                                   [:vector [:ref "Country"]]]],
                       "Burger" [:map
                                 [:name string?]
                                 [:description {:optional true} string?]
                                 [:origin [:maybe "Country"]]
                                 [:price pos-int?]],
                       "OrderLine" [:map
                                    {:closed true}
                                    [:burger "Burger"]
                                    [:amount int?]],
                       "Order" [:map
                                {:closed true}
                                [:lines [:vector "OrderLine"]]
                                [:delivery
                                 [:map
                                  {:closed true}
                                  [:delivered boolean?]
                                  [:address
                                   [:map
                                    [:street string?]
                                    [:zip int?]
                                    [:country "Country"]]]]]]}}
           "Order"])))))


(comment
  (map-perf)
  (composite-perf)
  (composite-perf2)
  (composite-explain-perf)
  (basic-perf)
  (transform-test)
  (transform-test2)
  (map-transform-test)
  (select-keys-perf-test)
  (fn-test)
  (sequence-perf-test)
  (simple-regex)

  (prof/serve-files 8080)
  (prof/clear-results)

  (address-flame)
  (schema-flames))
