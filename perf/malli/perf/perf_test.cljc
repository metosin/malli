(ns malli.perf.perf-test
  (:require [clojure.spec.alpha :as s]
            [malli.perf.core :as p]
            [clj-async-profiler.core :as prof]
            [minimallist.helper :as mh]
            [minimallist.core :as mc]
            [net.cgrand.seqexp :as se]
            [malli.core :as m]
            [spec-tools.core :as st]
            [schema.core :as sc]
            [schema.coerce :as scc]
            [clojure.pprint]
            [malli.transform :as mt]
            [malli.provider :as mp]))

(s/def ::x boolean?)
(s/def ::y int?)
(s/def ::z string?)

(defn map-perf []

  (let [valid {:x true, :y 1, :z "kikka"}]

    ;; 30ns
    (let [valid? (fn [m]
                   (and (if-let [v (:x m)] (boolean? v) false)
                        (if-let [v (:y m)] (int? v) true)
                        (if-let [v (:z m)] (string? v) false)))]
      (assert (valid? valid))
      (p/bench (valid? valid)))

    ;; 40ns
    (let [valid? (m/validator [:map
                               [:x boolean?]
                               [:y {:optional true} int?]
                               [:z string?]])]
      (assert (valid? valid))
      (p/bench (valid? valid)))

    ;; 450ns
    (let [spec (s/keys :req-un [::x ::z] :opt-un [::y])]
      (assert (s/valid? spec valid))
      (p/bench (s/valid? spec valid)))

    ;; 650ns
    (let [valid? (sc/checker {:x sc/Bool
                              (sc/optional-key :y) sc/Int
                              :z sc/Str})]
      (assert (not (valid? valid)))
      (p/bench (valid? valid)))))

(defn composite-perf []

  ;; 7ns
  (let [valid? (fn [x] (and (int? x) (or (pos-int? x) (neg-int? x))))]
    (assert (= [true false true] (map valid? [-1 0 1])))
    (p/bench (valid? 0)))

  ;; 9ns
  (let [valid? (m/validator [:and int? [:or pos-int? neg-int?]])]
    (assert (= [true false true] (map valid? [-1 0 1])))
    (p/bench (valid? 0)))

  ;; 60ns
  (let [spec (s/and int? (s/or :pos-int pos-int? :neg-int neg-int?))]
    (assert (= [true false true] (map (partial s/valid? spec) [-1 0 1])))
    (p/bench (s/valid? spec 0)))

  ;; 130ns
  (let [valid? (sc/checker (sc/both sc/Int (sc/conditional pos-int? (sc/pred pos-int?) :else (sc/pred neg-int?))))]
    (assert (= [true false true] (map (comp boolean not valid?) [-1 0 1])))
    (p/bench (valid? 0))))

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
      (p/bench (valid? [-1 1 2])))

    ;; 29ns
    (let [valid? (m/validator
                   [:vector {:max 3}
                    [:and int? [:or pos-int? neg-int?]]])]
      (assert! valid?)
      (p/bench (valid? [-1 1 2])))

    ;; 560ns
    (let [spec (s/coll-of
                 (s/and int? (s/or :pos-int pos-int? :neg-int neg-int?))
                 :kind vector?
                 :max-count 3)
          valid? (partial s/valid? spec)]
      (assert! valid?)
      (p/bench (valid? [-1 1 2])))))

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
      (p/bench (explain valid))

      ;; 19µs
      (p/bench (explain invalid)))

    (let [explain (m/explainer Place)]
      (assert (not (explain valid)))
      (assert (explain invalid))

      ;; 1.2µs
      (p/bench (explain valid))

      ;; 1.4µs
      (p/bench (explain invalid)))))

(defn transform-test []
  (let [json {:id "Metosin"
              :tags ["clj" "cljs"]
              :address {:street "Hämeenkatu 14"
                        :zip 33800
                        :lonlat [61 23.7644223]}}]

    (let [json->place #(st/coerce ::place % st/json-transformer)]
      (clojure.pprint/pprint (json->place json))

      ;; 74µs (wrong result!)
      (p/bench (json->place json)))

    (let [json->place (m/decoder Place mt/json-transformer)]
      (clojure.pprint/pprint (json->place json))

      ;; 1µs -> 800ns
      (p/bench (json->place json)))))

(defn transform-test2 []

  ;;
  ;; predicate coercion
  ;;

  ;; 6µs
  (let [string->edn #(st/coerce int? % st/string-transformer)]
    (assert (= 1
               (string->edn "1")
               (string->edn 1)))
    (p/bench (string->edn "1")))

  ;; 4ns
  (let [string->edn (m/decoder int? mt/string-transformer)]
    (assert (= 1
               (string->edn "1")
               (string->edn 1)))
    (p/bench (string->edn "1")))

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
    (p/bench (string->edn {:id "1", :name "kikka"})))

  ;; 44ns
  (let [schema [:map [:id int?] [:name string?]]
        string->edn (m/decoder schema mt/string-transformer)]
    (assert (= {:id 1, :name "kikka"}
               (string->edn {:id 1, :name "kikka"})
               (string->edn {:id "1", :name "kikka"})))
    (p/bench (string->edn {:id "1", :name "kikka"})))

  ;; 46ns
  (let [string->edn (fn [x] (update x :id (fn [id] (if (string? id) (Long/parseLong id) id))))]
    (assert (= {:id 1, :name "kikka"}
               (string->edn {:id 1, :name "kikka"})
               (string->edn {:id "1", :name "kikka"})))
    (p/bench (string->edn {:id "1", :name "kikka"})))

  ;; 1.4µs
  (let [schema {:id sc/Int, :name sc/Str}
        string->edn (scc/coercer schema scc/string-coercion-matcher)]
    (assert (= {:id 1, :name "kikka"}
               (string->edn {:id 1, :name "kikka"})
               (string->edn {:id "1", :name "kikka"})))
    (p/bench (string->edn {:id "1", :name "kikka"})))

  ;;
  ;; no-op coercion
  ;;

  ;; 15µs
  (let [spec (s/keys :req-un [::id ::name])
        string->edn #(st/coerce spec % st/json-transformer)]
    (assert (= {:id 1, :name "kikka"}
               (string->edn {:id 1, :name "kikka"})))
    (p/bench (string->edn {:id 1, :name "kikka"})))

  ;; 3.0ns
  (let [schema [:map [:id int?] [:name string?]]
        string->edn (m/decoder schema mt/json-transformer)]
    (assert (= {:id 1, :name "kikka"}
               (string->edn {:id 1, :name "kikka"})))
    (p/bench (string->edn {:id 1, :name "kikka"}))))


(def tests
  [;; 1.7ns -> 4.5ns
   [int? 1]
   ;; 5.8ns -> 11ns
   [[:and int? [:> 2]] 3]
   ;; 107ns -> 122ns
   [[:vector int?] [1 2 3]]
   ;; 17ns -> 34ns
   [[:map [:x int?] [:y boolean?]] {:x 1, :y true}]])

(defn basic-perf []
  (doseq [[schema value] tests
          :let [validator (m/validator schema)]]
    (println)
    (println (m/form schema))
    (println "-------------")
    (p/bench (validator value))))

(defn fn-test []
  (let [f (fn [x] (> x 10))
        f2 (eval '(fn [x] (> x 10)))
        f3 (m/eval '(fn [x] (> x 10)))]

    ;; 4ns
    (p/bench (f 12))

    ;; 8ns
    (p/bench (f2 12))

    ;; 7000ns -> 73ns
    (p/bench (f3 12))))

(defn map-transform-test []
  (doseq [transformer [mt/json-transformer
                       (mt/transformer
                         mt/strip-extra-keys-transformer
                         mt/json-transformer)]]

    ;; 3ns -> 3ns
    ;; 520ns -> 130ns
    (let [>> (m/decoder [:map [:x string?] [:y int?]] transformer)]
      (p/bench (>> {:x "1", :y 1})))))

(defn select-keys-perf-test []
  (let [ks #{:a :b}
        quick-select-keys (fn [x] (reduce (fn [acc k] (if-not (ks k) (dissoc acc k) acc)) x (keys x)))
        normal-select-keys (fn [x] (select-keys x ks))]

    (assert (= {:a 1, :b 2} (normal-select-keys {:a 1, :b 2, :c 3})))
    (assert (= {:a 1, :b 2} (quick-select-keys {:a 1, :b 2, :c 3})))

    ;; 370ns
    (p/bench (normal-select-keys {:a 1, :b 2}))
    (p/bench (normal-select-keys {:a 1, :b 2, :c 3, :d 4}))

    ;; 110ns
    (p/bench (quick-select-keys {:a 1, :b 2}))
    (p/bench (quick-select-keys {:a 1, :b 2, :c 3, :d 4}))))

(defn sequence-perf-test []
  ;; 27µs
  (let [valid? (partial s/valid? (s/* int?))]
    (p/bench (valid? (range 10))))

  ;; 2.7µs
  (let [valid? (m/validator [:* int?])]
    (p/bench (valid? (range 10)))))

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

        malli [:* [:catn
                   [:prop string?]
                   [:val [:altn
                          [:s string?]
                          [:b boolean?]]]]]
        valid-malli? (m/validator malli)]

    ;; 90µs
    (p/bench (valid-seqxp? data))

    ;; 40µs
    (p/bench (valid-spec? data))

    ;; 12µs
    (p/bench (valid-minimallist? data))

    ;; 1.5µs
    (p/bench (valid-malli? data))))

(defn parsing []

  ;; 44µs
  (let [spec (s/* (s/cat :prop string?,
                         :val (s/alt :s string?
                                     :b boolean?)))
        parse (partial s/conform spec)]
    (p/bench
      (parse ["-server" "foo" "-verbose" "-verbose" "-user" "joe"])))

  ;; 2.5µs
  (let [schema [:* [:catn
                    [:prop string?]
                    [:val [:altn
                           [:s string?]
                           [:b boolean?]]]]]
        parse (m/parser schema)]
    (p/bench
      (parse ["-server" "foo" "-verbose" "-verbose" "-user" "joe"]))))

(defn and-map-perf-test []

  ;; 164ns -> 36ns
  (let [valid? (m/validator (into [:and] (map (fn [x] [:> x]) (range 5))))]
    (p/bench (valid? 5)))

  ;; 150ns -> 126n -> 39ns -> 32ns
  (let [->key #(keyword (str "key_" %))
        valid? (m/validator (into [:map] (map (fn [x] [(->key x) :any]) (range 5))))
        value (reduce (fn [acc x] (assoc acc (->key x) x)) {} (range 5))]
    (p/bench (valid? value))))

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

(defn provider-test []

  ;; 3.6ms
  ;; 2.1ms (1.7x)
  (p/bench (mp/provide [1 2 3]))

  ;; 2.6ms
  (p/bench (mp/provider))

  ;; 2.5ms
  ;;  54µs (46x)
  (let [provider (mp/provider)]
    (p/bench (provider [1 2 3]))))

(defn provider-test2 []
  (let [samples [{:id "Lillan"
                  :tags #{:artesan :coffee :hotel}
                  :address {:street "Ahlmanintie 29"
                            :city "Tampere"
                            :zip 33100
                            :lonlat [61.4858322, 23.7854658]}}
                 {:id "Huber",
                  :description "Beefy place"
                  :tags #{:beef :wine :beer}
                  :address {:street "Aleksis Kiven katu 13"
                            :city "Tampere"
                            :zip 33200
                            :lonlat [61.4963599 23.7604916]}}]]

    ;; 126ms
    (p/bench (mp/provide samples))

    ;; 26ms
    ;; 380µs no exceptions, (330x)
    (let [provide (mp/provider)]
      (p/bench (provide samples)))))

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
  (parsing)
  (and-map-perf-test)
  (provider-test)
  (provider-test2)

  (p/clear!)

  (address-flame)
  (schema-flames))

(comment
  (let [f (m/eval '(fn [x] (> x 10)))]
    (time
      (prof/profile
        (dotimes [_ 5000000]
          (f 12))))))
