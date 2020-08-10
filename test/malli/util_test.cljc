(ns malli.util-test
  (:require [clojure.test :refer [deftest testing is are]]
            [malli.util :as mu]
            [malli.core :as m]))

(defn form= [& ?schemas]
  (apply = (map #(if (m/schema? %) (m/form %) %) ?schemas)))

(deftest equals-test
  (is (true? (mu/equals int? int?)))
  (is (true? (mu/equals [:map [:x int?]] [:map [:x int?]])))
  (is (false? (mu/equals [:map [:x {} int?]] [:map [:x int?]]))))

(deftest simplify-map-entry-test
  (are [entry expected]
    (is (= expected (mu/-simplify-map-entry entry)))

    [:x 'int?] [:x 'int?]
    [:x nil 'int?] [:x 'int?]
    [:x {} 'int?] [:x 'int?]
    [:x {:optional false} 'int?] [:x 'int?]
    [:x {:optional false, :x 1} 'int?] [:x {:x 1} 'int?]
    [:x {:optional true} 'int?] [:x {:optional true} 'int?]))

(deftest merge-test
  (are [?s1 ?s2 expected]
    (= true (mu/equals expected (mu/merge ?s1 ?s2)))

    int? int? int?
    int? pos-int? pos-int?
    int? nil int?
    nil pos-int? pos-int?

    [:map [:x int?]]
    [:map [:x {:optional true} pos-int?]]
    [:map [:x {:optional true} pos-int?]]

    [:map [:x {:optional true} int?]]
    [:map [:x pos-int?]]
    [:map [:x pos-int?]]

    [:map [:x {:optional false} int?]]
    [:map [:x {:optional true} pos-int?]]
    [:map [:x {:optional true} pos-int?]]

    [:map {:title "parameters"}
     [:parameters
      [:map
       [:query-params {:title "query1", :description "first"}
        [:map [:x int?]]]]]]
    [:map {:description "description"}
     [:parameters
      [:map
       [:query-params {:title "query2", :summary "second"}
        [:map [:x string?] [:y int?]]]
       [:body-params
        [:map [:z int?]]]]]]
    [:map {:title "parameters", :description "description"}
     [:parameters
      [:map
       [:query-params {:title "query2", :description "first", :summary "second"}
        [:map [:x string?] [:y int?]]]
       [:body-params
        [:map [:z int?]]]]]]))

(deftest union-test
  (are [?s1 ?s2 expected]
    (= true (mu/equals expected (mu/union ?s1 ?s2)))

    int? int? int?
    int? pos-int? [:or int? pos-int?]
    int? nil int?
    nil pos-int? pos-int?

    [:map [:x int?]]
    [:map [:x {:optional true} pos-int?]]
    [:map [:x {:optional true} [:or int? pos-int?]]]

    [:map [:x int?]]
    [:map [:x {:optional true} pos-int?]]
    [:map [:x {:optional true} [:or int? pos-int?]]]

    [:map [:x {:optional true} int?]]
    [:map [:x {:optional true} pos-int?]]
    [:map [:x {:optional true} [:or int? pos-int?]]]

    [:map [:x {:optional false} int?]]
    [:map [:x {:optional true} pos-int?]]
    [:map [:x {:optional true} [:or int? pos-int?]]]

    [:map {:title "parameters"}
     [:parameters
      [:map
       [:query-params {:title "query1", :description "first"}
        [:map [:x int?]]]]]]
    [:map {:description "description"}
     [:parameters
      [:map
       [:query-params {:title "query2", :summary "second"}
        [:map [:x string?] [:y int?]]]
       [:body-params
        [:map [:z int?]]]]]]
    [:map {:title "parameters", :description "description"}
     [:parameters
      [:map
       [:query-params {:title "query2", :description "first", :summary "second"}
        [:map [:x [:or int? string?]] [:y int?]]]
       [:body-params
        [:map [:z int?]]]]]]))

(deftest update-properties-test
  (let [schema [:and {:x 0} int?]]
    (is (mu/equals [:and {:x 1} int?]
                   (mu/update-properties schema update :x inc)))
    (is (mu/equals [:and {:x 0, :joulu "loma"} int?]
                   (mu/update-properties schema assoc :joulu "loma")))))

(deftest open-closed-schema-test
  (let [open [:map {:title "map"}
              [:a int?]
              [:b {:optional true} int?]
              [:c [:map
                   [:d int?]]]]
        closed [:map {:title "map", :closed true}
                [:a int?]
                [:b {:optional true} int?]
                [:c [:map {:closed true}
                     [:d int?]]]]]

    (is (mu/equals closed (mu/closed-schema open)))
    (is (mu/equals open (mu/open-schema closed))))

  (testing "explicitely open maps not effected"
    (let [schema [:map {:title "map", :closed false}
                  [:a int?]
                  [:b {:optional true} int?]
                  [:c [:map {, :closed false}
                       [:d int?]]]]]

      (is (mu/equals schema (mu/closed-schema schema)))
      (is (mu/equals schema (mu/open-schema schema))))))

(deftest select-key-test
  (let [schema [:map {:title "map"}
                [:a int?]
                [:b {:optional true} int?]
                [:c string?]]]
    (is (mu/equals (mu/select-keys schema []) [:map {:title "map"}]))
    (is (mu/equals (mu/select-keys schema nil) [:map {:title "map"}]))
    (is (mu/equals (mu/select-keys schema [:a]) [:map {:title "map"} [:a int?]]))
    (is (mu/equals (mu/select-keys schema #{:a}) [:map {:title "map"} [:a int?]]))
    (is (mu/equals (mu/select-keys schema '(:a)) [:map {:title "map"} [:a int?]]))
    (is (mu/equals (mu/select-keys schema [:a :b :c]) schema))
    (is (mu/equals (mu/select-keys schema [:a :b :extra])
                   [:map {:title "map"}
                    [:a int?]
                    [:b {:optional true} int?]]))))

;;
;; LensSchemas
;;


(deftest basic-lens-schema-test
  (let [re #"kikka"
        int? (m/schema int?)]

    (testing "get"
      (are [schema key expected]
        (is (form= (try
                     (mu/get (m/schema schema) key)
                     (catch #?(:clj Exception, :cljs js/Error) _ ::throws))
                   expected))

        nil 0 ::throws

        int? 0 nil
        int? 1 nil

        [:re re] 0 re
        [:re re] 1 nil

        [:fn 'int?] 0 int?
        [:fn 'int?] 1 nil

        [:string {:min 1}] 0 nil
        [:string {:min 1}] 1 nil

        [:enum "A" "B"] 0 "A"
        [:enum "A" "B"] 2 nil

        [:map [:x int?]] :x int?
        [:map [:x int?]] :y nil

        [:map [:x {:optional true} int?]] :x int?
        [:map [:x {:optional true} int?]] :y nil

        [:vector int?] 0 int?
        [:vector int?] 1 int?

        [:list int?] 0 int?
        [:list int?] 1 int?

        [:set int?] 0 int?
        [:set int?] 1 int?

        [:sequential int?] 0 int?
        [:sequential int?] 1 int?

        [:or false? int?] 1 int?
        [:or false? int?] 2 nil

        [:and false? int?] 1 int?
        [:and false? int?] 2 nil

        [:tuple false? int?] 1 int?
        [:tuple false? int?] 2 nil

        [:map-of false? int?] 1 int?
        [:map-of false? int?] 2 nil

        [:ref {:registry {::a int?, ::b string?}} ::a] 0 ::a
        [:ref {:registry {::a int?, ::b string?}} ::a] 1 nil

        [:schema int?] 0 int?
        [:schema int?] 1 nil)

      (is (mu/equals (mu/get [:tuple int? pos-int?] 9 boolean?) boolean?))
      (is (mu/equals (mu/get [:map [:x int?]] :y boolean?) boolean?)))

    (testing "assoc"
      (are [schema key value expected]
        (is (form= (try
                     (mu/assoc (m/schema schema) key value)
                     (catch #?(:clj Exception, :cljs js/Error) _ ::throws))
                   expected))

        nil 0 'int? ::throws

        int? 0 'int? ::throws

        [:re #"kukka"] 0 re [:re re]
        [:re #"kukka"] 1 re ::throws

        [:fn 'string?] 0 'int? [:fn 'int?]
        [:fn 'string?] 1 'int? ::throws

        [:string {:min 1}] 0 'int? ::throws

        [:enum "A" "B"] 1 "C" [:enum "A" "C"]
        [:enum "A" "B"] 2 "C" [:enum "A" "B" "C"]
        [:enum "A" "B"] 3 "C" ::throws

        [:map [:x string?]] :x int? [:map [:x 'int?]]
        [:map [:x {:optional true} string?]] :x int? [:map [:x 'int?]]
        [:map [:x string?]] [:x {:optional true}] int? [:map [:x {:optional true} 'int?]]
        ;[:map [:x {:optional true} string?]] [:x] int? [:map [:x {:optional true} 'int?]]
        [:map [:x string?]] :y string? [:map [:x 'string?] [:y 'string?]]
        [:map [:x {:optional true} string?]] :y string? [:map [:x {:optional true} 'string?] [:y 'string?]]
        [:map [:x string?]] [:y {:optional true}] string? [:map [:x 'string?] [:y {:optional true} 'string?]]
        [:map [:x string?]] [] int? ::throws

        [:vector string?] 0 int? [:vector 'int?]
        [:vector string?] 1 int? [:vector 'int?]

        [:list string?] 0 int? [:list 'int?]
        [:list string?] 1 int? [:list 'int?]

        [:set string?] 0 int? [:set 'int?]
        [:set string?] 1 int? [:set 'int?]

        [:sequential string?] 0 int? [:sequential 'int?]
        [:sequential string?] 1 int? [:sequential 'int?]

        [:or false? string?] 1 int? [:or 'false? 'int?]
        [:or false? string?] 2 int? [:or 'false? 'string? 'int?]
        [:or false? string?] 3 int? ::throws

        [:and false? string?] 1 int? [:and 'false? 'int?]
        [:and false? string?] 2 int? [:and 'false? 'string? 'int?]
        [:and false? string?] 3 int? ::throws

        [:tuple false? string?] 1 int? [:tuple 'false? 'int?]
        [:tuple false? string?] 2 int? [:tuple 'false? 'string? 'int?]
        [:tuple false? string?] 3 int? ::throws

        [:map-of false? string?] 0 int? [:map-of 'int? 'string?]
        [:map-of false? string?] 1 int? [:map-of 'false? 'int?]
        [:map-of false? string?] 2 int? ::throws

        [:ref {:registry {::a int?, ::b string?}} ::a] 0 ::b [:ref {:registry {::a 'int?, ::b 'string?}} ::b]
        [:ref {:registry {::a int?, ::b string?}} ::a] 0 "invalid" ::throws
        [:ref {:registry {::a int?, ::b string?}} ::a] 1 ::b ::throws

        [:schema string?] 0 int? [:schema 'int?]
        [:schema string?] 0 "invalid" ::throws
        [:schema string?] 1 int? ::throws))))

(deftest get-in-test
  (is (mu/equals boolean?
                 (mu/get-in
                   (m/schema [:map
                              [:x [:vector
                                   [:list
                                    [:set
                                     [:sequential
                                      [:tuple int? [:map [:y [:maybe boolean?]]]]]]]]]])
                   [:x 0 0 0 0 1 :y 0])))
  (is (mu/equals (mu/get-in
                   [:map
                    [:x [:vector
                         [:list
                          [:set
                           [:sequential
                            [:tuple int? [:map [:y [:maybe boolean?]]]]]]]]]]
                   [:x 0 0 0 0 1 :y 9]
                   pos-int?)
                 pos-int?))
  (is (mu/equals [:maybe [:tuple int? boolean?]]
                 (mu/get-in (m/schema [:maybe [:tuple int? boolean?]]) [])))
  (is (form= (mu/get-in (m/schema [:ref {:registry {::a int?, ::b string?}} ::a]) [0]) ::a))
  (is (mu/equals (mu/get-in (m/schema [:ref {:registry {::a int?, ::b string?}} ::a]) [0 0]) int?))
  (is (form= (mu/get-in (m/schema [:schema {:registry {::a int?, ::b string?}} ::a]) [0]) ::a))
  (is (mu/equals (mu/get-in (m/schema [:schema {:registry {::a int?, ::b string?}} ::a]) [0 0]) int?)))

(deftest dissoc-test
  (let [schema [:map {:title "map"}
                [:a int?]
                [:b {:optional true} int?]
                [:c string?]]]
    (is (mu/equals (mu/dissoc schema :a)
                   [:map {:title "map"}
                    [:b {:optional true} int?]
                    [:c string?]]))
    (is (mu/equals (mu/dissoc schema :b)
                   [:map {:title "map"}
                    [:a int?]
                    [:c string?]]))))

(deftest update-test
  (is (mu/equals (mu/update (m/schema [:vector int?]) 0 (constantly string?)) [:vector string?]))
  (is (mu/equals (mu/update (m/schema [:tuple int? int?]) 1 (constantly string?)) [:tuple int? string?]))
  (is (mu/equals (mu/update (m/schema [:tuple int? int?]) 2 (constantly string?)) [:tuple int? int? string?]))
  (is (mu/equals (mu/update (m/schema [:or int? int?]) 1 (constantly string?)) [:or int? string?]))
  (is (mu/equals (mu/update (m/schema [:or int? int?]) 2 (constantly string?)) [:or int? int? string?]))
  (is (mu/equals (mu/update (m/schema [:and int? int?]) 1 (constantly string?)) [:and int? string?]))
  (is (mu/equals (mu/update (m/schema [:and int? int?]) 2 (constantly string?)) [:and int? int? string?]))
  (is (mu/equals (mu/update (m/schema [:maybe int?]) 0 (constantly string?)) [:maybe string?]))
  (is (mu/equals (mu/update (m/schema [:map [:x int?] [:y int?]]) :x (constantly nil)) [:map [:y int?]]))
  (is (mu/equals (mu/update (m/schema [:ref {:registry {::a int?, ::b string?}} ::a]) 0 (constantly ::b)) [:ref {:registry {::a int?, ::b string?}} ::b]))
  (is (mu/equals (mu/update (m/schema [:schema int?]) 0 (constantly string?)) [:schema string?]))

  (let [schema (m/schema
                 [:map {:title "map"}
                  [:a int?]
                  [:b {:optional true} int?]
                  [:c string?]])]
    (is (mu/equals (mu/update schema :a mu/update-properties assoc :title "a")
                   [:map {:title "map"}
                    [:a [int? {:title "a"}]]
                    [:b {:optional true} int?]
                    [:c string?]]))
    (is (mu/equals (mu/update schema :a (constantly string?))
                   [:map {:title "map"}
                    [:a string?]
                    [:b {:optional true} int?]
                    [:c string?]]))
    (is (mu/equals (mu/update schema [:a {:optional true}] (constantly string?))
                   [:map {:title "map"}
                    [:a {:optional true} string?]
                    [:b {:optional true} int?]
                    [:c string?]]))
    (is (mu/equals (mu/update schema :b (constantly string?))
                   [:map {:title "map"}
                    [:a int?]
                    [:b string?]
                    [:c string?]]))))

(deftest assoc-in-test
  (is (mu/equals (mu/assoc-in (m/schema [:vector int?]) [0] string?) [:vector string?]))
  (is (mu/equals (mu/assoc-in (m/schema [:tuple int? int?]) [1] string?) [:tuple int? string?]))
  (is (mu/equals (mu/assoc-in (m/schema [:tuple int? int?]) [2] string?) [:tuple int? int? string?]))
  (is (mu/equals (mu/assoc-in (m/schema [:or int? int?]) [1] string?) [:or int? string?]))
  (is (mu/equals (mu/assoc-in (m/schema [:or int? int?]) [2] string?) [:or int? int? string?]))
  (is (mu/equals (mu/assoc-in (m/schema [:and int? int?]) [1] string?) [:and int? string?]))
  (is (mu/equals (mu/assoc-in (m/schema [:and int? int?]) [2] string?) [:and int? int? string?]))
  (is (mu/equals (mu/assoc-in (m/schema [:maybe int?]) [0] string?) [:maybe string?]))
  (is (mu/equals (mu/assoc-in (m/schema [:map]) [:a :b :c :d] int?)
                 [:map [:a [:map [:b [:map [:c [:map [:d int?]]]]]]]]))
  (is (mu/equals (mu/assoc-in (m/schema [:ref {:registry {::a int?, ::b string?}} ::a]) [0] ::b) [:ref {:registry {::a int?, ::b string?}} ::b]))
  (is (mu/equals (mu/assoc-in (m/schema [:schema int?]) [0] string?) [:schema string?])))

(deftest update-in-test
  (is (mu/equals (mu/update-in (m/schema [:vector int?]) [0] (constantly string?)) [:vector string?]))
  (is (mu/equals (mu/update-in (m/schema [:tuple int? int?]) [1] (constantly string?)) [:tuple int? string?]))
  (is (mu/equals (mu/update-in (m/schema [:tuple int? int?]) [2] (constantly string?)) [:tuple int? int? string?]))
  (is (mu/equals (mu/update-in (m/schema [:or int? int?]) [1] (constantly string?)) [:or int? string?]))
  (is (mu/equals (mu/update-in (m/schema [:or int? int?]) [2] (constantly string?)) [:or int? int? string?]))
  (is (mu/equals (mu/update-in (m/schema [:and int? int?]) [1] (constantly string?)) [:and int? string?]))
  (is (mu/equals (mu/update-in (m/schema [:and int? int?]) [2] (constantly string?)) [:and int? int? string?]))
  (is (mu/equals (mu/update-in (m/schema [:maybe int?]) [0] (constantly string?)) [:maybe string?]))
  (is (mu/equals (mu/update-in (m/schema [:map]) [:a :b :c :d] (constantly int?))
                 [:map [:a [:map [:b [:map [:c [:map [:d int?]]]]]]]]))
  (is (mu/equals (mu/update-in (m/schema [:ref {:registry {::a int?, ::b string?}} ::a]) [0] (constantly ::b)) [:ref {:registry {::a int?, ::b string?}} ::b]))
  (is (mu/equals (mu/update-in (m/schema [:schema int?]) [0] (constantly string?)) [:schema string?])))

(deftest optional-keys-test
  (let [schema [:map [:x int?] [:y int?]]]
    (is (mu/equals (mu/optional-keys schema)
                   [:map [:x {:optional true} int?] [:y {:optional true} int?]]))
    (is (mu/equals (mu/optional-keys schema [:x :extra nil])
                   [:map [:x {:optional true} int?] [:y int?]]))))

(deftest required-keys-test
  (let [schema [:map [:x {:optional true} int?] [:y {:optional false} int?]]]
    (is (mu/equals (mu/required-keys schema)
                   [:map [:x int?] [:y int?]]))
    (is (mu/equals (mu/required-keys schema [:x :extra nil])
                   [:map [:x int?] [:y {:optional false} int?]]))))

(deftest find-first-test
  (let [schema [:map
                [:x int?]
                [:y [:vector [:tuple
                              [:maybe int?]
                              [:or [:and {:salaisuus "turvassa"} boolean?] int?]
                              [:schema {:salaisuus "vaarassa"} false?]]]]
                [:z [:string {:salaisuus "piilossa"}]]]]

    (let [walked-properties (atom [])]
      (is (= "turvassa" (mu/find-first
                          schema
                          (fn [s _in _options]
                            (some->> s m/properties (swap! walked-properties conj))
                            (some-> s m/properties :salaisuus)))))
      (is (= [{:salaisuus "turvassa"}] @walked-properties)))

    (let [walked-properties (atom [])]
      (is (= "vaarassa" (mu/find-first
                          schema
                          (fn [s _in _options]
                            (some->> s m/properties (swap! walked-properties conj))
                            (some-> s m/properties :salaisuus #{"vaarassa"})))))
      (is (= [{:salaisuus "turvassa"}
              {:salaisuus "vaarassa"}] @walked-properties)))))

(deftest subschemas-test
  (let [with-forms (partial map #(update % :schema m/form))
        fn '(constantly true)]
    (testing "distinct :path"
      (is (= (->> [{:path [], :in [], :schema [:and
                                               [:map [:a int?] [:b [:set boolean?]] [:c [:vector [:and [:fn fn] [:map [:d string?]]]]]]
                                               [:fn fn]]}
                   {:path [0], :in [], :schema [:map [:a int?] [:b [:set boolean?]] [:c [:vector [:and [:fn fn] [:map [:d string?]]]]]]}
                   {:path [0 :a], :in [:a], :schema int?}
                   {:path [0 :b], :in [:b], :schema [:set boolean?]}
                   {:path [0 :b :malli.core/in], :in [:b :malli.core/in], :schema boolean?}
                   {:path [0 :c], :in [:c], :schema [:vector [:and [:fn fn] [:map [:d string?]]]]}
                   {:path [0 :c :malli.core/in], :in [:c :malli.core/in], :schema [:and [:fn fn] [:map [:d string?]]]}
                   {:path [0 :c :malli.core/in 0], :in [:c :malli.core/in], :schema [:fn fn]}
                   {:path [0 :c :malli.core/in 1], :in [:c :malli.core/in], :schema [:map [:d string?]]}
                   {:path [0 :c :malli.core/in 1 :d], :in [:c :malli.core/in :d], :schema string?}
                   {:path [1], :in [], :schema [:fn fn]}]
                  (with-forms))
             (->> [:and
                   [:map
                    [:a int?]
                    [:b [:set boolean?]]
                    [:c [:vector
                         [:and
                          [:fn fn]
                          [:map [:d string?]]]]]]
                   [:fn fn]]
                  (m/schema)
                  (mu/subschemas)
                  (with-forms))
             (->> [:and
                   [:map
                    [:a int?]
                    [:b [:set boolean?]]
                    [:c [:vector
                         [:and
                          [:fn fn]
                          [:map [:d string?]]]]]]
                   [:fn fn]]
                  (m/schema)
                  (mu/subschemas)
                  (mu/distinct-by :path)
                  (with-forms)))))

    (testing "distinct :in"
      (is (= (->> [{:path [], :in [], :schema [:and
                                               [:map
                                                [:a int?]
                                                [:b [:set boolean?]]
                                                [:c [:vector [:and
                                                              [:fn fn]
                                                              [:map [:d string?]]]]]]
                                               [:fn fn]]}
                   {:path [0 :a], :in [:a], :schema int?}
                   {:path [0 :b], :in [:b], :schema [:set boolean?]}
                   {:path [0 :b :malli.core/in], :in [:b :malli.core/in], :schema boolean?}
                   {:path [0 :c], :in [:c], :schema [:vector [:and [:fn fn] [:map [:d string?]]]]}
                   {:path [0 :c :malli.core/in], :in [:c :malli.core/in], :schema [:and [:fn fn] [:map [:d string?]]]}
                   {:path [0 :c :malli.core/in 1 :d], :in [:c :malli.core/in :d], :schema string?}]
                  (with-forms))
             (->> [:and
                   [:map
                    [:a int?]
                    [:b [:set boolean?]]
                    [:c [:vector [:and
                                  [:fn fn]
                                  [:map [:d string?]]]]]]
                   [:fn fn]]
                  (m/schema)
                  (mu/subschemas)
                  (mu/distinct-by :in)
                  (with-forms)))))))

(deftest in-path-conversions
  (testing "symmetry of things"
    (let [subschema (m/schema [:maybe int?])
          schema (m/schema [:maybe [:and [:map [:x [:maybe [:map [:y subschema]]]]]]])
          path [0 0 :x 0 :y]
          in [:x :y]]
      (is (= [path] (mu/in->paths schema in)))
      (is (= in (mu/path->in schema path)))
      (is (= subschema
             (mu/get-in schema path)
             (mu/get-in schema (first (mu/in->paths schema in)))))))
  (testing "multiple paths for in"
    (is (= [[0 0 :a :malli.core/in :b]
            [0 1 :a 0 :malli.core/in 0 :b]]
           (-> [:and
                [:or
                 [:map
                  [:a [:vector [:map [:b [:maybe int?]]]]]]
                 [:map
                  [:a [:maybe [:list [:maybe [:map [:b [:and [:or int?]]]]]]]]]]
                [:fn '(constantly true)]]
               (m/schema)
               (mu/in->paths [:a 0 :b]))))))

(deftest to-from-maps-test
  (let [schema [:map {:registry {::size [:enum "S" "M" "L"]}}
                [:id string?]
                [:tags [:set keyword?]]
                [:size ::size]
                [:address
                 [:vector
                  [:map
                   [:street string?]
                   [:lonlat [:tuple double? double?]]]]]]]

    (testing "to-map-syntax"
      (is (= {:type :map,
              :properties {:registry {::size [:enum "S" "M" "L"]}}
              :children [[:id nil {:type 'string?}]
                         [:tags nil {:type :set
                                     :children [{:type 'keyword?}]}]
                         [:size nil {:type ::m/schema
                                     :children [::size]}]
                         [:address nil {:type :vector,
                                        :children [{:type :map,
                                                    :children [[:street nil {:type 'string?}]
                                                               [:lonlat nil {:type :tuple
                                                                             :children [{:type 'double?}
                                                                                        {:type 'double?}]}]]}]}]]}
             (mu/to-map-syntax schema))))

    (testing "from-map-syntax"
      (is (true? (mu/equals schema (-> schema (mu/to-map-syntax) (mu/from-map-syntax))))))

    (testing "walking entries"
      (is (= {:type :map,
              :properties {:registry {::size [:enum "S" "M" "L"]}}
              :children [[:id nil {:type ::m/entry
                                   :children [{:type 'string?}]}]
                         [:tags nil {:type ::m/entry
                                     :children [{:type :set
                                                 :children [{:type 'keyword?}]}]}]
                         [:size nil {:type ::m/entry
                                     :children [{:type ::m/schema
                                                 :children [::size]}]}]
                         [:address nil {:type ::m/entry
                                        :children [{:type :vector,
                                                    :children [{:type :map,
                                                                :children [[:street nil {:type ::m/entry
                                                                                         :children [{:type 'string?}]}]
                                                                           [:lonlat nil {:type ::m/entry
                                                                                         :children [{:type :tuple
                                                                                                     :children [{:type 'double?}
                                                                                                                {:type 'double?}]}]}]]}]}]}]]}
             (mu/to-map-syntax schema {::m/walk-map-entries true}))))))
