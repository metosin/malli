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
    (is (= expected (mu/simplify-map-entry entry)))

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

(deftest get-test
  (is (form= (mu/get (m/schema int?) 0) nil))
  (let [re #"kikka"]
    (is (form= (mu/get (m/schema [:re re]) 0) re)))
  (let [f int?]
    (is (form= (mu/get (m/schema [:fn f]) 0) f)))
  (is (form= (mu/get (m/schema [:string {:min 1}]) 0) nil))
  (is (form= (mu/get (m/schema [:enum "A" "B"]) 0) "A"))
  (is (mu/equals (mu/get (m/schema [:map [:x int?]]) :x) int?))
  (is (mu/equals (mu/get (m/schema [:map [:x {:optional true} int?]]) :x) int?))
  (is (mu/equals (mu/get (m/schema [:vector int?]) 0) int?))
  (is (mu/equals (mu/get (m/schema [:list int?]) 0) int?))
  (is (mu/equals (mu/get (m/schema [:set int?]) 0) int?))
  (is (mu/equals (mu/get (m/schema [:sequential int?]) 0) int?))
  (is (mu/equals (mu/get (m/schema [:or int? pos-int?]) 1) pos-int?))
  (is (mu/equals (mu/get (m/schema [:and int? pos-int?]) 1) pos-int?))
  (is (mu/equals (mu/get (m/schema [:tuple int? pos-int?]) 1) pos-int?))
  (is (form= (mu/get (m/schema [:map [:x int?]]) :y)
             (mu/get (m/schema [:maybe int?]) 1)
             nil))
  (is (mu/equals (mu/get (m/schema [:map-of int? pos-int?]) 0) int?))
  (is (mu/equals (mu/get (m/schema [:map-of int? pos-int?]) 1) pos-int?))
  (is (form= (mu/get (m/schema [:ref {:registry {::a int?, ::b string?}} ::a]) 0) ::a))
  (is (mu/equals (mu/get (m/schema [:schema int?]) 0) int?)))

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

(deftest assoc-test
  (let [re #"kikka"]
    (is (mu/equals (mu/assoc (m/schema [:re #"kukka"]) 0 re) [:re re])))
  (let [f 'int?]
    (is (mu/equals (mu/assoc (m/schema [:fn 'string?]) 0 f) [:fn f])))
  (is (mu/equals (mu/assoc (m/schema [:enum "A" "B"]) 1 "C") [:enum "A" "C"]))
  (is (mu/equals (mu/assoc (m/schema [:enum "A" "B"]) 2 "C") [:enum "A" "B" "C"]))
  (is (mu/equals (mu/assoc (m/schema [:vector int?]) 0 string?) [:vector string?]))
  (is (mu/equals (mu/assoc (m/schema [:tuple int? int?]) 1 string?) [:tuple int? string?]))
  (is (mu/equals (mu/assoc (m/schema [:tuple int? int?]) 2 string?) [:tuple int? int? string?]))
  (is (mu/equals (mu/assoc (m/schema [:and int? int?]) 1 string?) [:and int? string?]))
  (is (mu/equals (mu/assoc (m/schema [:maybe int?]) 0 string?) [:maybe string?]))
  (is (mu/equals (mu/assoc (m/schema [:map [:x int?] [:y int?]]) :x nil) [:map [:y int?]]))
  (is (mu/equals (mu/assoc (m/schema [:map-of int? int?]) 0 string?) [:map-of string? int?]))
  (is (mu/equals (mu/assoc (m/schema [:map-of int? int?]) 1 string?) [:map-of int? string?]))
  (is (mu/equals (mu/assoc (m/schema [:ref {:registry {::a int?, ::b string?}} ::a]) 0 ::b) [:ref {:registry {::a int?, ::b string?}} ::b]))
  (is (mu/equals (mu/assoc (m/schema [:schema int?]) 0 string?) [:schema string?]))

  (testing "invalid assoc throws"
    (are [schema i]
      (is (thrown? #?(:clj Exception, :cljs js/Error) (mu/assoc schema i string?)))

      nil :x
      int? 0
      [:string {:min 1}] 0
      [:vector int?] 1
      [:tuple int? int?] 3
      [:or int? int?] 3
      [:and int? int?] 3
      [:maybe int?] 1
      [:map-of int? int?] 2
      [:ref {:registry {::a int?}} ::a] 1
      [:schema int?] 1))

  (let [schema (m/schema
                 [:map {:title "map"}
                  [:a int?]
                  [:b {:optional true} int?]
                  [:c string?]])]
    (is (mu/equals (mu/assoc schema :a string?)
                   [:map {:title "map"}
                    [:a string?]
                    [:b {:optional true} int?]
                    [:c string?]]))
    (is (mu/equals (mu/assoc schema [:a {:optional true}] string?)
                   [:map {:title "map"}
                    [:a {:optional true} string?]
                    [:b {:optional true} int?]
                    [:c string?]]))
    (is (mu/equals (mu/assoc schema :b string?)
                   [:map {:title "map"}
                    [:a int?]
                    [:b string?]
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

(deftest path-schema-test
  (let [with-forms (partial map #(update % :schema m/form))]

    (testing "retains original order"
      (is (= (with-forms
               [{:path [],
                 :schema [:and
                          [:map
                           [:a int?]
                           [:b [:set boolean?]]
                           [:c [:vector
                                [:and
                                 [:fn '(constantly true)]
                                 [:map
                                  [:d string?]]]]]]
                          [:fn '(constantly true)]]}
                {:path [:a]
                 :schema int?}
                {:path [:b]
                 :schema [:set boolean?]}
                {:path [:b :malli.core/in]
                 :schema boolean?}
                {:path [:c]
                 :schema [:vector
                          [:and
                           [:fn '(constantly true)]
                           [:map
                            [:d string?]]]]}
                {:path [:c :malli.core/in]
                 :schema [:and
                          [:fn '(constantly true)]
                          [:map
                           [:d string?]]]}
                {:path [:c :malli.core/in :d]
                 :schema string?}])
             (with-forms
               (mu/path-schemas
                 [:and
                  [:map
                   [:a int?]
                   [:b [:set boolean?]]
                   [:c [:vector
                        [:and
                         [:fn '(constantly true)]
                         [:map
                          [:d string?]]]]]]
                  [:fn '(constantly true)]])))))))
