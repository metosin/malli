(ns malli.util-test
  (:require [clojure.test :refer [deftest testing is are]]
            [malli.util :as mu]))

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
  (is (mu/equals (mu/get [:map [:x int?]] :x) int?))
  (is (mu/equals (mu/get [:map [:x {:optional true} int?]] :x) int?))
  (is (mu/equals (mu/get [:vector int?] 0) int?))
  (is (mu/equals (mu/get [:list int?] 0) int?))
  (is (mu/equals (mu/get [:set int?] 0) int?))
  (is (mu/equals (mu/get [:sequential int?] 0) int?))
  (is (mu/equals (mu/get [:or int? pos-int?] 1) pos-int?))
  (is (mu/equals (mu/get [:and int? pos-int?] 1) pos-int?))
  (is (mu/equals (mu/get [:tuple int? pos-int?] 1) pos-int?))
  (is (mu/equals (mu/get [:tuple int? pos-int?] 9 boolean?) boolean?))
  (is (mu/equals (mu/get [:map [:x int?]] :y boolean?) boolean?))
  (is (nil? (mu/get [:map [:x int?]] :y)))
  (is (= (mu/get [:map [:x int?]] :y)
         (mu/get [:vector int?] 1))))

(deftest get-in-test
  (is (mu/equals (mu/get-in
                  [:map
                   [:x [:vector
                        [:list
                         [:set
                          [:sequential
                           [:tuple int? [:map [:y [:maybe boolean?]]]]]]]]]]
                  [:x 0 0 0 0 1 :y 0])
                 boolean?))
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
  (is (nil? (mu/get-in
             [:map
              [:x [:vector
                   [:list
                    [:set
                     [:sequential
                      [:tuple int? [:map [:y [:maybe boolean?]]]]]]]]]]
             [:x 0 0 0 0 1 :y 9]))))

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
  (is (mu/equals (mu/assoc [:vector int?] 0 string?) [:vector string?]))
  (is (mu/equals (mu/assoc [:tuple int? int?] 1 string?) [:tuple int? string?]))
  (is (mu/equals (mu/assoc [:tuple int? int?] 2 string?) [:tuple int? int? string?]))
  (is (mu/equals (mu/assoc [:and int? int?] 1 string?) [:and int? string?]))
  (is (mu/equals (mu/assoc [:maybe int?] 0 string?) [:maybe string?]))
  (is (mu/equals (mu/assoc [:map [:x int?] [:y int?]] :x nil) [:map [:y int?]]))
  (let [schema [:map {:title "map"}
                [:a int?]
                [:b {:optional true} int?]
                [:c string?]]]
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
  (is (mu/equals (mu/update [:vector int?] 0 (constantly string?)) [:vector string?]))
  (is (mu/equals (mu/update [:tuple int? int?] 1 (constantly string?)) [:tuple int? string?]))
  (is (mu/equals (mu/update [:tuple int? int?] 2 (constantly string?)) [:tuple int? int? string?]))
  (is (mu/equals (mu/update [:and int? int?] 1 (constantly string?)) [:and int? string?]))
  (is (mu/equals (mu/update [:maybe int?] 0 (constantly string?)) [:maybe string?]))
  (is (mu/equals (mu/update [:map [:x int?] [:y int?]] :x (constantly nil)) [:map [:y int?]]))
  (let [schema [:map {:title "map"}
                [:a int?]
                [:b {:optional true} int?]
                [:c string?]]]
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
  (is (mu/equals (mu/assoc-in [:vector int?] [0] string?) [:vector string?]))
  (is (mu/equals (mu/assoc-in [:tuple int? int?] [1] string?) [:tuple int? string?]))
  (is (mu/equals (mu/assoc-in [:tuple int? int?] [2] string?) [:tuple int? int? string?]))
  (is (mu/equals (mu/assoc-in [:and int? int?] [1] string?) [:and int? string?]))
  (is (mu/equals (mu/assoc-in [:maybe int?] [0] string?) [:maybe string?]))
  (is (mu/equals (mu/assoc-in nil [:a [:b {:optional true}] :c :d] int?)
                 [:map [:a [:map [:b {:optional true} [:map [:c [:map [:d int?]]]]]]]]))
  (is (mu/equals (mu/assoc-in [:map] [:a :b :c :d] int?)
                 [:map [:a [:map [:b [:map [:c [:map [:d int?]]]]]]]])))

(deftest update-in-test
  (is (mu/equals (mu/update-in [:vector int?] [0] (constantly string?)) [:vector string?]))
  (is (mu/equals (mu/update-in [:tuple int? int?] [1] (constantly string?)) [:tuple int? string?]))
  (is (mu/equals (mu/update-in [:tuple int? int?] [2] (constantly string?)) [:tuple int? int? string?]))
  (is (mu/equals (mu/update-in [:and int? int?] [1] (constantly string?)) [:and int? string?]))
  (is (mu/equals (mu/update-in [:maybe int?] [0] (constantly string?)) [:maybe string?]))
  (is (mu/equals (mu/update-in nil [:a [:b {:optional true}] :c :d] (constantly int?))
                 [:map [:a [:map [:b {:optional true} [:map [:c [:map [:d int?]]]]]]]]))
  (is (mu/equals (mu/update-in [:map] [:a :b :c :d] (constantly int?))
                 [:map [:a [:map [:b [:map [:c [:map [:d int?]]]]]]]])))

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
