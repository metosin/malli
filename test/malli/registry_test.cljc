(ns malli.registry-test
  (:require [clojure.test :refer [deftest is testing]]
            [malli.core :as m]
            [malli.registry :as mr]))

(deftest mutable-test
  (let [registry* (atom (m/default-schemas))
        registry (mr/mutable-registry registry*)
        register! (fn [t ?s] (swap! registry* assoc t ?s))]
    (testing "default registy"
      (is (thrown? #?(:clj Exception, :cljs js/Error) (m/validate :str "kikka" {:registry registry})))
      (register! :str (m/-string-schema))
      (is (true? (m/validate :str "kikka" {:registry registry}))))
    (register! ::int-pair (m/schema [:tuple :int :int]))
    (is (thrown-with-msg?
          #?(:clj Exception, :cljs js/Error)
          #?(:clj #":malli\.core/infinitely-expanding-schema"
             :cljs #":malli\.core/invalid-schema")
          (m/schema [::int-pair {:foo :bar}] {:registry registry})))))

(deftest composite-test
  (let [registry* (atom {})
        register! (fn [t ?s] (swap! registry* assoc t ?s))
        registry (mr/composite-registry
                  {:map (m/-map-schema)}
                  (mr/mutable-registry registry*)
                  (mr/dynamic-registry))]

    ;; register
    (register! :maybe (m/-maybe-schema))

    ;; use
    (binding [mr/*registry* {:string (m/-string-schema)}]
      (is (true? (m/validate
                  [:map [:maybe [:maybe :string]]]
                  {:maybe "sheep"}
                  {:registry registry})))
      (is (= #{:string :map :maybe} (-> registry (mr/-schemas) (keys) (set)))))))

(deftest lazy-registry-test
  (let [loads (atom [])
        registry (mr/lazy-registry
                   (m/default-schemas)
                   (fn [type registry]
                     (let [lookup {"AWS::ApiGateway::UsagePlan" [:map {:closed true}
                                                                 [:Type [:= "AWS::ApiGateway::UsagePlan"]]
                                                                 [:Description {:optional true} string?]
                                                                 [:UsagePlanName {:optional true} string?]]
                                   "AWS::AppSync::ApiKey" [:map {:closed true}
                                                           [:Type [:= "AWS::AppSync::ApiKey"]]
                                                           [:ApiId string?]
                                                           [:Description {:optional true} string?]]}
                           schema (some-> type lookup (m/schema {:registry registry}))]
                       (swap! loads conj type)
                       schema)))
        new-loads! #(first (reset-vals! loads []))
        CloudFormation (m/schema [:multi {:lazy-refs true, :dispatch :Type}
                                  "AWS::ApiGateway::Stage"
                                  "AWS::ApiGateway::UsagePlan"
                                  "AWS::AppSync::ApiKey"]
                                 {:registry registry})]

    (testing "nothing is loaded"
      (is (= [] (new-loads!))))

    (testing "validating a schema pulls schema"
      (let [f (m/validator CloudFormation)]
        (is (= [] (new-loads!)))
        (is (f {:Type "AWS::AppSync::ApiKey"
                :ApiId "123"
                :Description "apkey"}))
        (is (= ["AWS::AppSync::ApiKey"] (new-loads!)))))

    (testing "pulling more"
      (let [f (m/validator CloudFormation)]
        (is (= [] (new-loads!)))
        (is (f {:Type "AWS::ApiGateway::UsagePlan"}))
        (is (= ["AWS::ApiGateway::UsagePlan"] (new-loads!)))))))

(deftest recursive-lazy-registry-test
  (let [loads (atom [])
        registry (mr/lazy-registry
                  (m/default-schemas)
                  (fn [type registry]
                    (let [lookup {::List [:multi {:lazy-refs true :dispatch :op}
                                          ::Nil
                                          ::Cons]
                                  ::Nil [:map [:op [:enum ::Nil]]]
                                  ::Cons [:map [:op [:enum ::Cons]] [:car :any] [:cdr ::List]]}
                          schema (some-> type lookup (m/schema {:registry registry}))]
                      (swap! loads conj type)
                      schema)))
        new-loads! #(first (reset-vals! loads []))
        List (m/schema ::List {:registry registry})]
    (testing "::Cons and ::Nil are lazily loaded"
      (is (= [::List] (new-loads!))))
    (testing "the first two levels are lazily pulled, then cached"
      (let [f (m/validator List)]
        (testing "nothing is pulled to create validator"
          (is (= [] (new-loads!))))
        (testing "validating the first ::Nil the first time pulls"
          (is (f {:op ::Nil}))
          (is (= [::Nil] (new-loads!))))
        (testing "validating the first ::Nil the second time is cached"
          (is (f {:op ::Nil}))
          (is (= [] (new-loads!))))
        (testing "validating the first ::Cons the first time pulls"
          (is (f {:op ::Cons
                  :car "first"
                  :cdr {:op ::Nil}}))
          (is (= [::Cons] (new-loads!))))
        (testing "validating the first ::Cons the second time is cached"
          (is (f {:op ::Cons
                  :car "first"
                  :cdr {:op ::Nil}}))
          (is (= [] (new-loads!))))
        (testing "not further pulls are needed"
          (testing "for nested good values"
            (is (f (nth (iterate
                          (fn [x]
                            {:op ::Cons
                             :car "elem"
                             :cdr x})
                          {:op ::Nil})
                        10)))
            (is (= [] (new-loads!))))
          (testing "for nested bad values"
            (is (not (f {:op ::Cons
                         :car "first"
                         :cdr {:op ::Cons
                               :car "second"
                               :cdr "junk"}})))
            (is (= [] (new-loads!)))))))))
