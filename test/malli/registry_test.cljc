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
  (let [loads (atom #{})
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
        CloudFormation (m/schema [:multi {:lazy-refs true, :dispatch :Type}
                                  "AWS::ApiGateway::Stage"
                                  "AWS::ApiGateway::UsagePlan"
                                  "AWS::AppSync::ApiKey"]
                                 {:registry registry})]

    (testing "nothing is loaded"
      (is (= 0 (count @loads))))

    (testing "validating a schema pulls schema"
      (is (true? (m/validate
                  CloudFormation
                  {:Type "AWS::AppSync::ApiKey"
                   :ApiId "123"
                   :Description "apkey"})))

      (is (= 1 (count @loads))))

    (testing "pulling more"
      (is (true? (m/validate
                  CloudFormation
                  {:Type "AWS::ApiGateway::UsagePlan"})))

      (is (= 2 (count @loads))))))
