(ns malli.registry-test
  (:require [clojure.test :refer [deftest is testing]]
            [malli.core :as m]
            [malli.generator :as mg]
            [malli.registry :as mr]))

(deftest mutable-test
  (let [registry* (atom {})
        registry (mr/mutable-registry registry*)
        register! (fn [t ?s] (swap! registry* assoc t ?s))]
    (testing "default registy"
      (is (thrown? #?(:clj Exception, :cljs js/Error) (m/validate :str "kikka" {:registry registry})))
      (register! :str (m/-string-schema))
      (is (true? (m/validate :str "kikka" {:registry registry}))))))

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

(def UserId :string)

(def User
  [:map
   [:id #'UserId]
   [:friends {:optional true} [:set [:ref #'User]]]])

(deftest var-registry-test
  (let [var-registry (mr/var-registry)
        registry (mr/composite-registry
                  (m/default-schemas)
                  var-registry)
        schema (m/schema User {:registry registry})]
    (testing "getting schema over Var works"
      (is (= UserId (mr/-schema var-registry #'UserId)))
      (is (= User (mr/-schema var-registry #'User))))
    (testing "we do not list all Var schemas (yet)"
      (is (= nil (mr/-schemas var-registry))))
    (testing "works!"
      (is (= [:map
              [:id #'malli.registry-test/UserId]
              [:friends {:optional true} [:set [:ref #'malli.registry-test/User]]]]
             (m/form schema)))
      (is (every? (m/validator schema) (mg/sample schema {:seed 100}))))))

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
