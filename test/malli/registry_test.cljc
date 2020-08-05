(ns malli.registry-test
  (:require [clojure.test :refer [deftest testing is]]
            [malli.core :as m]
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
