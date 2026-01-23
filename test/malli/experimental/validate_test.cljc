(ns malli.experimental.validate-test
  (:require [clojure.test :refer [deftest is testing]]
            [malli.core :as m]
            [malli.error :as me]
            [malli.experimental.validate :as mev]))

(deftest test-validate
  (testing "simple case"
    (let [even-schema (m/schema [:validate (fn [x]
                                             (when-not (even? x)
                                               [{:in []
                                                 :type :not-even
                                                 :value x}]))]
                                {:registry (mev/schemas)})]
      (is (m/validate even-schema 4))
      (is (nil? (m/explain even-schema 4)))
      (is (not (m/validate even-schema 3)))
      (is (= [{:path []
               :in []
               :schema even-schema
               :value 3
               :type :not-even}]
             (:errors (m/explain even-schema 3))))))
  (testing "nested paths"
    (let [both-even-schema (m/schema [:validate (fn [x]
                                                  (if-not (map? x)
                                                    [{:in []
                                                      :type :not-map
                                                      :value x}]
                                                    (seq
                                                     (keep identity
                                                           [(when-not (even? (:a x))
                                                              {:in [:a]
                                                               :type :not-even
                                                               :value (:a x)})
                                                            (when-not (even? (:b x))
                                                              {:in [:b]
                                                               :type :not-even
                                                               :value (:b x)})]))))]
                                     {:registry (mev/schemas)})
          schema (m/schema [:map [:value both-even-schema]])]
      (is (m/validate schema {:value {:a 2 :b 4}}))
      (is (nil? (m/explain schema {:value {:a 2 :b 4}})))
      (is (not (m/validate schema {:value [2 4]})))
      (is (= [{:path [:value]
               :in [:value]
               :schema both-even-schema
               :value [2 4]
               :type :not-map}]
             (:errors (m/explain schema {:value [2 4]}))))
      (is (not (m/validate schema {:value {:a 3 :b 4}})))
      (is (= [{:path [:value]
               :in [:value :a]
               :schema both-even-schema
               :value 3
               :type :not-even}]
             (:errors (m/explain schema {:value {:a 3 :b 4}}))))
      (is (not (m/validate schema {:value {:a 2 :b 3}})))
      (is (= [{:path [:value]
               :in [:value :b]
               :schema both-even-schema
               :value 3
               :type :not-even}]
             (:errors (m/explain schema {:value {:a 2 :b 3}}))))
      (testing "multiple errors"
        (is (not (m/validate schema {:value {:a 3 :b 3}})))
        (is (= [{:path [:value]
                 :in [:value :a]
                 :schema both-even-schema
                 :value 3
                 :type :not-even}
                {:path [:value]
                 :in [:value :b]
                 :schema both-even-schema
                 :value 3
                 :type :not-even}]
               (:errors (m/explain schema {:value {:a 3 :b 3}})))))))
  (testing "humanize"
    (let [two-sub-errors (m/schema [:validate (fn [x]
                                                [{:in [:a]
                                                  :value (:a x)
                                                  :type :error-for-a}
                                                 {:in [:b]
                                                  :value (:b x)
                                                  :type :error-for-b}])]
                                   {:registry (mev/schemas)})
          schema (m/schema [:map [:value two-sub-errors]])
          value {:value {:a 1 :b "x"}}
          ]
      (is (not (m/validate schema value)))
      (is (= [{:path [:value]
               :in [:value :a]
               :schema two-sub-errors
               :value 1
               :type :error-for-a}
              {:path [:value]
               :in [:value :b]
               :schema two-sub-errors
               :value "x"
               :type :error-for-b}]
             (:errors (m/explain schema value))))
      (is (= {:value {:a ["unknown error"] :b ["unknown error"]}}
             (me/humanize (m/explain schema value))))
      (is (= {:value {:a ["a can not be!"] :b ["b can not be \"x\""]}}
             (me/humanize (m/explain schema value)
                          {:errors {:error-for-a {:error/message {:en "a can not be!"}}
                                    :error-for-b {:error/fn {:en (fn [{:keys [value]} _] (str "b can not be " (pr-str value)))}}}}))))))
