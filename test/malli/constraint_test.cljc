(ns malli.constraint-test
  (:require [clojure.string :as str]
            [clojure.test :refer [are deftest is testing]]
            [clojure.test.check.generators :as gen]
            [clojure.walk :as walk]
            [malli.core :as m]
            [malli.edn :as edn]
            [malli.generator :as mg]
            [malli.error :as me]
            [malli.impl.util :as miu]
            [malli.registry :as mr]
            [malli.test-utils :refer [with-schema-forms]]
            [malli.transform :as mt]
            [malli.util :as mu]
            #?(:clj [malli.test-macros :refer [when-env]]))
  #?(:clj  (:import (clojure.lang IFn PersistentArrayMap PersistentHashMap))
     :cljs (:require-macros [malli.test-macros :refer [when-env]])))

(def NonEmptyMapGroup
  [:map
   {:or [:a1 :a2]}
   [:a1 {:optional true} string?]
   [:a2 {:optional true} string?]])

(def UserPwGroups
  [:map
   {:and [[:or :secret [:and :user :pass]]
          [:disjoint [:secret] [:pass :user]]]}
   [:secret {:optional true} string?]
   [:user {:optional true} string?]
   [:pass {:optional true} string?]])

(def IffGroups
  [:map
   {:iff [:a1 :a2 :a3]}
   [:a1 {:optional true} string?]
   [:a2 {:optional true} string?]
   [:a3 {:optional true} string?]])

(def ImpliesGroups
  [:map
   {:implies [:a1 :a2 :a3]}
   [:a1 {:optional true} string?]
   [:a2 {:optional true} string?]
   [:a3 {:optional true} string?]])

(def XOrGroups
  [:map
   {:xor [:a1 :a2 :a3]}
   [:a1 {:optional true} string?]
   [:a2 {:optional true} string?]
   [:a3 {:optional true} string?]])

(def FlatNotGroup
  [:map
   {:not :a3}
   [:a1 {:optional true} string?]
   [:a2 {:optional true} string?]
   [:a3 {:optional true} string?]])

;; equivalent to [:implies :a3 :a1 :a2]
(def NotGroups
  [:map
   {:or [[:and :a1 :a2]
         [:not :a3]]}
   [:a1 {:optional true} string?]
   [:a2 {:optional true} string?]
   [:a3 {:optional true} string?]])

(deftest map-keyset-test
  (testing ":or"
    (testing "validate"
      (is (m/validate NonEmptyMapGroup {:a1 "a"}))
      (is (m/validate NonEmptyMapGroup {:a1 "a" :a2 "b"}))
      (is (m/validate NonEmptyMapGroup {:a1 "a" :a2 "b" :a3 "c"}))
      (is (not (m/validate NonEmptyMapGroup {}))))
    (testing "explain"
      (is (nil? (m/explain NonEmptyMapGroup {:a1 "a"})))
      (is (nil? (m/explain NonEmptyMapGroup {:a1 "a" :a2 "b"})))
      (is (nil? (m/explain NonEmptyMapGroup {:a1 "a" :a2 "b" :a3 "c"})))
      (is (= '{:schema [:map {:keyset [[:or :a1 :a2]]} [:a1 {:optional true} string?] [:a2 {:optional true} string?]]
               :value {}
               :errors ({:path []
                         :in []
                         :schema [:map {:keyset [[:or :a1 :a2]]} [:a1 {:optional true} string?] [:a2 {:optional true} string?]]
                         :value {}
                         :type :malli.core/constraint-violation
                         :message nil})}
             (with-schema-forms (m/explain NonEmptyMapGroup {}))))
      (is (= ["should provide at least one key: :a1 :a2"]
             (me/humanize (m/explain NonEmptyMapGroup {}))))))
  (testing ":disjoint"
    (testing "validate"
      (is (m/validate UserPwGroups {:secret "a"}))
      (is (m/validate UserPwGroups {:user "a"
                                    :pass "b"}))
      (is (not (m/validate UserPwGroups {:user "a"})))
      (is (not (m/validate UserPwGroups {})))
      (is (not (m/validate UserPwGroups {:secret "a"
                                         :user "b"})))
      (is (not (m/validate UserPwGroups {:secret "a"
                                         :user "b"
                                         :pass "c"}))))
    (testing "explain"
      (is (nil? (m/explain UserPwGroups {:secret "a"})))
      (is (nil? (m/explain UserPwGroups {:user "a"
                                         :pass "b"})))
      (is (= [{:path []
               :in []
               :schema (m/form UserPwGroups)
               :value {:user "a"}
               :type :malli.core/constraint-violation
               :message nil}]
            (:errors (with-schema-forms (m/explain UserPwGroups {:user "a"})))))
      (is (= ["either: 1). should provide key: :secret; or 2). should provide key: :pass"]
             (me/humanize (m/explain UserPwGroups {:user "a"}))))
      (is (= [{:path []
               :in []
               :schema (m/form UserPwGroups)
               :value {}
               :type :malli.core/constraint-violation
               :message nil}]
             (-> (m/explain UserPwGroups {})
                 with-schema-forms
                 :errors)))
      (is (= ["either: 1). should provide key: :secret; or 2). should provide keys: :user :pass"]
             (me/humanize (m/explain UserPwGroups {}))))
      (is (= [{:path []
               :in []
               :schema (m/form UserPwGroups)
               :value {:secret "a", :user "b"}
               :type :malli.core/constraint-violation
               :message nil}]
             (-> (m/explain UserPwGroups {:secret "a"
                                          :user "b"})
                 with-schema-forms
                 :errors)))
      (is (= ["should not combine key :secret with key: :user"]
             (me/humanize (m/explain UserPwGroups {:secret "a"
                                                   :user "b"}))))
      (is (= [{:path []
               :in []
               :schema (m/form UserPwGroups)
               :value {:secret "a", :user "b", :pass "c"}
               :type :malli.core/constraint-violation
               :message nil}]
             (-> (m/explain UserPwGroups {:secret "a"
                                          :user "b"
                                          :pass "c"})
                 with-schema-forms
                 :errors)))
      (is (= ["should not combine key :secret with keys: :pass :user"]
             (me/humanize (m/explain UserPwGroups {:secret "a"
                                                   :user "b"
                                                   :pass "c"}))))))
  (testing ":iff"
    (testing "validate"
      (is (m/validate IffGroups {}))
      (is (m/validate IffGroups {:a1 "a" :a2 "b" :a3 "c"}))
      (is (m/validate IffGroups {:a1 "a" :a2 "b" :a3 "c" :a4 "d"}))
      (is (not (m/validate IffGroups {:a1 "a" :a2 "b"})))
      (is (not (m/validate IffGroups {:a1 "a" :a3 "c"})))
      (is (not (m/validate IffGroups {:a2 "b" :a3 "c"})))
      (is (not (m/validate IffGroups {:a1 "a"})))
      (is (not (m/validate IffGroups {:a2 "b"})))
      (is (not (m/validate IffGroups {:a3 "c"}))))
    (testing "explain"
      (is (nil? (m/explain IffGroups {})))
      (is (nil? (m/explain IffGroups {:a1 "a" :a2 "b" :a3 "c"})))
      (is (nil? (m/explain IffGroups {:a1 "a" :a2 "b" :a3 "c" :a4 "d"})))
      (is (= ["should provide key: :a3"]
             (me/humanize (m/explain IffGroups {:a1 "a" :a2 "b"}))))
      (is (= ["should provide key: :a2"]
             (me/humanize (m/explain IffGroups {:a1 "a" :a3 "c"}))))
      (is (= ["should provide key: :a1"]
             (me/humanize (m/explain IffGroups {:a2 "b" :a3 "c"}))))
      (is (= ["should provide keys: :a2 :a3"]
             (me/humanize (m/explain IffGroups {:a1 "a"}))))
      (is (= ["should provide keys: :a1 :a3"]
             (me/humanize (m/explain IffGroups {:a2 "b"}))))
      (is (= ["should provide keys: :a1 :a2"]
             (me/humanize (m/explain IffGroups {:a3 "c"}))))))
  (testing ":implies"
    (testing "validate"
      (is (m/validate ImpliesGroups {}))
      (is (m/validate ImpliesGroups {:a1 "a" :a2 "b" :a3 "c"}))
      (is (m/validate ImpliesGroups {:a1 "a" :a2 "b" :a3 "c" :a4 "d"}))
      (is (not (m/validate ImpliesGroups {:a1 "a" :a2 "b"})))
      (is (not (m/validate ImpliesGroups {:a1 "a" :a3 "c"})))
      (is (m/validate ImpliesGroups {:a2 "b" :a3 "c"}))
      (is (not (m/validate ImpliesGroups {:a1 "a"})))
      (is (m/validate ImpliesGroups {:a2 "b"}))
      (is (m/validate ImpliesGroups {:a3 "c"})))
    (testing "explain"
      (is (nil? (m/explain ImpliesGroups {})))
      (is (nil? (m/explain ImpliesGroups {:a1 "a" :a2 "b" :a3 "c"})))
      (is (nil? (m/explain ImpliesGroups {:a1 "a" :a2 "b" :a3 "c" :a4 "d"})))
      (is (= ["should provide key: :a3"]
             (me/humanize (m/explain ImpliesGroups {:a1 "a" :a2 "b"}))))
      (is (= ["should provide key: :a2"]
             (me/humanize (m/explain ImpliesGroups {:a1 "a" :a3 "c"}))))
      (is (nil? (m/explain ImpliesGroups {:a2 "b" :a3 "c"})))
      (is (= ["should provide keys: :a2 :a3"]
             (me/humanize (m/explain ImpliesGroups {:a1 "a"}))))
      (is (nil? (m/explain ImpliesGroups {:a2 "b"})))
      (is (nil? (m/explain ImpliesGroups {:a3 "c"})))))
  (testing ":xor"
    (testing "validate"
      (is (not (m/validate XOrGroups {})))
      (is (not (m/validate XOrGroups {:a1 "a" :a2 "b" :a3 "c"})))
      (is (not (m/validate XOrGroups {:a1 "a" :a2 "b" :a3 "c" :a4 "d"})))
      (is (not (m/validate XOrGroups {:a1 "a" :a2 "b"})))
      (is (not (m/validate XOrGroups {:a1 "a" :a3 "c"})))
      (is (not (m/validate XOrGroups {:a2 "b" :a3 "c"})))
      (is (m/validate XOrGroups {:a1 "a"}))
      (is (m/validate XOrGroups {:a2 "b"}))
      (is (m/validate XOrGroups {:a3 "c"})))
    (testing "explain"
      (is (= ["should provide exactly one of the following keys: :a1 :a2 :a3"]
             (me/humanize (m/explain XOrGroups {}))))
      (is (= ["should provide exactly one of the following keys: :a1 :a2 :a3"]
             (me/humanize (m/explain XOrGroups {:a1 "a" :a2 "b" :a3 "c"}))))
      (is (= ["should provide exactly one of the following keys: :a1 :a2 :a3"]
             (me/humanize (m/explain XOrGroups {:a1 "a" :a2 "b" :a3 "c" :a4 "d"}))))
      (is (= ["should provide exactly one of the following keys: :a1 :a2"]
             (me/humanize (m/explain XOrGroups {:a1 "a" :a2 "b"}))))
      (is (= ["should provide exactly one of the following keys: :a1 :a3"]
             (me/humanize (m/explain XOrGroups {:a1 "a" :a3 "c"}))))
      (is (= ["should provide exactly one of the following keys: :a2 :a3"]
             (me/humanize (m/explain XOrGroups {:a2 "b" :a3 "c"}))))
      (is (nil? (m/explain XOrGroups {:a1 "a"})))
      (is (nil? (m/explain XOrGroups {:a2 "b"})))
      (is (nil? (m/explain XOrGroups {:a3 "c"})))))
  (testing ":not"
    (testing "validate"
      (is (m/validate FlatNotGroup {}))
      (is (m/validate FlatNotGroup {:a1 "b"}))
      (is (m/validate FlatNotGroup {:a1 "b" :a2 "c"}))
      (is (not (m/validate FlatNotGroup {:a1 "b" :a2 "c" :a3 "d"})))
      (is (m/validate NotGroups {}))
      (is (m/validate NotGroups {:a1 "a" :a2 "b" :a3 "c"}))
      (is (m/validate NotGroups {:a1 "a" :a2 "b" :a3 "c" :a4 "d"}))
      (is (m/validate NotGroups {:a1 "a" :a2 "b"}))
      (is (not (m/validate NotGroups {:a1 "a" :a3 "c"})))
      (is (not (m/validate NotGroups {:a2 "b" :a3 "c"})))
      (is (m/validate NotGroups {:a1 "a"}))
      (is (m/validate NotGroups {:a2 "b"}))
      (is (not (m/validate NotGroups {:a3 "c"}))))
    (testing "explain"
      (is (nil? (m/explain FlatNotGroup {})))
      (is (nil? (m/explain FlatNotGroup {:a1 "b"})))
      (is (nil? (m/explain FlatNotGroup {:a1 "b" :a2 "c"})))
      (is (= ["should not provide key: :a3"]
             (me/humanize (m/explain FlatNotGroup {:a1 "b" :a2 "c" :a3 "d"}))))
      (is (nil? (m/explain NotGroups {})))
      (is (nil? (m/explain NotGroups {:a1 "a" :a2 "b" :a3 "c"})))
      (is (nil? (m/explain NotGroups {:a1 "a" :a2 "b" :a3 "c" :a4 "d"})))
      (is (nil? (m/explain NotGroups {:a1 "a" :a2 "b"})))
      (is (= ["either: 1). should provide key: :a2; or 2). should not provide key: :a3"]
             (me/humanize (m/explain NotGroups {:a1 "a" :a3 "c"}))))
      (is (= ["either: 1). should provide key: :a1; or 2). should not provide key: :a3"]
             (me/humanize (m/explain NotGroups {:a2 "b" :a3 "c"}))))
      (is (nil? (m/explain NotGroups {:a1 "a"})))
      (is (nil? (m/explain NotGroups {:a2 "b"})))
      (is (= ["either: 1). should provide keys: :a1 :a2; or 2). should not provide key: :a3"]
             (me/humanize (m/explain NotGroups {:a3 "c"})))))))

(def Address
  [:map
   {:keyset [[:iff :street :city :zip]]}
   [:street {:optional true} string?]
   [:city {:optional true} string?]
   [:zip {:optional true} int?]])

(def GitOrMvn
  [:map {:xor [:mvn/version :git/sha]}
   [:mvn/version {:optional true} :string]
   [:git/sha {:optional true} :string]])

(def TagImpliesSha
  [:map {:implies [:git/tag :git/sha]}
   [:git/sha {:optional true} :string]
   [:git/tag {:optional true} :string]])

(def UserPass
  [:map {:iff [:user :pass]}
   [:user {:optional true} string?]
   [:pass {:optional true} string?]])

(def SeparateMvnGit
  [:map {:disjoint [[:mvn/version]
                    [:git/sha :git/url :git/tag]]}
   [:mvn/version {:optional true} :string]
   [:git/sha {:optional true} :string]
   [:git/tag {:optional true} :string]
   [:git/url {:optional true} :string]])

(def SecretOrCreds
  [:map {:or [:secret [:and :user :pass]]
         :disjoint [[:secret]
                    [:user :pass]]}
   [:secret {:optional true} string?]
   [:user {:optional true} string?]
   [:pass {:optional true} string?]])

(def DPad
  [:map {:keyset [[:disjoint [:down] [:up]]
                  [:disjoint [:left] [:right]]]}
   [:down {:optional true} [:= 1]]
   [:left {:optional true} [:= 1]]
   [:right {:optional true} [:= 1]]
   [:up {:optional true} [:= 1]]])

(def DPadNot
  [:map {:keyset [[:not [:and :down :up]]
                [:not [:and :left :right]]]}
   [:down {:optional true} [:= 1]]
   [:left {:optional true} [:= 1]]
   [:right {:optional true} [:= 1]]
   [:up {:optional true} [:= 1]]])

(def DPadDeMorgan
  [:map {:keyset [[:or [:not :down] [:not :up]]
                [:or [:not :left] [:not :right]]]}
   [:down {:optional true} [:= 1]]
   [:left {:optional true} [:= 1]]
   [:right {:optional true} [:= 1]]
   [:up {:optional true} [:= 1]]])

(def Padding
  [:map {:or [:top :bottom :left :right]}
   [:top {:optional true} number?]
   [:bottom {:optional true} number?]
   [:left {:optional true} number?]
   [:right {:optional true} number?]])

(deftest map-keyset-readme-examples-test
  (is (= (me/humanize
           (m/explain
             [:map {:keyset [:x]}]
             {}))
         ["should provide key: :x"]))
  (is (= (me/humanize
           (m/explain
             [:map {:keyset [[:contains nil]]}]
             {}))
         ["should provide key: nil"]))
  (is (= (me/humanize
           (m/explain
             [:map {:keyset [[:contains nil]
                             [:contains []]]}]
             {}))
         ["should provide keys: nil []"]))
  (is (= (me/humanize
           (m/explain
             [:map
              {:keyset [:x]}
              [:x {:optional true} :int]]
             {}))
         ["should provide key: :x"]))
  (is (= (me/humanize
           (m/explain
             [:map
              {:keyset [[:or :a1 :a2]]}
              [:a1 {:optional true} :string]
              [:a2 {:optional true} :string]]
             {}))
         ["should provide at least one key: :a1 :a2"]))
  (is (= (m/validate Address {})
         true))
  (is (= (me/humanize (m/explain Address {:zip 5555}))
         ["should provide keys: :street :city"]))
  (testing "GitOrMvn"
    (is (= (m/validate GitOrMvn {:mvn/version "1.0.0"})
           true))
    (is (= (me/humanize
             (m/explain GitOrMvn
                        {:mvn/version "1.0.0"
                         :git/sha "123"}))
           ["should provide exactly one of the following keys: :mvn/version :git/sha"]))
    (is (= (me/humanize
             (m/explain GitOrMvn
                        {}))
           ["should provide exactly one of the following keys: :mvn/version :git/sha"])))
  (testing "TagImpliesSha"
    (is (= (m/validate TagImpliesSha {:git/sha "abc123"})
           true))
    (is (= (m/validate TagImpliesSha {:git/tag "v1.0.0" :git/sha "abc123"})
           true))
    (is (= (me/humanize
             (m/explain TagImpliesSha {:git/tag "v1.0.0"}))
           ["should provide key: :git/sha"])))
  (testing "UserPass"
    (is (= (m/validate UserPass {})
           true))
    (is (= (m/validate UserPass {:user "a" :pass "b"})
           true))
    (is (= (me/humanize
             (m/explain UserPass {:user "a"}))
           ["should provide key: :pass"])))
  (testing "SeparateMvnGit"
    (is (= (m/validate SeparateMvnGit {})
           true))
    (is (= (m/validate SeparateMvnGit {:mvn/version "1.0.0"})
           true))
    (is (= (m/validate SeparateMvnGit {:git/sha "1.0.0"})
           true))
    (is (= (me/humanize
             (m/explain SeparateMvnGit
                        {:mvn/version "1.0.0"
                         :git/sha "abc123"}))
           ["should not combine key :mvn/version with key: :git/sha"])))
  (testing "SecretOrCreds"
    (is (= (m/validate SecretOrCreds {:secret "1234"})
           true))
    (is (= (m/validate SecretOrCreds {:user "user" :pass "hello"})
           true))
    (is (= (me/humanize
             (m/explain SecretOrCreds {:user "user"}))
           ["either: 1). should provide key: :secret; or 2). should provide key: :pass"]))
    (is (= (me/humanize
             (m/explain SecretOrCreds {:secret "1234" :user "user"}))
           ["should not combine key :secret with key: :user"])))

  (testing "DPad"
    (doseq [DPad [DPad DPadNot DPadDeMorgan]]
      (is (m/validate DPad {}))
      (is (m/validate DPad {:up 1}))
      (is (m/validate DPad {:down 1}))
      (is (m/validate DPad {:right 1}))
      (is (m/validate DPad {:left 1}))
      (is (m/validate DPad {:up 1 :left 1}))
      (is (m/validate DPad {:down 1 :left 1}))
      (is (m/validate DPad {:up 1 :right 1}))
      (is (m/validate DPad {:down 1 :right 1})))
    (doseq [DPad [DPadNot DPadDeMorgan]]
      (is (= (me/humanize
               (m/explain DPad {:up 1 :down 1}))
             ["either: 1). should not provide key: :down; or 2). should not provide key: :up"]))
      (is (= (me/humanize
               (m/explain DPad {:left 1 :right 1}))
             ["either: 1). should not provide key: :left; or 2). should not provide key: :right"])))
    (is (= (me/humanize
             (m/explain DPad {:up 1 :down 1}))
           ["should not combine key :down with key: :up"]))
    (is (= (me/humanize
             (m/explain DPad {:left 1 :right 1}))
           ["should not combine key :left with key: :right"])))
  (testing "Padding"
    (is (m/validate Padding {:left 1 :right 10 :up 25 :down 50}))
    (is (= (me/humanize
             (m/explain Padding {}))
           ["should provide at least one key: :top :bottom :left :right"]))
    (is (mg/sample Padding {:size 5}))))

(def OpenSetAB [:set {:or [:a :b]} :keyword])
(def ClosedSetAB [:set {:or [:a :b]} [:enum :a :b]])

(deftest set-keyset-test
  (testing "OpenSetAB"
    (is (m/validate      OpenSetAB #{:a}))
    (is (m/validate      OpenSetAB #{:a :z}))
    (is (m/validate      OpenSetAB #{:b}))
    (is (m/validate      OpenSetAB #{:b :z}))
    (is (not (m/validate OpenSetAB #{:z})))
    (is (not (m/validate OpenSetAB #{}))))

  (testing "ClosedSetAB"
    (is (m/validate      ClosedSetAB #{:a}))
    (is (not (m/validate ClosedSetAB #{:a :z})))
    (is (m/validate      ClosedSetAB #{:b}))
    (is (not (m/validate ClosedSetAB #{:b :z})))
    (is (not (m/validate ClosedSetAB #{:z})))
    (is (not (m/validate ClosedSetAB #{}))))

  (is (not (m/validate [:set {:or [:a :b]
                              :min 2}
                        [:enum :a :b]]
                       #{:a})))
  (is (m/validate [:set {:or [:a :b]
                         :min 2}
                   [:enum :a :b]]
                  #{:a :b}))
  ;;TODO are there satisfiable sets that cannot be generated?
  ;; e.g., allowing keys in the child but not telling the keyset
  ;; perhaps :optional [:b] property key to inform generator?
  #_
  (mg/sample [:set {:keyset [:a]
                    :min 2}
              [:enum :a :b]]
             #{:a :b}))

(def OpenMapOfAB [:map-of {:or [:a :b]} keyword? :any])
(def ClosedMapOfAB [:map-of {:or [:a :b]} [:enum :a :b] :any])

(deftest map-of-keyset-test
  (testing "OpenMapOfAB"
    (is (m/validate      OpenMapOfAB {:a 0}))
    (is (m/validate      OpenMapOfAB {:a 0 :z 0}))
    (is (m/validate      OpenMapOfAB {:b 0}))
    (is (m/validate      OpenMapOfAB {:b 0 :z 0}))
    (is (not (m/validate OpenMapOfAB {:z 0})))
    (is (not (m/validate OpenMapOfAB {}))))

  (testing "ClosedMapOfAB"
    (is (m/validate      ClosedMapOfAB {:a 0}))
    (is (not (m/validate ClosedMapOfAB {:a 0 :z 0})))
    (is (m/validate      ClosedMapOfAB {:b 0}))
    (is (not (m/validate ClosedMapOfAB {:b 0 :z 0})))
    (is (not (m/validate ClosedMapOfAB {:z 0})))
    (is (not (m/validate ClosedMapOfAB {}))))

  ;;TODO are there satisfiable sets that cannot be generated?
  ;; e.g., allowing keys in the child but not telling the keyset
  ;; perhaps :optional [:b] property key to inform generator?
  #_
  (mg/sample [:set {:keyset [:a]
                    :min 2}
              [:enum :a :b]]
             #{:a :b}))

(deftest number-constraint-test
  (is (m/validate [:int {:> 5 :< 10}] 8))
  (is (not (m/validate [:int {:> 5 :< 10}] 2)))
  (is (not (m/validate [:int {:> 5 :< 10}] 5)))
  (is (m/validate [:int {:=> 5 :< 10}] 5))
  (is (not (m/validate [:int {:> 5 :< 10}] 10)))
  (is (m/validate [:int {:> 5 :<= 10}] 10))
  (testing "nested constraints"
    (is (m/validate [:int {:and [[:> 5] [:< 10]]}] 8))
    (is (not (m/validate [:int {:and [[:> 5] [:< 10]]}] 11)))
    (is (m/validate [:int {:not [:and [:> 5] [:< 10]]}] 4))
    (is (m/validate [:int {:not [:and [:> 5] [:< 10]]}] 11))
    (is (not (m/validate [:int {:not [:> 5]}] 6)))
    (is (m/validate [:int {:not [:> 5]}] 5))
    (is (m/validate [:int {:not [:> 5]}] 4)))
  (testing ":gen ignored"
    (is (m/validate [:int {:gen/> 5 :< 10}] 3))
    (is (not (m/validate [:int {:gen/> 5 :< 10}] 11)))
    (is (m/validate [:int {:and [[:gen/> 5] [:< 10]]}] 8))
    (is (not (m/validate [:int {:and [[:gen/> 5] [:< 10]]}] 11)))
    (is (m/validate [:int {:and [[:gen/> 5] [:< 10]]}] 4))))

(deftest vector-constraint-test
  (is (m/validate [:sequential {:distinct true} :any] [1 2 3]))
  (is (not (m/explain [:sequential {:distinct true} :any] [1 2 3])))
  (is (not (m/validate [:sequential {:distinct true} :any] [1 3 3])))
  (is (= ["should be distinct: 3 provided 2 times"]
         (me/humanize (m/explain [:sequential {:distinct true} :any] [1 3 3]))))

  (is (m/validate [:sequential {:sorted true} :any] [1 2 3]))
  (is (not (m/explain [:sequential {:sorted true} :any] [1 2 3])))
  (is (not (m/validate [:sequential {:sorted true} :any] [3 2 1])))
  (is (= ["should be sorted: index 0 has 3 but expected 1"]
         (me/humanize (m/explain [:sequential {:sorted true} :any] [3 2 1]))))

  (is (= ["should be distinct: 2 provided 2 times"]
         (me/humanize (m/explain [:sequential {:sorted true :distinct true} :any] [1 2 2 3]))))

  (is (= ["should be distinct: 3 provided 2 times"
          "should be sorted: index 1 has 3 but expected 2"]
         (me/humanize (m/explain
                        [:sequential {:sorted true
                                      :distinct true} :any]
                        [1 3 3 2])))))

(deftest string-constraint-test
  (testing ":min/:max"
    (is (m/validate [:string {:min 1 :max 5}] "ab"))
    (is (not (m/validate [:string {:min 1 :max 5}] "")))
    (is (= ["should be at least 1 character, given 0"]
           (me/humanize (m/explain [:string {:min 1}] ""))
           (me/humanize (m/explain [:string {:min 1 :max 10}] ""))
           (me/humanize (m/explain [:string {:and [[:min 1]]}] ""))))
    (is (= ["should be at least 2 characters, given 0"]
           (me/humanize (m/explain [:string {:min 2}] ""))
           (me/humanize (m/explain [:string {:min 2 :max 10}] ""))
           (me/humanize (m/explain [:string {:and [[:min 2]]}] ""))))
    (is (= ["should be at most 1 character, given 2"]
           (me/humanize (m/explain [:string {:max 1}] "12"))
           (me/humanize (m/explain [:string {:min 0 :max 1}] "12"))
           (me/humanize (m/explain [:string {:and [[:max 1]]}] "12"))))
    (is (= ["should be at most 2 characters, given 3"]
           (me/humanize (m/explain [:string {:max 2}] "123"))
           (me/humanize (m/explain [:string {:min 1 :max 2}] "123"))
           (me/humanize (m/explain [:string {:and [[:max 2]]}] "123"))))
    (is (= ["should be less than 1 character, given 1"]
           (me/humanize (m/explain [:string {:not [:min 1]}] "a"))
           ))
    ;;FIXME
    #_(is (me/humanize (m/explain [:string {:not [:and [:min 1] [:max 2]]}] "a")))
    (is (= ["should be more than 1 character, given 0"]
           (me/humanize (m/explain [:string {:not [:max 1]}] "")))))
  (testing ":alpha"
    (is (m/validate [:string {:alpha true}] "ab"))
    (is (m/validate [:string {:alpha true}] ""))
    (is (not (m/validate [:string {:alpha true}] "ab1")))
    (is (not (m/validate [:string {:not [:alpha]}] "ab")))
    (is (m/validate [:string {:not [:alpha]}] "ab1"))
    (is (m/validate [:string {:not [:alpha]}] "1"))
    (is (not (m/validate [:string {:not [:alpha]}] "")))
    (is (= ["should contain a non-alphabetic character"]
           (me/humanize (m/explain [:string {:not [:alpha]}] "ab"))
           (me/humanize (m/explain [:string {:not [:alpha]}] "")))))
  (testing ":non-alpha"
    (is (m/validate [:string {:non-alpha true}] "12"))
    (is (m/validate [:string {:non-alpha true}] ""))
    (is (not (m/validate [:string {:non-alpha true}] "a2")))
    (is (not (m/validate [:string {:not [:non-alpha]}] "12")))
    (is (= ["should not contain alphabetic characters: index 0 has \\a."
            "should not contain alphabetic characters: index 1 has \\b."
            "should not contain alphabetic characters: index 3 has \\c."]
           (me/humanize (m/explain [:string {:non-alpha true}] "ab1c*"))))
    (is (= ["should contain an alphabetic character"]
           (me/humanize (m/explain [:string {:not [:non-alpha]}] "12")))))
  (testing ":numeric"
    (is (m/validate [:string {:numeric true}] ""))
    (is (m/validate [:string {:numeric true}] "12"))
    (is (not (m/validate [:string {:numeric true}] "1a2")))
    (is (= ["should be numeric: index 0 has \\a."
            "should be numeric: index 1 has \\b."
            "should be numeric: index 3 has \\c." "should be numeric: index 4 has \\*."]
           (me/humanize (m/explain [:string {:numeric true}] "ab1c*"))))
    (is (not (m/validate [:string {:not [:numeric]}] "")))
    (is (not (m/validate [:string {:not [:numeric]}] "12")))
    (is (m/validate [:string {:not [:numeric]}] "1a2"))
    (is (= ["should contain a non-numeric character"]
           (me/humanize (m/explain [:string {:not [:numeric]}] ""))
           (me/humanize (m/explain [:string {:not [:numeric]}] "123")))))
  (testing ":non-numeric"
    (is (m/validate [:string {:non-numeric true}] ""))
    (is (m/validate [:string {:non-numeric true}] "abc"))
    (is (m/validate [:string {:non-numeric true}] "abc]["))
    (is (not (m/validate [:string {:non-numeric true}] "12")))
    (is (not (m/validate [:string {:non-numeric true}] "1a2")))
    (is (= ["should not contain numeric characters: index 0 has \\1."
            "should not contain numeric characters: index 2 has \\2."]
           (me/humanize (m/explain [:string {:non-numeric true}] "1a2"))))
    (is (not (m/validate [:string {:not [:non-numeric]}] "")))
    (is (not (m/validate [:string {:not [:non-numeric]}] "abc")))
    (is (not (m/validate [:string {:not [:non-numeric]}] "abc][")))
    (is (m/validate [:string {:not [:non-numeric]}] "12"))
    (is (m/validate [:string {:not [:non-numeric]}] "1a2"))
    (is (= ["should contain a numeric character"]
           (me/humanize (m/explain [:string {:not [:non-numeric]}] ""))
           (me/humanize (m/explain [:string {:not [:non-numeric]}] "abc")))))
  (testing ":alphanumeric"
    (is (m/validate [:string {:alphanumeric true}] ""))
    (is (m/validate [:string {:alphanumeric true}] "12"))
    (is (m/validate [:string {:alphanumeric true}] "12ab"))
    (is (not (m/validate [:string {:alphanumeric true}] "[a1]")))
    (is (= ["should be alphanumeric: index 0 has \\[."
            "should be alphanumeric: index 3 has \\]."]
           (me/humanize (m/explain [:string {:alphanumeric true}] "[a1]"))))
    (is (not (m/validate [:string {:not [:alphanumeric]}] "")))
    (is (not (m/validate [:string {:not [:alphanumeric]}] "12")))
    (is (not (m/validate [:string {:not [:alphanumeric]}] "12ab")))
    (is (m/validate [:string {:not [:alphanumeric]}] "[a1]"))
    (is (= ["should contain a non-alphanumeric character"]
           (me/humanize (m/explain [:string {:not [:alphanumeric]}] "12")))))
  (testing ":non-alphanumeric"
    (is (m/validate [:string {:non-alphanumeric true}] ""))
    (is (m/validate [:string {:non-alphanumeric true}] "[]"))
    (is (not (m/validate [:string {:non-alphanumeric true}] "[12]")))
    (is (not (m/validate [:string {:non-alphanumeric true}] "[ab]")))
    (is (not (m/validate [:string {:non-alphanumeric true}] "12")))
    (is (= ["should not contain alphanumeric characters: index 1 has \\a."
            "should not contain alphanumeric characters: index 2 has \\1."]
           (me/humanize (m/explain [:string {:non-alphanumeric true}] "[a1]"))))
    (is (not (m/validate [:string {:not [:non-alphanumeric]}] "")))
    (is (not (m/validate [:string {:not [:non-alphanumeric]}] "[]")))
    (is (m/validate [:string {:not [:non-alphanumeric]}] "[12]"))
    (is (m/validate [:string {:not [:non-alphanumeric]}] "[ab]"))
    (is (m/validate [:string {:not [:non-alphanumeric]}] "12"))
    (is (= ["should contain an alphanumeric character"]
           (me/humanize (m/explain [:string {:not [:non-alphanumeric]}] ""))
           (me/humanize (m/explain [:string {:not [:non-alphanumeric]}] "[]")))))
  (testing ":sorted"
    (is (m/validate [:string {:sorted true}] "[]"))
    (is (not (m/validate [:string {:sorted true}] "][")))
    (is (not (m/validate [:string {:distinct true}] "abcba")))
    (is (= ["should be sorted: index 0 has \\] but expected \\["]
           (me/humanize (m/explain [:string {:sorted true}] "][")))))
  (testing ":distinct"
    (is (m/validate [:string {:distinct true}] "[]"))
    (is (not (m/validate [:string {:distinct true}] "[[")))
    (is (m/validate [:string {:distinct true}] "abcde"))
    (is (= ["should be distinct: \\[ provided 2 times"]
           (me/humanize (m/explain [:string {:distinct true}] "[[")))))
  (testing ":palindrome"
    (is (not (m/validate [:string {:palindrome true}] "[]")))
    (is (m/validate [:string {:palindrome true}] "[["))
    (is (m/validate [:string {:palindrome true}] ""))
    (is (m/validate [:string {:palindrome true}] "a"))
    (is (m/validate [:string {:palindrome true}] "abcba"))
    (is (not (m/validate [:string {:palindrome true}] "abcbab")))
    (is (= ["should be a palindrome"]
           (me/humanize (m/explain [:string {:palindrome true}] "[]"))
           (me/humanize (m/explain [:string {:palindrome true}] "abab"))))
    (is (= ["should not be a palindrome"]
           (me/humanize (m/explain [:string {:not [:palindrome]}] ""))
           (me/humanize (m/explain [:string {:not [:palindrome]}] "ababa")))))
  (testing ":trim"
    (is (m/validate [:string {:trim true}] "[]"))
    (is (not (m/validate [:string {:trim true}] "  a")))
    (is (not (m/validate [:string {:trim true}] "a  ")))
    (is (not (m/validate [:string {:trim true}] " a ")))
    (is (= ["should not have leading whitespace"]
           (me/humanize (m/explain [:string {:trim true}] " a"))))
    (is (= ["should not have trailing whitespace"]
           (me/humanize (m/explain [:string {:trim true}] "a "))))
    (is (= ["should not have leading whitespace"
            "should not have trailing whitespace"]
           (me/humanize (m/explain [:string {:trim true}] " a "))))
    (is (= ["should have leading or trailing whitespace"]
           (me/humanize (m/explain [:string {:not [:trim]}] ""))
           (me/humanize (m/explain [:string {:not [:trim]}] "abc")))))
  (testing ":triml"
    (is (m/validate [:string {:triml true}] "[]"))
    (is (not (m/validate [:string {:triml true}] "  a")))
    (is (m/validate [:string {:triml true}] "a  "))
    (is (not (m/validate [:string {:triml true}] " a ")))
    (is (= ["should not have leading whitespace"]
           (me/humanize (m/explain [:string {:triml true}] " a"))))
    (is (= ["should not have leading whitespace"]
           (me/humanize (m/explain [:string {:triml true}] " a "))))
    (is (= ["should have leading whitespace"]
           (me/humanize (m/explain [:string {:not [:triml]}] ""))
           (me/humanize (m/explain [:string {:not [:triml]}] "abc"))
           (me/humanize (m/explain [:string {:not [:triml]}] "abc ")))))
  (testing ":trimr"
    (is (m/validate [:string {:trimr true}] "[]"))
    (is (m/validate [:string {:trimr true}] "  a"))
    (is (not (m/validate [:string {:trimr true}] "a  ")))
    (is (not (m/validate [:string {:trimr true}] " a ")))
    (is (= ["should not have trailing whitespace"]
           (me/humanize (m/explain [:string {:trimr true}] "a "))))
    (is (= ["should not have trailing whitespace"]
           (me/humanize (m/explain [:string {:trimr true}] " a "))))
    (is (= ["should have trailing whitespace"]
           (me/humanize (m/explain [:string {:not [:trimr]}] "")))))
  (testing ":trim-newline"
    (is (m/validate [:string {:trim-newline true}] ""))
    (is (m/validate [:string {:trim-newline true}] "[]"))
    (is (m/validate [:string {:trim-newline true}] "  a"))
    (is (m/validate [:string {:trim-newline true}] "a  "))
    (is (not (m/validate [:string {:trim-newline true}] "a\n")))
    (is (= ["should not have trailing newline"]
           (me/humanize (m/explain [:string {:trim-newline true}] "a\n"))))
    (is (= ["should have trailing newline"]
           (me/humanize (m/explain [:string {:not [:trim-newline]}] "")))))
  (testing ":blank"
    (is (m/validate [:string {:blank true}] ""))
    (is (m/validate [:string {:blank true}] "   \n   "))
    (is (not (m/validate [:string {:blank true}] nil)))
    (is (not (m/validate [:string {:blank true}] [\space])))
    (is (not (m/validate [:string {:blank true}] "abc")))
    (is (not (m/validate [:string {:blank true}] "  a")))
    (is (not (m/validate [:string {:blank true}] "a  ")))
    (is (not (m/validate [:string {:blank true}] "a\n")))
    (is (= ["should be blank"]
           (me/humanize (m/explain [:string {:blank true}] "a\nab"))))
    (is (= ["should not be blank"]
           (me/humanize (m/explain [:string {:not [:blank]}] ""))
           (me/humanize (m/explain [:string {:not [:blank]}] "  \n "))))
    (is (= ["invalid type"]
           (me/humanize (m/explain [:string {:blank true}] nil)))))
  (testing ":non-blank"
    (is (not (m/validate [:string {:non-blank true}] "")))
    (is (not (m/validate [:string {:non-blank true}] "   \n   ")))
    (is (not (m/validate [:string {:non-blank true}] nil)))
    (is (not (m/validate [:string {:non-blank true}] [\space])))
    (is (m/validate [:string {:non-blank true}] "abc"))
    (is (m/validate [:string {:non-blank true}] "  a"))
    (is (m/validate [:string {:non-blank true}] "a  "))
    (is (m/validate [:string {:non-blank true}] "a\n"))
    (is (= ["should not be blank"]
           (me/humanize (m/explain [:string {:non-blank true}] "   "))
           (me/humanize (m/explain [:string {:non-blank true}] ""))))
    (is (= ["should be blank"]
           (me/humanize (m/explain [:string {:not [:non-blank]}] "a"))
           (me/humanize (m/explain [:string {:not [:non-blank]}] "a"))))
    (is (= ["invalid type"]
           (me/humanize (m/explain [:string {:non-blank true}] nil)))))
  (testing ":escapes"
    (is (m/validate [:string {:escapes {\c "b"}}] "b"))
    (is (not (m/validate [:string {:escapes {\c "b"}}] "c")))
    (is (thrown-with-msg?
          #?(:clj Exception, :cljs js/Error)
          #":malli.constraint.string.validate/escape-constraint-map-cannot-overlap-keys-vals"
          (m/validator [:string {:escapes {\c "c"}}])))
    (is (= ["should escape character \\c"]
           (me/humanize (m/explain [:string {:escapes {\c "b"}}] "c"))))
    (is (= ["should include at least one unescaped character: \\c"]
           (me/humanize (m/explain [:string {:not [:escapes {\c "b"}]}] "b")))))
  (testing ":includes"
    (is (m/validate [:string {:includes "foo"}] "foobar"))
    (is (not (m/validate [:string {:includes "foo"}] "oofar")))
    (is (= ["should include substring \"foo\""]
           (me/humanize (m/explain [:string {:includes "foo"}] "oobar"))))
    (is (= ["should not include substring \"foo\""]
           (me/humanize (m/explain [:string {:not [:includes "foo"]}] "foobar")))))
)
