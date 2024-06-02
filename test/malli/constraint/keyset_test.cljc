(ns malli.constraint.keyset-test
  (:require [clojure.string :as str]
            [clojure.test :refer [are deftest is testing]]
            [clojure.test.check.generators :as gen]
            [clojure.walk :as walk]
            [malli.core :as m]
            [malli.constraint.string.util :refer [code-point-offset-seq]]
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
      (is (= '{:schema [:map {:or [:a1 :a2]} [:a1 {:optional true} string?] [:a2 {:optional true} string?]]
               :value {}
               :errors ({:path []
                         :in []
                         :schema [:map {:or [:a1 :a2]} [:a1 {:optional true} string?] [:a2 {:optional true} string?]]
                         :value {}
                         :type :malli.core/constraint-violation
                         :message nil})}
             (with-schema-forms (m/explain NonEmptyMapGroup {}))))
      (is (= [[:or
               "should provide key: :a1"
               "should provide key: :a2"]]
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
      (is (= [[:or
               "should provide key: :secret"
               "should provide key: :pass"]]
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
      (is (= [[:or
               "should provide key: :secret"
               [:and
                "should provide key: :user"
                "should provide key: :pass"]]]
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
      (is (= [[:xor
               "should provide key: :a3"
               [:and
                "should not provide key: :a1"
                "should not provide key: :a2"]]]
             (me/humanize (m/explain IffGroups {:a1 "a" :a2 "b"}))))
      (is (= [[:xor
               "should provide key: :a2"
               [:and
                "should not provide key: :a1"
                "should not provide key: :a3"]]]
             (me/humanize (m/explain IffGroups {:a1 "a" :a3 "c"}))))
      (is (= [[:xor
               "should provide key: :a1"
               [:and
                "should not provide key: :a2"
                "should not provide key: :a3"]]]
             (me/humanize (m/explain IffGroups {:a2 "b" :a3 "c"}))))
      (is (= [[:xor
               [:and
                "should provide key: :a2"
                "should provide key: :a3"]
               "should not provide key: :a1"]]
             (me/humanize (m/explain IffGroups {:a1 "a"}))))
      (is (= [[:xor
               [:and
                "should provide key: :a1"
                "should provide key: :a3"]
               "should not provide key: :a2"]]
             (me/humanize (m/explain IffGroups {:a2 "b"}))))
      (is (= [[:xor
               [:and
                "should provide key: :a1"
                "should provide key: :a2"]
               "should not provide key: :a3"]]
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
      (is (= [["should provide key: :a3"]]
             (me/humanize (m/explain ImpliesGroups {:a1 "a" :a2 "b"}))))
      (is (= [["should provide key: :a2"]]
             (me/humanize (m/explain ImpliesGroups {:a1 "a" :a3 "c"}))))
      (is (nil? (m/explain ImpliesGroups {:a2 "b" :a3 "c"})))
      (is (= [["should provide key: :a2"
               "should provide key: :a3"]]
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
      (is (= [[:xor
               "should provide key: :a1"
               "should provide key: :a2"
               "should provide key: :a3"]]
             (me/humanize (m/explain XOrGroups {}))))
      (is (= [[:xor
               [:and "should not provide key: :a1" "should not provide key: :a2"]
               [:and "should not provide key: :a1" "should not provide key: :a3"]
               [:and "should not provide key: :a2" "should not provide key: :a3"]]]
             (me/humanize (m/explain XOrGroups {:a1 "a" :a2 "b" :a3 "c" :a4 "d"}))))
      (is (= [[:xor
               [:and "should not provide key: :a1" "should not provide key: :a2"]
               [:and "should not provide key: :a1" "should not provide key: :a3"]
               [:and "should not provide key: :a2" "should not provide key: :a3"]]]
            (me/humanize (m/explain XOrGroups {:a1 "a" :a2 "b" :a3 "c"}))))
      (is (= [[:xor
               "should not provide key: :a1"
               "should not provide key: :a2"]]
             (me/humanize (m/explain XOrGroups {:a1 "a" :a2 "b"}))))
      (is (= [[:xor
               "should not provide key: :a1"
               "should not provide key: :a3"]]
             (me/humanize (m/explain XOrGroups {:a1 "a" :a3 "c"}))))
      (is (= [[:xor
               "should not provide key: :a2"
               "should not provide key: :a3"]]
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
      (is (= [[:or
               "should provide key: :a2"
               "should not provide key: :a3"]]
             (me/humanize (m/explain NotGroups {:a1 "a" :a3 "c"}))))
      (is (= [[:or "should provide key: :a1" "should not provide key: :a3"]]
             (me/humanize (m/explain NotGroups {:a2 "b" :a3 "c"}))))
      (is (nil? (m/explain NotGroups {:a1 "a"})))
      (is (nil? (m/explain NotGroups {:a2 "b"})))
      (is (= [[:or
               [:and
                "should provide key: :a1"
                "should provide key: :a2"]
               "should not provide key: :a3"]]
             (me/humanize (m/explain NotGroups {:a3 "c"})))))))

(def Address
  [:map
   {:iff [:street :city :zip]}
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
  [:map {:and [[:disjoint [:down] [:up]]
               [:disjoint [:left] [:right]]]}
   [:down {:optional true} [:= 1]]
   [:left {:optional true} [:= 1]]
   [:right {:optional true} [:= 1]]
   [:up {:optional true} [:= 1]]])

(def DPadNot
  [:map {:and [[:not [:and :down :up]]
               [:not [:and :left :right]]]}
   [:down {:optional true} [:= 1]]
   [:left {:optional true} [:= 1]]
   [:right {:optional true} [:= 1]]
   [:up {:optional true} [:= 1]]])

(def DPadDeMorgan
  [:map {:and [[:or [:not :down] [:not :up]]
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
             [:map {:contains :x}]
             {}))
         ["should provide key: :x"]))
  (is (not (m/validate [:map {:contains :a}] {})))
  (is (not (m/validate [:map {:contains "a"}] {})))
  (is (not (m/validate [:map {:contains []}] {})))
  (is (m/validate [:map {:contains []}] {[] nil}))
  (is (not (m/validate [:map {:contains nil}] {})))
  (is (m/validate [:map {:contains nil}] {nil nil}))
  (is (= (me/humanize
           (m/explain
             [:map {:contains nil}]
             {}))
         ["should provide key: nil"]))
  (is (= (me/humanize
           (m/explain
             [:map {:and [[:contains nil]
                          [:contains []]]}]
             {}))
         [[:and
           "should provide key: nil"
           "should provide key: []"]]))
  (is (= (me/humanize
           (m/explain
             [:map
              {:contains :x}
              [:x {:optional true} :int]]
             {}))
         ["should provide key: :x"]))
  (is (= (me/humanize
           (m/explain
             [:map
              {:or [:a1 :a2]}
              [:a1 {:optional true} :string]
              [:a2 {:optional true} :string]]
             {}))

         [[:or
           "should provide key: :a1"
           "should provide key: :a2"]]))
  (is (= (m/validate Address {})
         true))
  (is (= (me/humanize (m/explain Address {:zip 5555}))
         [[:xor
           [:and
            "should provide key: :street"
            "should provide key: :city"]
           "should not provide key: :zip"]]))
  (testing "GitOrMvn"
    (is (= (m/validate GitOrMvn {:mvn/version "1.0.0"})
           true))
    (is (= (me/humanize
             (m/explain GitOrMvn
                        {:mvn/version "1.0.0"
                         :git/sha "123"}))

           [[:xor
             "should not provide key: :mvn/version"
             "should not provide key: :git/sha"]]))
    (is (= (me/humanize
             (m/explain GitOrMvn {}))

           [[:xor
             "should provide key: :mvn/version"
             "should provide key: :git/sha"]])))
  (testing "TagImpliesSha"
    (is (= (m/validate TagImpliesSha {:git/sha "abc123"})
           true))
    (is (= (m/validate TagImpliesSha {:git/tag "v1.0.0" :git/sha "abc123"})
           true))
    (is (= (me/humanize
             (m/explain TagImpliesSha {:git/tag "v1.0.0"}))
           [["should provide key: :git/sha"]])))
  (testing "UserPass"
    (is (= (m/validate UserPass {})
           true))
    (is (= (m/validate UserPass {:user "a" :pass "b"})
           true))
    (is (= (me/humanize
             (m/explain UserPass {:user "a"}))
           [[:xor
             "should provide key: :pass"
             "should not provide key: :user"]])))
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
           ;;FIXME should say: either remove user and add secret, or add pass, but not both
           [[:or
             "should provide key: :secret"
             "should provide key: :pass"]]))
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
             [[:or
               "should not provide key: :down"
               "should not provide key: :up"]]))
      (is (= (me/humanize
               (m/explain DPad {:left 1 :right 1}))
             [[:or
               "should not provide key: :left"
               "should not provide key: :right"]])))
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
           [[:or
             "should provide key: :top"
             "should provide key: :bottom"
             "should provide key: :left"
             "should provide key: :right"]]))
    (is (= '({:top 0} {:bottom 0.5} {:left -1} {:left 0} {:bottom 1.5})
           (mg/sample Padding {:size 5 :seed 0})))))

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
  (mg/sample [:set {:contains :a
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
  (mg/sample [:set {:contains :a
                    :min 2}
              [:enum :a :b]]
             #{:a :b}))

(def DatomicValueType
  [:map {:dispatch [:db/valueType
                    [:db.type/tuple [:xor :db/tupleType :db/tupleAttrs :db/tupleTypes]]
                    [:db.type/bigint :any]
                    [:db.type/boolean :any]
                    [:db.type/double :any]
                    [:db.type/float :any]
                    ;; etc https://docs.datomic.com/schema/schema-reference.html#db-valuetype
                    [::m/default :any]]}
   [:db/valueType :keyword]
   [:db/tupleType {:optional true} keyword?]
   [:db/tupleTypes {:optional true} [:sequential keyword?]]
   [:db/tupleAttrs {:optional true} [:sequential keyword?]]])

(deftest datomic-tuple-test
  (is (m/validate DatomicValueType
                  {:db/valueType :db.type/tuple
                   :db/tupleTypes [:db.type/long :db.type/long]}))
  (is (nil? (m/explain
              DatomicValueType
              {:db/valueType :db.type/tuple
               :db/tupleTypes [:db.type/long :db.type/long]})))
  (is (m/validate DatomicValueType
                  {:db/valueType :db.type/tuple
                   :db/tupleAttrs [:semester/year :semester/season]}))
  (is (nil? (m/explain
              DatomicValueType
              {:db/valueType :db.type/tuple
               :db/tupleAttrs [:semester/year :semester/season]})))
  (is (m/validate DatomicValueType
                  {:db/valueType :db.type/tuple
                   :db/tupleType :db.type/keyword}))
  (is (nil? (m/explain
              DatomicValueType
              {:db/valueType :db.type/tuple
               :db/tupleType :db.type/keyword})))
  (is (not (m/validate DatomicValueType {:db/valueType :db.type/tuple})))
  (is (m/explain DatomicValueType {:db/valueType :db.type/tuple}))
  (is (= [[:xor
           "should provide key: :db/tupleType"
           "should provide key: :db/tupleAttrs"
           "should provide key: :db/tupleTypes"]]
         (me/humanize (m/explain DatomicValueType {:db/valueType :db.type/tuple}))))
  (is (not (m/validate DatomicValueType
                       {:db/valueType :db.type/tuple
                        :db/tupleType :db.type/keyword
                        :db/tupleAttrs [:semester/year :semester/season]})))
  (is (= [[:xor
           "should not provide key: :db/tupleType"
           "should not provide key: :db/tupleAttrs"]]
         (me/humanize
           (m/explain DatomicValueType
                      {:db/valueType :db.type/tuple
                       :db/tupleType :db.type/keyword
                       :db/tupleAttrs [:semester/year :semester/season]})))))
