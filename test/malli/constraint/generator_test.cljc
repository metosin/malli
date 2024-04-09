(ns malli.constraint.generator-test
  (:require [clojure.test :refer [are deftest is testing]]
            [clojure.test.check :refer [quick-check]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [com.gfredericks.test.chuck.properties :as prop' :refer [for-all]]
            [malli.core :as m]
            [malli.generator :as mg]
            [malli.json-schema-test :as json-schema-test]
            [malli.util :as mu]
            #?(:clj  [malli.test-macros :refer [when-env]]
               :cljs ["@js-joda/timezone/dist/js-joda-timezone-10-year-range"]))
  #?(:cljs (:require-macros [malli.test-macros :refer [when-env]])))

(defn shrink
  ([?schema] (shrink ?schema nil))
  ([?schema {:keys [seed]}]
  (-> (quick-check 1 (for-all [s (mg/generator ?schema)] false) {:seed (or seed 0)})
      :shrunk
      :smallest
      first
      (get 's))))

;; these generators don't work well with :and but do with constraints
;; because the generators are much more specific. they also grow
;; and shrink better because such-that fails a lot less often
;; and the size remains more consistent.
(deftest int-constraint-generator-test
  (is (thrown?
        #?(:clj Exception, :cljs js/Error)
        (dotimes [_ 10] (doall (mg/sample [:and int? [:> 739] [:< 741]])))))
  (is (= 740 (mg/generate [:int {:> 739 :< 741}])))
  (is (= 740 (mg/generate [:int {:and [[:not [:<= 739]]
                                       [:not [:>= 741]]]}])))
  (is (= 740 (shrink [:int {:> 739 :< 741}])))
  (dotimes [_ 100]
    (is (every? #{740}
                (mg/sample [:int {:> 739 :< 741}]
                           {:size 1000}))))
  (is (thrown?
        #?(:clj Exception, :cljs js/Error)
        (dotimes [_ 10]
          (doall (mg/sample [:and int? [:> 10] [:< 100]]
                            {:size 1000})))))
  (is (doall (mg/sample [:int {:> 10 :< 100}]
                        {:seed 123})))
  (is (= (mg/sample [:int {:> 10 :< 100}]
                    {:size 1000
                     :seed 0})
         (mg/sample [:int {:gen/> 10 :gen/< 100}]
                    {:size 1000
                     :seed 0})))
  (is (= 11 (shrink [:int {:> 10 :< 100}])))
  (is (thrown-with-msg?
        #?(:clj Exception, :cljs js/Error)
        #":malli\.generator/int-bounds-must-be-ints"
        (shrink [:int {:> ##Inf}])))
  #?(:clj (is (thrown-with-msg?
                Exception
                #":malli\.generator/int-generator-min-value-failure"
                (shrink [:int {:> Long/MAX_VALUE}]))))
  (is (thrown-with-msg?
        #?(:clj Exception, :cljs js/Error)
        #":malli\.generator/int-bounds-must-be-ints"
        (shrink [:int {:min ##Inf :max ##Inf}])))
  (is (thrown-with-msg?
        #?(:clj Exception, :cljs js/Error)
        #":malli\.generator/int-bounds-must-be-ints"
        (shrink [:int {:< ##-Inf}])))
  (is (thrown-with-msg?
        #?(:clj Exception, :cljs js/Error)
        #":malli\.generator/int-generator-max-value-failure"
        (shrink [:int {:< Long/MIN_VALUE}])))
  )

(deftest double-constraint-generator-test
  (is (thrown?
        #?(:clj Exception, :cljs js/Error)
        (dotimes [_ 10] (doall (mg/sample [:and :double [:> 739] [:< 741]])))))
  (dotimes [_ 10]
    (let [vs (mg/sample [:double {:> 739.000001 :< 739.000002}]
                        {:size 1000})]
      (is (< 500 (count (distinct vs))))
      (is (every? #(< 739.000001 % 739.000002)
                  vs))))
  (dotimes [_ 10]
    (let [vs (mg/sample [:double {:and [[:not [:<= 739.000001]]
                                        [:not [:>= 739.000002]]]}]
                        {:size 1000})]
      (is (< 500 (count (distinct vs))))
      (is (every? #(< 739.000001 % 739.000002)
                  vs))))
  (is (= 739.0000010000001
         (shrink [:double {:> 739.000001 :< 739.000002}])
         (shrink [:double {:and [[:not [:<= 739.000001]]
                                 [:not [:>= 739.000002]]]}])))
  (is (= (mg/sample [:double {:> 10 :< 100}]
                    {:size 1000
                     :seed 0})
         (mg/sample [:double {:gen/> 10 :gen/< 100}]
                    {:size 1000
                     :seed 0})))
  (is (= 10.0 (shrink [:double {:>= 10}])))
  (is (thrown-with-msg?
        #?(:clj Exception, :cljs js/Error)
        #":malli\.generator/double-generator-min-value-failure"
        (shrink [:double {:> ##Inf}])))
  (is (thrown-with-msg?
        #?(:clj Exception, :cljs js/Error)
        #":malli\.generator/double-generator-max-value-failure"
        (shrink [:double {:< ##-Inf}])))
  #?(:clj (is (= 10.000000000000002 (shrink [:double {:> 10 :< 100}])))
     :cljs (is (= 10.001 (shrink [:double {:> 10 :< 100}]))))
  (is (= 10.00000000001 (shrink [:double {:>= 10.00000000001 :< 100}])))
  (is (= 9.999999999999999 (shrink [:double {:>= 9.999999999999999 :< 100}]))))

(deftest string-constraint-generate-test
  (testing ":alphanumeric + :alpha :numeric"
    (is (= ["qBoTBneKUb" "RznfuEdSsmp" "pwbdaMNTYxxH" "MtCxHNxEiZPJ" "pRXpSisHqog"
            "QhEHYYfiswSl" "XKYeOExmpprzg" "xMwVWpPDVlAp" "iMrMJGWzETClJ" "syJVWCJeAOPmABe"]
           (vec (mg/sample [:string {:min 10 :alpha true}]
                           {:seed 0}))
           (vec (mg/sample [:string {:min 10 :alpha true :alphanumeric true}]
                           {:seed 0}))))
    (is (= ["0512069087" "81635196348" "416649118043" "105571456853" "94201363561" "815573842818"
            "2789823848768" "650900047134" "4183700601041" "300407523083076"]
           (vec (mg/sample [:string {:min 10 :numeric true}]
                           {:seed 0}))
           (vec (mg/sample [:string {:min 10 :numeric true :alphanumeric true}]
                           {:seed 0}))))
    (is (= ["5" "42" "" "" "" "W" "3" "0" "" "Gr"]
           (vec (mg/sample [:string {:not [:min 3]}]
                           {:seed 1}))))
    (is (every? seq (mg/sample [:string {:not [:max 0]}])))
    ; [:not [:and [:min 1] [:max 1]]]
    ; [:or [:not [:min 1]] [:not [:max 1]]]
    ; [:or [:max 0] [:min 2]]
    (is (= ["" "VuV" "fk" "j62" "" "" "" "" "" "lUm6Wj9gzzj" "Nk257" "Mo"
            "" "" "" "" "A1X93e8d" "UMcSfA0pN6N" "pa3Oh3st" "75zyo"]
           (mg/sample [:string {:not [:and [:min 1] [:max 1]]}]
                      {:size 20
                       :seed 0})
           (mg/sample [:string {:or [[:max 0] [:min 2]]}]
                      {:size 20
                       :seed 0})))
    (is (= ["" "Vu3H" "fR9" "j6pk" "p" "" "" "" "" "lUm6Wj9gzzRy" "Nk255d"
            "M0O" "B" "" "" "m" "A1X93e89Z" "UMcSfA0pN6bu" "pa3Oh3sLG" "75zy7p"]
          (mg/sample [:string {:not [:and [:min 2] [:max 2]]}]
                     {:size 20
                      :seed 0})
          (mg/sample [:string {:or [[:max 1] [:min 3]]}]
                     {:size 20
                      :seed 0})))
    (is (thrown-with-msg?
          #?(:clj Exception, :cljs js/Error)
          #":malli\.generator/unsatisfiable-string-constraint"
          (mg/generate [:string {:not [:min 0]}])))
    (is (= ["Q0o7BnE37EF" "6zNfuEdSsmmZ" "pwBdA45T9xxXu" "4t1X2NXEI96C6" "p6Xp7IS2qOc2"
            "6h1299fiSw70z" "8K9e51XMppRz7c" "X4W88PP18l02g" "I4r432WZE70lE0" "sy3V813e055M00ei"
            "NRQ2Gl195ax1V9UovVgd" "2So0R1gyU3011RcyD" "BtQmb9i90iW" "98ObX5CI5R2FIWse5"
            "rLoTqt6g235W410S" "mW2SSgREaTk9VN" "IPXCS20FN87X3gB4RsRkM" "BqD6BGChjQYtN2OnRE8x564Crst"
            "MJWJ1e325aR36vhwE0m71eDY0" "lLwqov5F4mFaqUL95TFgzMY7Wl"]
           (mg/sample [:string {:not [:and [:min 0] [:max 10]]}]
                      {:size 20 :seed 0})
           (mg/sample [:string {:min 11}]
                      {:size 20 :seed 0})))
    (is (= "5833307285"
           (mg/generate [:string {:min 10 :not :alpha}]
                        {:seed 0})))
    (is (thrown-with-msg?
          #?(:clj Exception, :cljs js/Error)
          #":malli\.generator/unsatisfiable-string-constraint"
          (mg/generate [:string {:min 10 :numeric true :alpha true}]))))
  (testing ":non-alpha"
    (is (= "5833307285"
           (mg/generate [:string {:min 10 :non-alpha true}]
                        {:seed 0}))))
  (testing ":includes"
    (is (= "54T0oJ7NCbWYeLkvm84iwi1POI68Cblah"
           (mg/generate [:string {:min 10 :includes "blah"}]
                        {:seed 0})))
    (is (= "54T0oJ7NCbWYeLkvm84iwi1POISfoobar"
           (mg/generate [:string {:min 10 :and [[:includes "foo"]
                                                [:includes "bar"]]}]
                        {:seed 0})))
    (is (= "000Afoobar"
           (shrink [:string {:min 10
                             :and [[:includes "foo"]
                                   [:includes "bar"]]}])))
    (is (thrown-with-msg?
          #?(:clj Exception, :cljs js/Error)
          #":malli\.generator/cannot-fit-includes-string"
          (mg/generate [:string {:max 5 :includes "longerthan5chars"}])))))
