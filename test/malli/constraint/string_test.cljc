(ns malli.constraint.string-test
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

(deftest string-constraint-test
  (testing ":min/:max"
    (is (m/validate [:string {:min 1 :max 5}] "ab"))
    (is (m/validate [:string {:min 4 :max 4}] "🌉🜉"))
    (is (m/validate [:string {:min-code-points 2 :max-code-points 2}] "🌉🜉"))
    (is (m/validate [:string {:max-code-points 2}] "🌉🜉"))
    (is (not (m/validate [:string {:min-code-points 2}] "🌉")))
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
           (me/humanize (m/explain [:string {:max 1}] "🌉"))
           (me/humanize (m/explain [:string {:max 1}] "12"))
           (me/humanize (m/explain [:string {:min 0 :max 1}] "12"))
           (me/humanize (m/explain [:string {:and [[:max 1]]}] "12"))))
    (is (= ["should be at most 1 code point, given 2"]
           (me/humanize (m/explain [:string {:max-code-points 1}] "🌉🌉"))))
    (is (= ["should be at least 3 code points, given 2"]
           (me/humanize (m/explain [:string {:min-code-points 3}] "🌉🌉"))))
    (is (= ["should be less than 1 code point, given 2"]
           (me/humanize (m/explain [:string {:not [:min-code-points 1]}] "🌉🌉"))))
    (is (= ["should be less than 1 code point, given 1"]
           (me/humanize (m/explain [:string {:not [:min-code-points 1]}] "🌉"))))
    (is (= ["should be more than 2 code points, given 1"]
           (me/humanize (m/explain [:string {:not [:max-code-points 2]}] "🌉"))))
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
    (is (m/validate [:string {:not [:alpha]}] "𓅡"))
    (is (= [["should be alphabetic: index 0 has \\space."]]
           (me/humanize (m/explain [:string {:alpha true}] " ab"))
           (me/humanize (m/explain [:string {:alpha true}] " "))))
    (is (= [["should be alphabetic: index 0 has code point 78177."
             "should be alphabetic: index 4 (code point offset 3) has code point 78177."]]
           (me/humanize (m/explain [:string {:alpha true}] "𓅡ab𓅡"))))
    (is (= ["should contain a non-alphabetic character"]
           (me/humanize (m/explain [:string {:not [:alpha]}] "ab"))
           (me/humanize (m/explain [:string {:not [:alpha]}] "")))))
  (testing ":non-alpha"
    (is (m/validate [:string {:non-alpha true}] "12"))
    (is (m/validate [:string {:non-alpha true}] ""))
    (is (not (m/validate [:string {:non-alpha true}] "a2")))
    (is (not (m/validate [:string {:not [:non-alpha]}] "12")))
    (is (= [["should not contain alphabetic characters: index 0 has \\a."
             "should not contain alphabetic characters: index 1 has \\b."
             "should not contain alphabetic characters: index 3 has \\c."]]
           (me/humanize (m/explain [:string {:non-alpha true}] "ab1c*"))))
    (is (= ["should contain an alphabetic character"]
           (me/humanize (m/explain [:string {:not [:non-alpha]}] "12")))))
  (testing ":numeric"
    (is (m/validate [:string {:numeric true}] ""))
    (is (m/validate [:string {:numeric true}] "12"))
    (is (not (m/validate [:string {:numeric true}] "1a2")))
    (is (= [["should be numeric: index 0 has \\a."
             "should be numeric: index 1 has \\b."
             "should be numeric: index 3 has \\c." "should be numeric: index 4 has \\*."]]
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
    (is (= [["should not contain numeric characters: index 0 has \\1."
             "should not contain numeric characters: index 2 has \\2."]]
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
    (is (= [["should be alphanumeric: index 0 has \\[."
             "should be alphanumeric: index 3 has \\]."]]
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
    (is (= [["should not contain alphanumeric characters: index 1 has \\a."
             "should not contain alphanumeric characters: index 2 has \\1."]]
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
    (is (not (m/validate [:string {:distinct true}] "🌉🜉"))) ;; same lower surrogate
    (is (m/validate [:string {:distinct-code-points true}] "🌉🜉")) ;; same lower surrogate
    (is (= ["invalid type"]
           (me/humanize (m/explain [:string {:distinct true}] nil))))
    (is (= ["should be distinct: \\[ provided 2 times"]
           (me/humanize (m/explain [:string {:distinct true}] "[[")))))
  (testing ":palindrome"
    (is (not (m/validate [:string {:palindrome true}] "[]")))
    (is (m/validate [:string {:palindrome true}] "[["))
    (is (m/validate [:string {:palindrome true}] ""))
    (is (m/validate [:string {:palindrome true}] "a"))
    (is (m/validate [:string {:palindrome true}] "abcba"))
    (is (m/validate [:string {:palindrome true}] "𓅡abcba𓅡"))
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
    (is (= [["should not have leading whitespace"]]
           (me/humanize (m/explain [:string {:trim true}] " a"))))
    (is (= [["should not have trailing whitespace"]]
           (me/humanize (m/explain [:string {:trim true}] "a "))))
    (is (= [["should not have leading whitespace"
             "should not have trailing whitespace"]]
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
          #":malli\.constraint\.string\.validate/escape-constraint-map-cannot-overlap-keys-vals"
          (m/validator [:string {:escapes {\c "c"}}])))
    (is (= [["should escape character \\c"]]
           (me/humanize (m/explain [:string {:escapes {\c "b"}}] "c"))))
    (is (= ["should include at least one unescaped character: \\c"]
           (me/humanize (m/explain [:string {:not [:escapes {\c "b"}]}] "b")))))
  (testing ":includes"
    (is (m/validate [:string {:includes "foo"}] "foobar"))
    (is (m/validate [:string {:includes "𓅡"}] "𓅡foobar"))
    (is (not (m/validate [:string {:includes "foo"}] "oofar")))
    (is (= ["should include substring \"foo\""]
           (me/humanize (m/explain [:string {:includes "foo"}] "oobar"))))
    (is (= ["should include substring \"𓅡\""]
           (me/humanize (m/explain [:string {:includes "𓅡"}] "oobar"))))
    (is (= ["should not include substring \"foo\""]
           (me/humanize (m/explain [:string {:not [:includes "foo"]}] "foobar")))))
  (testing ":edn"
    (is (not (m/validate [:string {:edn true}] "")))
    (is (m/validate [:string {:edn true}] "()"))
    (is (m/validate [:string {:edn :int}] "123"))
    (is (not (m/validate [:string {:edn :int}] "a")))
    (is (m/validate [:string {:edn :symbol}] "a"))
    (is (m/validate [:string {:edn :string}] "\"a\""))
    (is (m/validate [:string {:edn :string}] "\"𓅡\""))
    (is (not (m/validate [:string {:edn :keyword}] "\"a\"")))
    (is (m/validate [:string {:edn :keyword}] ":a"))
    (is (m/validate [:string {:or [:edn :alpha]}] ":a"))
    (is (m/validate [:string {:alpha true}] "a"))
    (is (not (m/validate [:string {:xor [:edn :alpha]}] "a")))
    ;;TODO not yet implemented
    (is (thrown-with-msg?
          #?(:clj Exception, :cljs js/Error)
          #":malli\.constraint\.string\.validate/edn-string-regex-schema-not-yet-implemented"
          (m/validate [:string {:edn [:+ :keyword]}] "\"a\"")))
    (is (= [["should be a string of :keyword" :constraint-failure ["invalid type"]]]
           (me/humanize (m/explain [:string {:edn :keyword}] "a"))))
    (is (= [["should be a string of [:map [:a :int]]"
             :constraint-failure {:a ["missing required key"]}]]
           (me/humanize (m/explain [:string {:edn [:map [:a :int]]}]
                                   "{}"))))
    (is (m/validate [:string {:xor [:edn :alpha]}] "a1"))
    (is (m/validate [:string {:xor [:edn :alpha]}] "1"))
    (is (= ["should contain a non-alphabetic character"]
           (me/humanize (m/explain [:string {:xor [:edn :alpha]}] "a"))))
    (is (= ["should not include substring \"foo\""]
           (me/humanize (m/explain [:string {:xor [:edn [:includes "foo"]]}] "foo1"))))
    (is (= ["should not include substring \"𓅡\""]
           (me/humanize (m/explain [:string {:xor [:edn [:includes "𓅡"]]}] "𓅡foo1"))))
    (is (m/validate [:string {:implies [:alphanumeric :edn]}] "1"))
    (is (m/validate [:string {:implies [:alphanumeric :edn]}] "a"))
    (is (m/validate [:string {:implies [:alphanumeric :edn]}] "a1"))
    (is (= [["should be valid edn"]]
           (me/humanize (m/explain [:string {:implies [:alphanumeric :edn]}] "1a"))))))

(def Password
  [:string {:min 5
            :and [[:not [:non-alpha]]
                  [:not [:non-numeric]]
                  ;; TODO [:string-class {:min 1} [\; \! \@ \# \$ \% \^ \& \*]]
                  (into [:or] (map #(do [:includes (str %)]))
                        [\; \! \@ \# \$ \% \^ \& \*])]}])

(deftest string-password-test
  (is (not (m/validate Password "12345abc")))
  (is (m/validate Password "12345abc^"))
  (is (= [[:and
           "should contain an alphabetic character"
           "should contain a numeric character"
           [:or
            "should include substring \";\""
            "should include substring \"!\""
            "should include substring \"@\""
            "should include substring \"#\""
            "should include substring \"$\""
            "should include substring \"%\""
            "should include substring \"^\""
            "should include substring \"&\""
            "should include substring \"*\""]
           "should be at least 5 characters, given 0"]]
         (me/humanize (m/explain Password ""))))
  #_;;TODO
  (is (mg/generate Password)))
