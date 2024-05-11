(ns malli.adapter.spec1-demo
  (:require [clojure.spec.alpha :as s]
            [malli.adapter.spec1 :as from]
            [malli.core :as m]
            [malli.registry :as mr]))

;; to migrate from spec1 to malli, gradually replace spec registry
;; entries with malli schemas via the `from/malli` macro.
;; to escape back to spec inside malli, use `from/spec`.

(def email-regex #"^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,63}$")

;;old
;(s/def :acct/email-type (s/and string? #(re-matches email-regex %)))
;;new
(s/def :acct/email-type (from/malli [:re email-regex]))

;;old
;(s/def :acct/acctid int?)
;;new
(s/def :acct/acctid (from/malli :int)) ;; new

(s/def :acct/first-name string?)
(s/def :acct/last-name string?)
(s/def :acct/email :acct/email-type)

;; example escaping back to spec from malli
(s/def :person/relatives (from/malli [:sequential
                                      (from/spec :acct/person)]))

(s/def :acct/person (s/keys :req [:acct/first-name :acct/last-name :acct/email]
                            :opt [:demo-escape-spec/recursive :acct/phone]))

(s/valid? :acct/person
  {:acct/first-name "Bugs"
   :acct/last-name "Bunny"
   :acct/email "bugs@example.com"})
;;=> true

;; Fails required key check
(s/explain :acct/person
  {:acct/first-name "Bugs"})
;; #:acct{:first-name "Bugs"} - failed: (contains? % :acct/last-name)
;;   spec: :acct/person
;; #:acct{:first-name "Bugs"} - failed: (contains? % :acct/email)
;;   spec: :acct/person

;; Fails attribute conformance
(s/explain :acct/person
  {:acct/first-name "Bugs"
   :acct/last-name "Bunny"
   :acct/email "n/a"})
;; before
;; VVVVVV
;; "n/a" - failed: (re-matches email-regex %) in: [:acct/email]
;;   at: [:acct/email] spec: :acct/email-type

;; after
;; VVVVVV
;; "n/a" - failed: (malli [:re #"^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,63}$"]) in: [:acct/email]
;; at: [:acct/email] spec: :acct/email-type

(s/valid? :acct/person
  {:acct/first-name "Bugs"
   :acct/last-name "Bunny"
   :acct/email "bugs@example.com"
   :person/relatives
   [{:acct/first-name "Bugs"
     :acct/last-name "Bunny"
     :acct/email "bugs@example.com"}]})

;; Fails required key check
(s/explain :acct/person
  {:acct/first-name "Bugs"
   :acct/last-name "Bunny"
   :acct/email "bugs@example.com"
   :person/relatives
   [{:acct/first-name "Bugs"}]})
;; {:acct/first-name "Bugs"} - failed: (malli [:malli.adapter.spec1/spec (contains? % :acct/last-name)]) in: [:person/relatives 0] at: [:person/relatives 0] spec: :person/relatives
;; {:acct/first-name "Bugs"} - failed: (malli [:malli.adapter.spec1/spec (contains? % :acct/email)]) in: [:person/relatives 0] at: [:person/relatives 0] spec: :person/relatives
