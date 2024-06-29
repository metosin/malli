(ns malli.adapter.spec1.generator
  (:require [clojure.spec.alpha :as s]
            [clojure.test.check.generators]
            [malli.core :as m]
            [malli.adapter.spec1 :as from]
            [malli.generator :as mg]))

(defmethod mg/-schema-generator ::from/spec [schema {::keys [overrides rmap] :as options}]
  (s/gen* (-> schema m/children peek s/spec) overrides [] rmap))

(defn -malli-gen* [m overrides path rmap]
  (mg/generator
    (m/-update-options m #(into (or % {}) {::overrides overrides ::rmap rmap}))))

(reset! malli.adapter.spec1/-malli-gen* -malli-gen*)
