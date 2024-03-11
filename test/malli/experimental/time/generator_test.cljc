(ns malli.experimental.time.generator-test
  (:require [malli.generator :as mg]
            [malli.core :as m]
            [malli.experimental.time-test :refer [r]]
            [malli.experimental.time.generator]
            [clojure.test :as t]
            #?(:cljs [malli.experimental.time :refer [LocalDate LocalTime]]))
  #?(:clj (:import (java.time LocalDate LocalTime))))

(defn exercise [schema]
  (let [schema (m/schema schema {:registry r})
        v (m/validator schema {:registry r})
        g (mg/generator schema {:registry r})]
    (every? v (mg/sample g {:size 1000 :registry r}))))

(t/deftest generator-test
  (t/testing "simple schemas"
    (t/is (exercise :time/duration))
    (t/is (exercise :time/period))
    (t/is (exercise :time/zone-id))
    (t/is (exercise :time/zone-offset))
    (t/is (exercise :time/instant))
    (t/is (exercise :time/zoned-date-time))
    (t/is (exercise :time/offset-date-time))
    (t/is (exercise :time/offset-time))
    (t/is (exercise :time/local-date))
    (t/is (exercise :time/local-time))
    (t/is (exercise :time/local-date-time)))
  (t/testing "min max"
    (t/is (exercise [:time/local-date {:min (. LocalDate parse "1980-01-02") :max (. LocalDate parse "1982-03-02")}]))
    (t/is (exercise [:time/local-time {:min (. LocalTime parse "12:00:00") :max (. LocalTime parse "18:00:00")}]))))
