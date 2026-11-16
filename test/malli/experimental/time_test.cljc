(ns ^:simple malli.experimental.time-test
  (:require [malli.core :as m]
            [malli.error :as me]
            [malli.registry :as mr]
            #?(:clj  [malli.experimental.time :as time]
               :cljs [malli.experimental.time :as time
                      :refer [Duration Period LocalDate LocalDateTime LocalTime Instant ZonedDateTime OffsetDateTime ZoneId OffsetTime]])
            [clojure.test :as t])
  #?(:clj (:import (java.time Duration Period LocalDate LocalDateTime LocalTime Instant ZonedDateTime OffsetDateTime ZoneId OffsetTime))))

(t/deftest compare-dates
  (t/is (time/<= (. LocalDate parse "2020-01-01")
                 (. LocalDate parse "2020-01-01")))
  (t/is (time/<= (. LocalDate parse "2020-01-01")
                 (. LocalDate parse "2020-01-02")))
  (t/is (time/<= (. Duration ofMillis 10)
                 (. Duration ofMillis 11)))
  (t/is (not (time/<= (. LocalDate parse "2020-01-02")
                      (. LocalDate parse "2020-01-01")))))

(def r
  (mr/composite-registry
   m/default-registry
   (mr/registry (time/schemas))))

(t/deftest basic-types
  (t/testing "Duration"
    (t/is (m/validate :time/duration (. Duration ofMillis 10) {:registry r}))
    (t/is (not (m/validate :time/duration 10 {:registry r}))))
  (t/testing "Period"
    (t/is (m/validate :time/period (. Period of 10 1 2) {:registry r}))
    (t/is (not (m/validate :time/period 10 {:registry r}))))
  (t/testing "zone id"
    (t/is (m/validate :time/zone-id (. ZoneId of "UTC") {:registry r}))
    (t/is (not (m/validate :time/zone-id "UTC" {:registry r}))))
  (t/testing "local date"
    (t/is (m/validate :time/local-date (. LocalDate parse "2020-01-01") {:registry r}))
    (t/is (not (m/validate :time/local-date "2020-01-01" {:registry r}))))
  (t/testing "local time"
    (t/is (m/validate :time/local-time (. LocalTime parse "12:00:00") {:registry r}))
    (t/is (not (m/validate :time/local-time "12:00:00" {:registry r}))))
  (t/testing "offset time"
    (t/is (m/validate :time/offset-time (. OffsetTime parse "12:00:00+00:00") {:registry r}))
    (t/is (not (m/validate :time/offset-time "12:00:00" {:registry r}))))
  (t/testing "local date time"
    (t/is (m/validate :time/local-date-time (. LocalDateTime parse "2020-01-01T12:00:00") {:registry r}))
    (t/is (not (m/validate :time/local-date-time "2020-01-01T12:00:00" {:registry r}))))
  (t/testing "instant"
    (t/is (m/validate :time/instant (. Instant parse "2022-12-18T12:00:25.840823567Z") {:registry r}))
    (t/is (not (m/validate :time/instant "2022-12-18T12:00:25.840823567Z" {:registry r}))))
  (t/testing "zoned date time"
    (t/is (m/validate :time/zoned-date-time (. ZonedDateTime parse "2022-12-18T12:00:25.840823567Z[UTC]") {:registry r}))
    (t/is (m/validate :time/zoned-date-time (. ZonedDateTime parse "2022-12-18T06:00:25.840823567-06:00[America/Chicago]") {:registry r}))
    (t/is (not (m/validate :time/zoned-date-time "2022-12-18T12:00:25.840823567Z[UTC]" {:registry r}))))
  (t/testing "offset date time"
    (t/is (m/validate :time/offset-date-time (. OffsetDateTime parse "2022-12-18T12:00:25.840823567Z") {:registry r}))
    (t/is (m/validate :time/offset-date-time (. OffsetDateTime parse "2022-12-18T06:00:25.840823567-06:00") {:registry r}))
    (t/is (not (m/validate :time/offset-date-time "2022-12-18T12:00:25.840823567Z" {:registry r})))))

(t/deftest min-max
  (t/testing "Duration"
    (t/is (-> [:time/duration {:min (. Duration ofMillis 9) :max (. Duration ofMillis 10)}]
              (m/validate (. Duration ofMillis 10) {:registry r})))
    (t/is (-> [:time/duration {:min (. Duration ofMillis 9) :max (. Duration ofMillis 10)}]
              (m/validate (. Duration ofMillis 12) {:registry r})
              not)))
  (t/testing "Period"
    (t/is (-> [:time/period {:min (. Period ofYears 9) :max (. Period ofYears 10)}]
            (m/validate (. Period ofYears 10) {:registry r})))
    (t/is (-> [:time/period {:min (. Period ofMonths 9) :max (. Period ofMonths 10)}]
            (m/validate (. Period ofMonths 12) {:registry r})
            not))
    (t/is (-> [:time/period {:min (. Period ofMonths 9) :max (. Period ofMonths 10)}]
            (m/validate (. Period ofDays 12) {:registry r})
            not))
    (t/is (-> [:time/period {:min (. Period ofYears 9)}]
            (m/validate (. Period ofYears 9) {:registry r})))
    (t/is (-> [:time/period {:min (. Period ofYears 9)}]
            (m/validate (. Period ofYears 10) {:registry r})))
    (t/is (-> [:time/period {:min (. Period ofYears 9)}]
            (m/validate (. Period ofYears 8) {:registry r})
            not))
    (t/is (-> [:time/period {:min (. Period of 0 10 2)}]
            (m/validate (. Period of 1 9 3) {:registry r})))
    (t/is (-> [:time/period {:max (. Period ofYears 9)}]
            (m/validate (. Period ofYears 9) {:registry r})))
    (t/is (-> [:time/period {:max (. Period ofYears 9)}]
            (m/validate (. Period ofYears 8) {:registry r})))
    (t/is (-> [:time/period {:max (. Period ofYears 9)}]
            (m/validate (. Period ofDays 8) {:registry r})))
    (t/is (-> [:time/period {:max (. Period ofYears 1)}]
            (m/validate (. Period ofMonths 23) {:registry r})))
    (t/is (-> [:time/period {:max (. Period ofYears 9)}]
            (m/validate (. Period ofYears 10) {:registry r})
            not))
    (t/is (-> [:time/period {:max (. Period of 0 10 2)}]
            (m/validate (. Period of 1 9 3) {:registry r})
            not)))
  (t/testing "local date"
    (t/is (-> [:time/local-date {:min (. LocalDate parse "2020-01-01") :max (. LocalDate parse "2020-01-03")}]
              (m/validate (. LocalDate parse "2020-01-01") {:registry r})))
    (t/is (-> [:time/local-date {:min (. LocalDate parse "2020-01-01") :max (. LocalDate parse "2020-01-03")}]
              (m/validate (. LocalDate parse "2020-01-04") {:registry r})
              not)))
  (t/testing "local time"
    (t/is (-> [:time/local-time {:min (. LocalTime parse "12:00:00") :max (. LocalTime parse "13:00:00")}]
              (m/validate (. LocalTime parse "12:00:00") {:registry r})))
    (t/is (-> [:time/local-time {:min (. LocalTime parse "12:00:00") :max (. LocalTime parse "13:00:00")}]
              (m/validate (. LocalTime parse "14:00:00") {:registry r})
              not)))
  (t/testing "local date time"
    (t/is (m/validate [:time/local-date-time
                       {:min (. LocalDateTime parse "2020-01-01T11:30:00")
                        :max (. LocalDateTime parse "2020-01-01T12:30:00")}]
                      (. LocalDateTime parse "2020-01-01T12:00:00") {:registry r}))
    (t/is (-> [:time/local-date-time
               {:min (. LocalDateTime parse "2020-01-01T11:30:00")
                :max (. LocalDateTime parse "2020-01-01T12:30:00")}]
              (m/validate (. LocalDateTime parse "2020-01-01T12:40:00") {:registry r})
              not)))
  (t/testing "instant"
    (t/is (-> [:time/instant
               {:min (. Instant parse "2022-12-18T11:30:25.840823567Z")
                :max (. Instant parse "2022-12-18T12:30:25.840823567Z")}]
              (m/validate (. Instant parse "2022-12-18T12:00:25.840823567Z") {:registry r})))
    (t/is (-> [:time/instant
               {:min (. Instant parse "2022-12-18T11:30:25.840823567Z")
                :max (. Instant parse "2022-12-18T12:30:25.840823567Z")}]
              (m/validate (. Instant parse "2022-12-18T12:40:25.840823567Z") {:registry r})
              not)))
  (t/testing "zoned date time"
    (t/is (-> [:time/zoned-date-time
               {:min (. ZonedDateTime parse "2022-12-18T11:30:25.840823567Z[UTC]")
                :max (. ZonedDateTime parse "2022-12-18T12:10:25.840823567Z[UTC]")}]
              (m/validate (. ZonedDateTime parse "2022-12-18T12:00:25.840823567Z[UTC]") {:registry r})))
    (t/is (-> [:time/zoned-date-time
               {:min (. ZonedDateTime parse "2022-12-18T05:40:25.840823567-06:00[America/Chicago]")
                :max (. ZonedDateTime parse "2022-12-18T12:10:25.840823567Z[UTC]")}]
              (m/validate (. ZonedDateTime parse
                             "2022-12-18T06:00:25.840823567-06:00[America/Chicago]") {:registry r})))
    (t/is (not (m/validate :time/zoned-date-time "2022-12-18T12:00:25.840823567Z[UTC]" {:registry r}))))
  (t/testing "offset date time"
    (t/is (m/validate :time/offset-date-time (. OffsetDateTime parse "2022-12-18T12:00:25.840823567Z") {:registry r}))
    (t/is (m/validate :time/offset-date-time (. OffsetDateTime parse "2022-12-18T06:00:25.840823567-06:00") {:registry r}))
    (t/is (not (m/validate :time/offset-date-time "2022-12-18T12:00:25.840823567Z" {:registry r})))))

(t/deftest humanize-test
  (let [humanize (fn [?schema value] (-> (m/explain ?schema value {:registry r}) (me/humanize)))]
    (t/testing "invalid type"
      (doseq [[type message] {:time/duration "should be a duration"
                              :time/period "should be a period"
                              :time/instant "should be an instant"
                              :time/local-date "should be a local date"
                              :time/local-time "should be a local time"
                              :time/local-date-time "should be a local date time"
                              :time/offset-date-time "should be an offset date time"
                              :time/offset-time "should be an offset time"
                              :time/zoned-date-time "should be a zoned date time"
                              :time/zone-id "should be a zone id"
                              :time/zone-offset "should be a zone offset"}]
        (t/is (= [message] (humanize type "2020-01-01")) (str type))))
    (t/testing "min and max"
      (let [schema [:time/local-date {:min (. LocalDate parse "2020-01-01") :max (. LocalDate parse "2020-01-03")}]]
        (t/is (nil? (humanize schema (. LocalDate parse "2020-01-02"))))
        (t/is (= ["should be at least 2020-01-01"] (humanize schema (. LocalDate parse "2019-12-31"))))
        (t/is (= ["should be at most 2020-01-03"] (humanize schema (. LocalDate parse "2020-01-04"))))
        (t/is (= ["should be a local date"] (humanize schema "2020-01-02"))))
      (t/is (= ["should be 2020-01-01"]
               (humanize [:time/local-date {:min (. LocalDate parse "2020-01-01") :max (. LocalDate parse "2020-01-01")}]
                         (. LocalDate parse "2020-01-02"))))
      (t/is (= ["should be at least P1Y"]
               (humanize [:time/period {:min (. Period ofYears 1)}] (. Period ofMonths 11))))
      (t/is (= ["should be at most PT0.01S"]
               (humanize [:time/duration {:max (. Duration ofMillis 10)}] (. Duration ofMillis 11)))))
    (t/testing "negation"
      ;; schema instances, as negation of registry references is not humanized yet
      (t/is (= ["should not be a local date"]
               (humanize [:not (time/-local-date-schema)] (. LocalDate parse "2020-01-02"))))
      (t/is (= ["should not be at least 2020-01-01"]
               (humanize [:not [(time/-local-date-schema) {:min (. LocalDate parse "2020-01-01")}]] (. LocalDate parse "2020-01-02")))))
    (t/testing "schema properties override the default message"
      (t/is (= ["custom"] (humanize [:time/local-date {:error/message "custom"}] "2020-01-01"))))))
