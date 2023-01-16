(ns malli.experimental.js-joda-time-test
  (:require [malli.core :as m]
            [malli.registry :as mr]
            [malli.experimental.js-joda-time :as time]
            [clojure.test :as t]
            ["@js-joda/core" :as js-joda]
            [goog.object]))

(def Period (goog.object/get js-joda "Period"))
(def Instant (goog.object/get js-joda "Instant"))
(def Duration (goog.object/get js-joda "Duration"))
(def LocalDate (goog.object/get js-joda "LocalDate"))
(def LocalTime (goog.object/get js-joda "LocalTime"))
(def ZonedDateTime (goog.object/get js-joda "ZonedDateTime"))
(def LocalDateTime (goog.object/get js-joda "LocalDateTime"))
(def MonthDay (goog.object/get js-joda "MonthDay"))
(def Year (goog.object/get js-joda "Year"))
(def YearMonth (goog.object/get js-joda "YearMonth"))
(def ZoneId (goog.object/get js-joda "ZoneId"))
(def DayOfWeek (goog.object/get js-joda "DayOfWeek"))
(def Month (goog.object/get js-joda "Month"))
(def Clock (goog.object/get js-joda "Clock"))
(def ZoneOffset (goog.object/get js-joda "ZoneOffset"))
(def OffsetDateTime (goog.object/get js-joda "OffsetDateTime"))
(def OffsetTime (goog.object/get js-joda "OffsetTime"))


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
  #_(t/testing "zone id"
    (t/is (m/validate :time/zone-id (ZoneId/of "UTC") {:registry r}))
    (t/is (not (m/validate :time/zone-id "UTC" {:registry r}))))
  #_(t/testing "local date"
    (t/is (m/validate :time/local-date (LocalDate/parse "2020-01-01") {:registry r}))
    (t/is (not (m/validate :time/local-date "2020-01-01" {:registry r}))))
  #_(t/testing "local time"
    (t/is (m/validate :time/local-time (LocalTime/parse "12:00:00") {:registry r}))
    (t/is (not (m/validate :time/local-time "12:00:00" {:registry r}))))
  #_(t/testing "offset time"
    (t/is (m/validate :time/offset-time (OffsetTime/parse "12:00:00+00:00") {:registry r}))
    (t/is (not (m/validate :time/offset-time "12:00:00" {:registry r}))))
  #_(t/testing "local date time"
    (t/is (m/validate :time/local-date-time (LocalDateTime/parse "2020-01-01T12:00:00") {:registry r}))
    (t/is (not (m/validate :time/local-date-time "2020-01-01T12:00:00" {:registry r}))))
  #_(t/testing "instant"
    (t/is (m/validate :time/instant (Instant/parse "2022-12-18T12:00:25.840823567Z") {:registry r}))
    (t/is (not (m/validate :time/instant "2022-12-18T12:00:25.840823567Z" {:registry r}))))
  #_(t/testing "zoned date time"
    (t/is (m/validate :time/zoned-date-time (ZonedDateTime/parse "2022-12-18T12:00:25.840823567Z[UTC]") {:registry r}))
    (t/is (m/validate :time/zoned-date-time (ZonedDateTime/parse "2022-12-18T06:00:25.840823567-06:00[America/Chicago]") {:registry r}))
    (t/is (not (m/validate :time/zoned-date-time "2022-12-18T12:00:25.840823567Z[UTC]" {:registry r}))))
  #_(t/testing "offset date time"
    (t/is (m/validate :time/offset-date-time (OffsetDateTime/parse "2022-12-18T12:00:25.840823567Z") {:registry r}))
    (t/is (m/validate :time/offset-date-time (OffsetDateTime/parse "2022-12-18T06:00:25.840823567-06:00") {:registry r}))
    (t/is (not (m/validate :time/offset-date-time "2022-12-18T12:00:25.840823567Z" {:registry r})))))

(t/deftest min-max
  (t/testing "Duration"
    (t/is (-> [:time/duration {:min (. Duration ofMillis 9) :max (. Duration ofMillis 10)}]
              (m/validate (. Duration ofMillis 10) {:registry r})))
    (t/is (-> [:time/duration {:min (. Duration ofMillis 9) :max (. Duration ofMillis 10)}]
              (m/validate (. Duration ofMillis 12) {:registry r})
              not)))
  #_(t/testing "local date"
    (t/is (-> [:time/local-date {:min (LocalDate/parse "2020-01-01") :max (LocalDate/parse "2020-01-03")}]
              (m/validate (LocalDate/parse "2020-01-01") {:registry r})))
    (t/is (-> [:time/local-date {:min (LocalDate/parse "2020-01-01") :max (LocalDate/parse "2020-01-03")}]
              (m/validate (LocalDate/parse "2020-01-04") {:registry r})
              not)))
  #_(t/testing "local time"
    (t/is (-> [:time/local-time {:min (LocalTime/parse "12:00:00") :max (LocalTime/parse "13:00:00")}]
              (m/validate (LocalTime/parse "12:00:00") {:registry r})))
    (t/is (-> [:time/local-time {:min (LocalTime/parse "12:00:00") :max (LocalTime/parse "13:00:00")}]
              (m/validate (LocalTime/parse "14:00:00") {:registry r})
              not)))
  #_(t/testing "local date time"
    (t/is (m/validate [:time/local-date-time
                       {:min (LocalDateTime/parse "2020-01-01T11:30:00")
                        :max (LocalDateTime/parse "2020-01-01T12:30:00")}]
            (LocalDateTime/parse "2020-01-01T12:00:00") {:registry r}))
    (t/is (-> [:time/local-date-time
               {:min (LocalDateTime/parse "2020-01-01T11:30:00")
                :max (LocalDateTime/parse "2020-01-01T12:30:00")}]
              (m/validate (LocalDateTime/parse "2020-01-01T12:40:00") {:registry r})
              not)))
  #_(t/testing "instant"
    (t/is (-> [:time/instant
               {:min (Instant/parse "2022-12-18T11:30:25.840823567Z")
                :max (Instant/parse "2022-12-18T12:30:25.840823567Z")}]
              (m/validate (Instant/parse "2022-12-18T12:00:25.840823567Z") {:registry r})))
    (t/is (-> [:time/instant
               {:min (Instant/parse "2022-12-18T11:30:25.840823567Z")
                :max (Instant/parse "2022-12-18T12:30:25.840823567Z")}]
              (m/validate (Instant/parse "2022-12-18T12:40:25.840823567Z") {:registry r})
              not)))
  #_(t/testing "zoned date time"
    (t/is (-> [:time/zoned-date-time
               {:min (ZonedDateTime/parse "2022-12-18T11:30:25.840823567Z[UTC]")
                :max (ZonedDateTime/parse "2022-12-18T12:10:25.840823567Z[UTC]")}]
              (m/validate (ZonedDateTime/parse "2022-12-18T12:00:25.840823567Z[UTC]") {:registry r})))
    (t/is (-> [:time/zoned-date-time
               {:min (ZonedDateTime/parse "2022-12-18T05:40:25.840823567-06:00[America/Chicago]")
                :max (ZonedDateTime/parse "2022-12-18T12:10:25.840823567Z[UTC]")}]
              (m/validate (ZonedDateTime/parse
                            "2022-12-18T06:00:25.840823567-06:00[America/Chicago]") {:registry r})))
    #_(t/is (not (m/validate :time/zoned-date-time "2022-12-18T12:00:25.840823567Z[UTC]" {:registry r}))))
  #_(t/testing "offset date time"
      (t/is (m/validate :time/offset-date-time (OffsetDateTime/parse "2022-12-18T12:00:25.840823567Z") {:registry r}))
      (t/is (m/validate :time/offset-date-time (OffsetDateTime/parse "2022-12-18T06:00:25.840823567-06:00") {:registry r}))
      (t/is (not (m/validate :time/offset-date-time "2022-12-18T12:00:25.840823567Z" {:registry r})))))

