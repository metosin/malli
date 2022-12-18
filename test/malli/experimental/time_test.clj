(ns malli.experimental.time-test
  (:require
   [malli.core :as m]
   [malli.registry :as mr]
   [malli.experimental.time :as time]
   [clojure.test :as t])
  (:import
   (java.time Duration LocalDate LocalDateTime LocalTime Instant ZonedDateTime OffsetDateTime ZoneId)))

(t/deftest compare-dates
  (t/is
   (time/t<= (LocalDate/parse "2020-01-01")
             (LocalDate/parse "2020-01-01")))
  (t/is
   (time/t<= (LocalDate/parse "2020-01-01")
             (LocalDate/parse "2020-01-02")))
  (t/is
   (not
    (time/t<= (LocalDate/parse "2020-01-02")
              (LocalDate/parse "2020-01-01")))))

(def r
  (mr/composite-registry
   m/default-registry
   (mr/registry (time/-time-schemas))))

(t/deftest basic-types
  (t/testing "zone id"
    (t/is (m/validate :time/zone-id (ZoneId/of "UTC") {:registry r}))
    (t/is (not (m/validate :time/zone-id "UTC" {:registry r}))))
  (t/testing "local date"
    (t/is (m/validate :time/local-date (LocalDate/parse "2020-01-01") {:registry r}))
    (t/is (not (m/validate :time/local-date "2020-01-01" {:registry r}))))
  (t/testing "local time"
    (t/is (m/validate :time/local-time (LocalTime/parse "12:00:00") {:registry r}))
    (t/is (not (m/validate :time/local-time "12:00:00" {:registry r}))))
  (t/testing "local date time"
    (t/is (m/validate :time/local-date-time (LocalDateTime/parse "2020-01-01T12:00:00") {:registry r}))
    (t/is (not (m/validate :time/local-date-time "2020-01-01T12:00:00" {:registry r}))))
  (t/testing "instant"
    (t/is (m/validate :time/instant (Instant/parse "2022-12-18T12:00:25.840823567Z") {:registry r}))
    (t/is (not (m/validate :time/instant "2022-12-18T12:00:25.840823567Z" {:registry r}))))
  (t/testing "zoned date time"
    (t/is (m/validate :time/zoned-date-time (ZonedDateTime/parse "2022-12-18T12:00:25.840823567Z[UTC]") {:registry r}))
    (t/is (m/validate :time/zoned-date-time (ZonedDateTime/parse "2022-12-18T06:00:25.840823567-06:00[America/Chicago]") {:registry r}))
    (t/is (not (m/validate :time/zoned-date-time "2022-12-18T12:00:25.840823567Z[UTC]" {:registry r}))))
  (t/testing "offset date time"
    (t/is (m/validate :time/offset-date-time (OffsetDateTime/parse "2022-12-18T12:00:25.840823567Z") {:registry r}))
    (t/is (m/validate :time/offset-date-time (OffsetDateTime/parse "2022-12-18T06:00:25.840823567-06:00") {:registry r}))
    (t/is (not (m/validate :time/offset-date-time "2022-12-18T12:00:25.840823567Z" {:registry r})))))

(t/deftest min-max
  (t/testing "local date"
    (t/is (m/validate [:time/local-date {:min "2020-01-01" :max (LocalDate/parse "2020-01-03")}] (LocalDate/parse "2020-01-01") {:registry r}))
    (t/is (not (m/validate [:time/local-date {:min "2020-01-01" :max (LocalDate/parse "2020-01-03")}] (LocalDate/parse "2020-01-04") {:registry r})))
    )
  #_
  (t/testing "local time"
    (t/is (m/validate :time/local-time (LocalTime/parse "12:00:00") {:registry r}))
    (t/is (not (m/validate :time/local-time "12:00:00" {:registry r}))))
  #_
  (t/testing "local date time"
    (t/is (m/validate :time/local-date-time (LocalDateTime/parse "2020-01-01T12:00:00") {:registry r}))
    (t/is (not (m/validate :time/local-date-time "2020-01-01T12:00:00" {:registry r}))))
  #_
  (t/testing "instant"
    (t/is (m/validate :time/instant (Instant/parse "2022-12-18T12:00:25.840823567-00:00") {:registry r}))
    (t/is (not (m/validate :time/instant "2022-12-18T12:00:25.840823567-00:00" {:registry r}))))
  #_
  (t/testing "zoned date time"
    (t/is (m/validate :time/zoned-date-time (ZonedDateTime/parse "2022-12-18T12:00:25.840823567Z[UTC]") {:registry r}))
    (t/is (m/validate :time/zoned-date-time (ZonedDateTime/parse "2022-12-18T06:00:25.840823567-06:00[America/Chicago]") {:registry r}))
    (t/is (not (m/validate :time/zoned-date-time "2022-12-18T12:00:25.840823567Z[UTC]" {:registry r}))))
  #_
  (t/testing "offset date time"
    (t/is (m/validate :time/offset-date-time (OffsetDateTime/parse "2022-12-18T12:00:25.840823567Z") {:registry r}))
    (t/is (m/validate :time/offset-date-time (OffsetDateTime/parse "2022-12-18T06:00:25.840823567-06:00") {:registry r}))
    (t/is (not (m/validate :time/offset-date-time "2022-12-18T12:00:25.840823567Z" {:registry r})))))
