(ns malli.experimental.time.transform-test
  (:require [malli.core :as m]
            [malli.experimental.time-test :refer [r]]
            [malli.experimental.time.transform :as time.transform]
            [clojure.test :as t]))

(defn validate
  ([schema v]
   (validate schema v {:registry r}))
  ([schema v options]
   (validate schema v time.transform/time-transformer options))
  ([schema v transformer options]
   (m/validate schema (m/decode schema v options transformer) options)))

(t/deftest decode
  (t/testing "Duration"
    (t/is (validate :time/duration "PT0.01S"))
    (t/is (not (validate :time/duration 10))))
  (t/testing "Period"
    (t/is (validate :time/period "P-1Y10D"))
    (t/is (validate :time/period "P-1Y8M"))
    (t/is (not (validate :time/period 10))))
  (t/testing "zone id"
    (t/is (validate :time/zone-id "UTC"))
    (t/is (not (validate :time/zone-id "UTC'"))))
  (t/testing "zone offset"
    (t/is (validate :time/zone-offset "+15:00"))
    (t/is (not (validate :time/zone-offset "UTC"))))
  (t/testing "local date"
    (t/is (validate :time/local-date "2020-01-01"))
    (t/testing "Pattern"
      (t/is (validate [:time/local-date {:pattern "yyyyMMdd"}] "20200101")))
    (t/is (not (validate :time/local-date "202001-01"))))
  (t/testing "local time"
    (t/is (validate :time/local-time "12:00:00"))
    (t/is (not (validate :time/local-time "$12:00:00"))))
  (t/testing "local date time"
    (t/is (validate :time/local-date-time "2020-01-01T12:00:00"))
    (t/is (not (validate :time/local-date-time "2x020-01-01T12:00:00"))))
  (t/testing "instant"
    (t/is (validate :time/instant "2022-12-18T12:00:25.840823567Z"))
    (t/is (not (validate :time/instant "2022-12-ABC18T12:00:25.840823567Z"))))
  (t/testing "zoned date time"
    (t/is (validate :time/zoned-date-time "2022-12-18T12:00:25.840823567Z[UTC]"))
    (t/is (validate :time/zoned-date-time "2022-12-18T06:00:25.840823567-06:00[America/Chicago]"))
    (t/is (not (validate :time/zoned-date-time "20%22-12-18T12:00:25.840823567Z[UTC]"))))
  (t/testing "offset date time"
    (t/is (validate :time/offset-date-time "2022-12-18T12:00:25.840823567Z"))
    (t/is (validate :time/offset-date-time "2022-12-18T06:00:25.840823567-06:00"))
    (t/is (not (validate :time/offset-date-time "2_022-12-18T12:00:25.840823567Z"))))
  (t/testing "Aggregates"
    (t/is (validate [:map [:date :time/local-date]] {:date "2020-01-01"}))))

(defn -decode [schema v]
  (m/decode schema v {:registry r} time.transform/time-transformer))

(defn -encode [schema v]
  (m/encode schema v {:registry r} time.transform/time-transformer))

(t/deftest encode
  (t/testing "Round trip with patterns"
    (t/is (= "20200101" (->> "20200101"
                             (-decode [:time/local-date {:pattern "yyyyMMdd"}])
                             (-encode [:time/local-date {:pattern "yyyyMMdd"}]))))
    (t/is (= "2020_01_01" (->> "20200101"
                               (-decode [:time/local-date {:pattern "yyyyMMdd"}])
                               (-encode [:time/local-date {:pattern "yyyy_MM_dd"}]))))))


