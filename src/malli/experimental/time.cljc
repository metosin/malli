(ns malli.experimental.time
  (:refer-clojure :exclude [<=])
  (:require [malli.core :as m]
            #?(:cljs ["@js-joda/core" :as js-joda]))
  #?(:clj (:import (java.time Duration LocalDate LocalDateTime LocalTime Instant ZonedDateTime OffsetDateTime ZoneId OffsetTime ZoneOffset))))

(defn <= [^Comparable x ^Comparable y] (not (pos? (.compareTo x y))))

(defn -min-max-pred [_]
  (fn [{:keys [min max]}]
    (cond
      (not (or min max)) nil
      (and min max) (fn [x] (and (<= x max) (<= min x)))
      min (fn [x] (<= min x))
      max (fn [x] (<= x max)))))

(defn -temporal-schema [{:keys [type class type-properties]}]
  (m/-simple-schema
   (cond->
     {:type type
      :pred (fn pred [x]
              #?(:clj  (.isInstance ^Class class x)
                 :cljs (instance? class x)))
      :property-pred (-min-max-pred nil)}
     type-properties
     (assoc :type-properties type-properties))))

#?(:cljs
   (do
     (def Period (.-Period js-joda))
     (def Instant (.-Instant js-joda))
     (def Duration (.-Duration js-joda))
     (def LocalDate (.-LocalDate js-joda))
     (def LocalTime (.-LocalTime js-joda))
     (def ZonedDateTime (.-ZonedDateTime js-joda))
     (def LocalDateTime (.-LocalDateTime js-joda))
     (def MonthDay (.-MonthDay js-joda))
     (def Year (.-Year js-joda))
     (def YearMonth (.-YearMonth js-joda))
     (def ZoneId (.-ZoneId js-joda))
     (def DayOfWeek (.-DayOfWeek js-joda))
     (def Month (.-Month js-joda))
     (def Clock (.-Clock js-joda))
     (def ZoneOffset (.-ZoneOffset js-joda))
     (def OffsetDateTime (.-OffsetDateTime js-joda))
     (def OffsetTime (.-OffsetTime js-joda))
     (def TemporalAccessor (.-TemporalAccessor js-joda))
     (def TemporalQuery (.-TemporalQuery js-joda))
     (def DateTimeFormatter (.-DateTimeFormatter js-joda))))

#?(:cljs
   (defn createTemporalQuery [f]
     (let [parent (TemporalQuery. "")
           query (js/Object.create parent)]
       (set! (.-queryFrom query) (fn [t] (f t)))
       query)))

(defn -duration-schema [] (-temporal-schema {:type :time/duration :class Duration}))
(defn -instant-schema [] (-temporal-schema {:type :time/instant :class Instant}))
(defn -local-date-schema [] (-temporal-schema {:type :time/local-date :class LocalDate :type-properties {:min (. LocalDate -MIN) :max (. LocalDate -MAX)}}))
(defn -local-time-schema [] (-temporal-schema {:type :time/local-time :class LocalTime :type-properties {:min (. LocalTime -MIN) :max (. LocalTime -MAX)}}))
(defn -local-date-time-schema [] (-temporal-schema {:type :time/local-date-time :class LocalDateTime :type-properties {:min (. LocalDateTime -MIN) :max (. LocalDateTime -MAX)}}))
(defn -offset-date-time-schema [] (-temporal-schema {:type :time/offset-date-time :class OffsetDateTime}))
(defn -offset-time-schema [] (-temporal-schema {:type :time/offset-time :class OffsetTime :type-properties {:min (. OffsetTime -MIN) :max (. OffsetTime -MAX)}}))
(defn -zoned-date-time-schema [] (-temporal-schema {:type :time/zoned-date-time :class ZonedDateTime}))
(defn -zone-id-schema [] (m/-simple-schema {:type :time/zone-id :pred #(instance? ZoneId %)}))
(defn -zone-offset-schema [] (m/-simple-schema {:type :time/zone-offset :pred #(instance? ZoneOffset %) :type-properties {:min (. ZoneOffset -MIN) :max (. ZoneOffset -MAX)}}))

(defn schemas []
  {:time/zone-id (-zone-id-schema)
   :time/instant (-instant-schema)
   :time/duration (-duration-schema)
   :time/zoned-date-time (-zoned-date-time-schema)
   :time/offset-date-time (-offset-date-time-schema)
   :time/local-date (-local-date-schema)
   :time/local-time (-local-time-schema)
   :time/offset-time (-offset-time-schema)
   :time/zone-offset (-zone-offset-schema)
   :time/local-date-time (-local-date-time-schema)})
