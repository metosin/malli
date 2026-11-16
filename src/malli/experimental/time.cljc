(ns malli.experimental.time
  (:refer-clojure :exclude [<=])
  (:require [malli.core :as m]
            #?(:cljs ["@js-joda/core" :as js-joda]))
  #?(:clj (:import (java.time Duration Period LocalDate LocalDateTime LocalTime Instant ZonedDateTime OffsetDateTime ZoneId OffsetTime ZoneOffset))))

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

(defn <= [^Comparable x ^Comparable y] (not (pos? (.compareTo x y))))

(defn compare-periods
  "Periods are not comparable in the java Comparable sense, instead this performs simple units-by-units comparison.
   So a period of 1 year will always compare greater than a period of 13 months and similar for days and months."
  [^Period p1 ^Period p2]
  (let [years1 #?(:clj (.getYears p1) :cljs (.years p1))
        years2 #?(:clj (.getYears p2) :cljs (.years p2))
        months1 #?(:clj (.getMonths p1) :cljs (.months p1))
        months2 #?(:clj (.getMonths p2) :cljs (.months p2))
        days1 #?(:clj (.getDays p1) :cljs (.days p1))
        days2 #?(:clj (.getDays p2) :cljs (.days p2))]
    (cond
      (not (= years1 years2)) (- years1 years2)
      (not (= months1 months2)) (- months1 months2)
      :else (- days1 days2))))

(defn -min-max-pred [_]
  (fn [{:keys [min max]}]
    (cond
      (not (or min max)) nil
      (and min max)
      (if (and (instance? Period min) (instance? Period max))
        (fn [^Period x]
          (and
           (not (pos? (compare-periods x max)))
           (not (pos? (compare-periods min x)))))
        (fn [x] (and (<= x max) (<= min x))))
      min (fn [x]
            (if (instance? Period min)
              (not (pos? (compare-periods min x)))
              (<= min x)))
      max (fn [x]
            (if (instance? Period max)
              (not (pos? (compare-periods x max)))
              (<= x max))))))

(defn -less-than? [x y]
  (if (instance? Period x)
    (neg? (compare-periods x y))
    (not (<= y x))))

(defn -min-max-error-fn [pred message]
  (fn [{:keys [schema value negated]} _]
    (let [{:keys [min max]} (m/properties schema)]
      (cond
        (not (pred value)) message
        (and min max (#?(:clj = :cljs .equals) min max)) (str "should be " min)
        (and min (if negated (not (-less-than? value min)) (-less-than? value min))) (str "should be at least " min)
        max (str "should be at most " max)
        negated message))))

(defn -temporal-schema [{:keys [type class type-properties message]}]
  (let [pred (fn pred [x]
               #?(:clj  (.isInstance ^Class class x)
                  :cljs (instance? class x)))]
    (m/-simple-schema
     {:type type
      :pred pred
      :property-pred (-min-max-pred nil)
      :type-properties (assoc type-properties :error/fn {:en (-min-max-error-fn pred message)})})))

#?(:cljs
   (defn createTemporalQuery [f]
     (let [parent (TemporalQuery. "")
           query (js/Object.create parent)]
       (set! (.-queryFrom query) (fn [t] (f t)))
       query)))

(defn -duration-schema [] (-temporal-schema {:type :time/duration :class Duration :message "should be a duration"}))
(defn -period-schema [] (-temporal-schema {:type :time/period :class Period :message "should be a period"}))
(defn -instant-schema [] (-temporal-schema {:type :time/instant :class Instant :message "should be an instant"}))
(defn -local-date-schema [] (-temporal-schema {:type :time/local-date :class LocalDate :message "should be a local date" :type-properties {:min (. LocalDate -MIN) :max (. LocalDate -MAX)}}))
(defn -local-time-schema [] (-temporal-schema {:type :time/local-time :class LocalTime :message "should be a local time" :type-properties {:min (. LocalTime -MIN) :max (. LocalTime -MAX)}}))
(defn -local-date-time-schema [] (-temporal-schema {:type :time/local-date-time :class LocalDateTime :message "should be a local date time" :type-properties {:min (. LocalDateTime -MIN) :max (. LocalDateTime -MAX)}}))
(defn -offset-date-time-schema [] (-temporal-schema {:type :time/offset-date-time :class OffsetDateTime :message "should be an offset date time"}))
(defn -offset-time-schema [] (-temporal-schema {:type :time/offset-time :class OffsetTime :message "should be an offset time" :type-properties {:min (. OffsetTime -MIN) :max (. OffsetTime -MAX)}}))
(defn -zoned-date-time-schema [] (-temporal-schema {:type :time/zoned-date-time :class ZonedDateTime :message "should be a zoned date time"}))
(defn -zone-id-schema [] (m/-simple-schema {:type :time/zone-id :pred #(instance? ZoneId %) :type-properties {:error/message {:en "should be a zone id"}}}))
(defn -zone-offset-schema [] (m/-simple-schema {:type :time/zone-offset :pred #(instance? ZoneOffset %) :type-properties {:min (. ZoneOffset -MIN) :max (. ZoneOffset -MAX), :error/message {:en "should be a zone offset"}}}))

(defn schemas []
  {:time/zone-id (-zone-id-schema)
   :time/instant (-instant-schema)
   :time/duration (-duration-schema)
   :time/period (-period-schema)
   :time/zoned-date-time (-zoned-date-time-schema)
   :time/offset-date-time (-offset-date-time-schema)
   :time/local-date (-local-date-schema)
   :time/local-time (-local-time-schema)
   :time/offset-time (-offset-time-schema)
   :time/zone-offset (-zone-offset-schema)
   :time/local-date-time (-local-date-time-schema)})
