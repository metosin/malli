(ns malli.experimental.time
  (:refer-clojure :exclude [<=])
  (:require
   [malli.core :as m]
   [malli.experimental.time :as time])
  (:import
   (java.time Duration LocalDate LocalDateTime LocalTime Instant ZonedDateTime OffsetDateTime ZoneId)))

(set! *warn-on-reflection* true)

(defn <=
  [^Comparable x ^Comparable y]
  (if (pos? (.compareTo x y))
    false
    true))

(defn -min-max-pred [_]
  (fn [{:keys [min max]}]
    (cond
      (not (or min max)) nil
      (and min max) (fn [x] (and (<= x max) (<= min x)))
      min (fn [x] (<= min x))
      max (fn [x] (<= x max)))))

(defn -temporal-schema
  [{:keys [type class type-properties]}]
  (m/-simple-schema
   (cond->
       {:type type
        :pred (fn pred [x] (.isInstance ^Class class x))
        :property-pred (-min-max-pred nil)}
     type-properties
     (assoc :type-properties type-properties))))

(defn duration-schema [] (-temporal-schema {:type :time/duration :class Duration}))
(defn instant-schema [] (-temporal-schema {:type :time/instant :class Instant}))
(defn local-date-schema [] (-temporal-schema {:type :time/local-date :class LocalDate :type-properties {:min LocalDate/MIN :max LocalDate/MAX}}))
(defn local-time-schema [] (-temporal-schema {:type :time/local-time :class LocalTime :type-properties {:min LocalTime/MIN :max LocalTime/MAX}}))
(defn local-date-time-schema [] (-temporal-schema {:type :time/local-date-time :class LocalDateTime :type-properties {:min LocalDateTime/MIN :max LocalDateTime/MAX}}))
(defn offset-date-time-schema [] (-temporal-schema {:type :time/offset-date-time :class OffsetDateTime}))
(defn zoned-date-time-schema [] (-temporal-schema {:type :time/zoned-date-time :class ZonedDateTime}))
(defn zone-id-schema [] (m/-simple-schema {:type :time/zone-id :pred #(instance? ZoneId %)}))

(defn -time-schemas
  []
  {:time/zone-id (zone-id-schema)
   :time/instant (instant-schema)
   :time/duration (duration-schema)
   :time/zoned-date-time (zoned-date-time-schema)
   :time/offset-date-time (offset-date-time-schema)
   :time/local-date (local-date-schema)
   :time/local-time (local-time-schema)
   :time/local-date-time (local-date-time-schema)})
