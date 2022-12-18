(ns malli.experimental.time
  (:require
   [malli.core :as m]
   [malli.experimental.time :as time])
  (:import
   (java.time.temporal TemporalQuery)
   (java.time Duration LocalDate LocalDateTime LocalTime Instant ZonedDateTime OffsetDateTime ZoneId)
   (java.time.format DateTimeFormatter)))

(set! *warn-on-reflection* true)

(def default-formats
  {:time/instant DateTimeFormatter/ISO_INSTANT
   :time/local-date DateTimeFormatter/ISO_LOCAL_DATE
   :time/local-date-time DateTimeFormatter/ISO_LOCAL_DATE_TIME
   :time/local-time DateTimeFormatter/ISO_LOCAL_TIME
   :time/offset-date-time DateTimeFormatter/ISO_OFFSET_DATE_TIME
   :time/zoned-date-time DateTimeFormatter/ISO_ZONED_DATE_TIME})

(def queries
  {:time/instant #(Instant/from %)
   :time/local-time #(LocalTime/from %)
   :time/local-date #(LocalDate/from %)
   :time/local-date-time #(LocalDateTime/from %)
   :time/offset-date-time #(OffsetDateTime/from %)
   :time/zoned-date-time #(ZonedDateTime/from %)})

(defn ->temporal-query
  ^TemporalQuery [f]
  (reify TemporalQuery
    (queryFrom [_ t]
      (f t))))

(defn ->parser
  [formatter qf]
  (let [query (->temporal-query qf)]
    (fn [^CharSequence s]
      (if (instance? CharSequence s)
        (.parse ^DateTimeFormatter formatter s query)
        s))))

(def default-parsers
  (reduce-kv
   (fn [m k v] (assoc m k (->parser v (get queries k))))
   {:time/duration #(Duration/parse %)
    :time/zone-id #(ZoneId/of %)}
   default-formats))

(defn ->formatter
  [x]
  (cond
    (instance? DateTimeFormatter x) x
    (instance? String x) (DateTimeFormatter/ofPattern x)
    :else (throw (ex-info "Invalid formatter" {:formatter x :type (type x)}))))

(defn safe-fn
  [f]
  (fn safe [x]
    (try
      (f x)
      (catch Exception _
        x))))

(defn t<=
  [^Comparable x ^Comparable y]
  (if (pos? (.compareTo x y))
    false
    true))

(defn compile-parser
  [type formatter pattern]
  (safe-fn
   (or
    (when-let [formatter (when-let [x (or formatter pattern)]
                           (->formatter x))]
      (->parser formatter (get queries type)))
    (get default-parsers type))))

(defn -min-max-pred [type]
  (fn [{:keys [min max formatter pattern]}]
    (let [f (compile-parser type formatter pattern)
          min (f min) max (f max)]
      (cond
        (not (or min max)) nil
        (and min max) (fn [x] (and (t<= x max) (t<= min x)))
        min (fn [x] (t<= min x))
        max (fn [x] (t<= x max))))))

(defn -temporal-schema
  [{:keys [type class type-properties]}]
  (m/-simple-schema
   (cond->
       {:type type
        :pred (fn pred [x] (.isInstance ^Class class x))
        :property-pred (-min-max-pred type)}
     type-properties
     (assoc :type-properties type-properties))))

(defn duration-schema [] (-temporal-schema {:type :time/duration :class Duration}))
(defn instant-schema [] (-temporal-schema {:type :time/instant :class Instant}))
(defn local-date-schema [] (-temporal-schema {:type :time/local-date :class LocalDate}))
(defn local-time-schema [] (-temporal-schema {:type :time/local-time :class LocalTime :type-properties {:min LocalTime/MIN :max LocalTime/MAX}}))
(defn local-date-time-schema [] (-temporal-schema {:type :time/local-date-time :class LocalDateTime}))
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
