(ns malli.experimental.time.transform
  (:require [malli.transform :as mt :refer [-safe]]
            [malli.core :as m]
            #?(:cljs [malli.experimental.time :as time
                      :refer [Duration Period LocalDate LocalDateTime LocalTime Instant OffsetTime ZonedDateTime OffsetDateTime ZoneId ZoneOffset
                              TemporalAccessor TemporalQuery DateTimeFormatter createTemporalQuery]]))
  #?(:clj
     (:import (java.time Duration Period LocalDate LocalDateTime LocalTime Instant ZonedDateTime OffsetDateTime ZoneId OffsetTime ZoneOffset)
              (java.time.temporal TemporalAccessor TemporalQuery)
              (java.time.format DateTimeFormatter))))

#?(:clj (set! *warn-on-reflection* true))

#?(:clj
   (defn ->temporal-query ^TemporalQuery [f]
     (reify TemporalQuery
       (queryFrom [_ t]
         (f t))))
   :cljs
   (defn ->temporal-query ^TemporalQuery [f]
     (createTemporalQuery f)))

(defn ->parser [formatter qf]
  (let [query (->temporal-query qf)]
    (fn [#?(:clj ^CharSequence s :cljs s)]
      (if #?(:clj (instance? CharSequence s) :cljs (string? s))
        (.parse ^DateTimeFormatter formatter s query)
        s))))

(defn ->formatter [x]
  (cond
    (instance? DateTimeFormatter x) x
    #?(:clj  (instance? String x)
       :cljs (string? x)) (. DateTimeFormatter ofPattern x)
    :else (throw (ex-info "Invalid formatter" {:formatter x :type (type x)}))))

(def default-formats
  {:time/instant (. DateTimeFormatter -ISO_INSTANT)
   :time/local-date (. DateTimeFormatter -ISO_LOCAL_DATE)
   :time/local-date-time (. DateTimeFormatter -ISO_LOCAL_DATE_TIME)
   :time/local-time (. DateTimeFormatter -ISO_LOCAL_TIME)
   :time/offset-time (. DateTimeFormatter -ISO_OFFSET_TIME)
   :time/offset-date-time (. DateTimeFormatter -ISO_OFFSET_DATE_TIME)
   :time/zoned-date-time (. DateTimeFormatter -ISO_ZONED_DATE_TIME)})

(def queries
  {:time/instant #(. Instant from %)
   :time/local-time #(. LocalTime from %)
   :time/local-date #(. LocalDate from %)
   :time/local-date-time #(. LocalDateTime from %)
   :time/offset-date-time #(. OffsetDateTime from %)
   :time/offset-time #(. OffsetTime from %)
   :time/zoned-date-time #(. ZonedDateTime from %)})

(def default-parsers
  (reduce-kv
   (fn [m k v] (assoc m k (-safe (->parser v (get queries k)))))
   {:time/duration (-safe #(. Duration parse %))
    :time/period (-safe #(. Period parse %))
    :time/zone-offset (-safe #(. ZoneOffset of ^String %))
    :time/zone-id (-safe #(. ZoneId of %))}
   default-formats))

(defn compile-parser [type formatter pattern]
  (when-let [formatter (when-let [x (or formatter pattern)]
                         (->formatter x))]
    (-safe (->parser formatter (get queries type)))))

(defn time-decoders [formats]
  (into
   default-parsers
   (for [k (keys formats)]
     [k {:compile
         (fn [schema opts]
           (let [t (m/type schema opts)
                 {:keys [formatter pattern]} (m/properties schema)]
             (or (compile-parser t formatter pattern)
                 (get default-parsers t))))}])))

(defn time-encoders [formats]
  (into
   {:time/duration str
    :time/period str
    :time/zone-id str}
   (for [k (keys formats)]
     [k {:compile
         (fn [schema opts]
           (let [t (m/type schema opts)
                 {:keys [formatter pattern]} (m/properties schema)
                 formatter (->formatter (or formatter pattern (get default-formats t)))]
             (-safe
              (fn [^TemporalAccessor ta]
                (if (instance? TemporalAccessor ta)
                  (.format ^DateTimeFormatter formatter ta)
                  ta)))))}])))

(defn time-transformer
  ([] (time-transformer default-formats))
  ([formats]
   (mt/transformer
    {:name :time
     :decoders (time-decoders formats)
     :encoders (time-encoders formats)})))
