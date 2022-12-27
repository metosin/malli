(ns malli.experimental.time.transform
  (:import
   (java.time Duration LocalDate LocalDateTime LocalTime Instant ZonedDateTime OffsetDateTime ZoneId OffsetTime ZoneOffset)
   (java.time.temporal TemporalAccessor TemporalQuery)
   (java.time.format DateTimeFormatter))
  (:require
   [malli.transform :as mt]
   [malli.core :as m]))

(set! *warn-on-reflection* true)

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

(def default-formats
  {:time/instant DateTimeFormatter/ISO_INSTANT
   :time/local-date DateTimeFormatter/ISO_LOCAL_DATE
   :time/local-date-time DateTimeFormatter/ISO_LOCAL_DATE_TIME
   :time/local-time DateTimeFormatter/ISO_LOCAL_TIME
   :time/offset-time DateTimeFormatter/ISO_OFFSET_TIME
   :time/offset-date-time DateTimeFormatter/ISO_OFFSET_DATE_TIME
   :time/zoned-date-time DateTimeFormatter/ISO_ZONED_DATE_TIME})

(def queries
  {:time/instant #(Instant/from %)
   :time/local-time #(LocalTime/from %)
   :time/local-date #(LocalDate/from %)
   :time/local-date-time #(LocalDateTime/from %)
   :time/offset-date-time #(OffsetDateTime/from %)
   :time/offset-time #(OffsetTime/from %)
   :time/zoned-date-time #(ZonedDateTime/from %)})

(def default-parsers
  (reduce-kv
   (fn [m k v] (assoc m k (safe-fn (->parser v (get queries k)))))
   {:time/duration (safe-fn #(Duration/parse %))
    :time/zone-offset (safe-fn #(ZoneOffset/of ^String %))
    :time/zone-id (safe-fn #(ZoneId/of %))}
   default-formats))

(defn compile-parser
  [type formatter pattern]
  (when-let [formatter (when-let [x (or formatter pattern)]
                         (->formatter x))]
    (safe-fn (->parser formatter (get queries type)))))

(defn time-decoders
  [formats]
  (into
   default-parsers
   (for [k (keys formats)]
     [k {:compile
         (fn [schema opts]
           (let [t (m/type schema opts)
                 {:keys [formatter pattern]} (m/properties schema)]
             (or (compile-parser t formatter pattern)
                 (get default-parsers t))))}])))

(defn time-encoders
  [formats]
  (into
   {:time/duration str
    :time/zone-id str}
   (for [k (keys formats)]
     [k {:compile
         (fn [schema opts]
           (let [t (m/type schema opts)
                 {:keys [formatter pattern]} (m/properties schema)
                 formatter (->formatter (or formatter pattern (get default-formats t)))]
             (safe-fn
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

#_(m/decode (time/local-date-schema) "2020-01-01" (time-transformer))
#_(m/decode (time/local-time-schema) "18:00:01" (time-transformer))
