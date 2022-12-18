(ns malli.experimental.time.transform
  (:import
   (java.time.format DateTimeFormatter)
   (java.time.temporal TemporalAccessor))
  (:require
   [malli.experimental.time :as time]
   [malli.transform :as mt]
   [malli.core :as m]))

(set! *warn-on-reflection* true)

(defn time-decoders
  [formats]
  (into
   time/default-parsers
   (for [k (keys formats)]
     [k {:compile
         (fn [schema opts]
           (let [t (m/type schema opts)
                 {:keys [formatter pattern]} (m/properties schema)]
             (time/compile-parser t formatter pattern)))}])))

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
                 formatter (time/->formatter (or formatter pattern (get time/default-formats t)))]
             (time/safe-fn
              (fn [^TemporalAccessor ta]
                (if (instance? TemporalAccessor ta)
                  (.format ^DateTimeFormatter formatter ta)
                  ta)))))}])))

(defn time-transformer
  ([] (time-transformer time/default-formats))
  ([formats]
   (mt/transformer
    {:name :time
     :decoders (time-decoders formats)
     :encoders (time-encoders formats)})))

#_(m/decode (time/local-date-schema) "2020-01-01" (time-transformer))
#_(m/decode (time/local-time-schema) "18:00:01" (time-transformer))
