(ns malli.experimental.time.generator
  (:require [clojure.test.check.generators :as gen]
            [clojure.spec.gen.alpha :as ga]
            [malli.core :as m]
            [malli.generator :as mg]
            #?(:clj  [malli.experimental.time :as time]
               :cljs [malli.experimental.time :as time
                      :refer [Duration Period LocalDate LocalDateTime LocalTime Instant OffsetTime ZonedDateTime OffsetDateTime ZoneId ZoneOffset]]))
  #?(:clj (:import (java.time Duration Period LocalDate LocalDateTime LocalTime Instant OffsetTime ZonedDateTime OffsetDateTime ZoneId ZoneOffset))))

#?(:clj (set! *warn-on-reflection* true))

(defn zone-id-gen []
  (mg/generator (m/into-schema (m/-enum-schema) nil (map #(. ZoneId of ^String %) (. ZoneId getAvailableZoneIds)))))

(defmethod mg/-schema-generator :time/zone-id [_schema _options] (zone-id-gen))

#?(:clj  (def ^:const ^:private seconds-in-day 86400)
   :cljs (def ^:private seconds-in-day 86400))

(defn -to-long ^long [o]
  (cond
    (instance? Instant o) (.toEpochMilli ^Instant o)
    (instance? LocalDate o) (.toEpochDay ^LocalDate o)
    (instance? LocalTime o) (.toSecondOfDay ^LocalTime o)
    (instance? ZoneOffset o) #?(:clj  (.getTotalSeconds ^ZoneOffset o)
                                :cljs (.totalSeconds ^ZoneOffset o))
    (instance? LocalDateTime o)
    (unchecked-add
     (unchecked-multiply (.toEpochDay (.toLocalDate ^LocalDateTime o)) seconds-in-day)
     (-to-long (.toLocalTime ^LocalDateTime o)))
    (instance? OffsetDateTime o) (.toEpochMilli (.toInstant ^OffsetDateTime o))
    (instance? ZonedDateTime o) (.toEpochMilli (.toInstant ^ZonedDateTime o))
    (instance? Duration o) (.toNanos ^Duration o)
    (int? o) (long o)))

(defn to-long [o] (when o (-to-long o)))

(defn -min-max [schema options]
  (let [{:keys [min max] gen-min :gen/min gen-max :gen/max}
        (merge
         (m/type-properties schema options)
         (m/properties schema options))
        {:keys [accessor] :or {accessor identity}} options
        as-long #(when % (to-long (accessor %)))
        min (as-long min) max (as-long max) gen-min (as-long gen-min) gen-max (as-long gen-max)]
    (when (and min gen-min (< gen-min min))
      (m/-fail! ::mg/invalid-property {:key :gen/min, :value gen-min, :min min}))
    (when (and max gen-max (> gen-max max))
      (m/-fail! ::mg/invalid-property {:key :gen/max, :value gen-min, :max min}))
    {:min (or gen-min min)
     :max (or gen-max max)}))

(defn -zone-offset-gen [schema options]
  (ga/fmap #(. ZoneOffset ofTotalSeconds %) (gen/large-integer* (-min-max schema options))))

(defmethod mg/-schema-generator :time/zone-offset [schema options]
  (-zone-offset-gen schema options))

(defn -instant-gen [schema options]
  (ga/fmap #(. Instant ofEpochMilli %) (gen/large-integer* (-min-max schema options))))

(defmethod mg/-schema-generator :time/instant [schema options]
  (-instant-gen schema options))

(comment
 (gen/sample (mg/-schema-generator (time/-instant-schema) nil)))

(defmethod mg/-schema-generator :time/local-date [schema options]
  (ga/fmap #(. LocalDate ofEpochDay %) (gen/large-integer* (-min-max schema options))))

(defn -local-time-gen [schema options]
  (ga/fmap #(. LocalTime ofSecondOfDay %) (gen/large-integer* (-min-max schema options))))

(defmethod mg/-schema-generator :time/local-time [schema options]
  (-local-time-gen schema options))

(comment
 (gen/sample (mg/-schema-generator (time/-local-time-schema) nil)))

(defn -offset-time-gen [schema options]
  (let [local-opts (assoc options :accessor #(.toLocalTime ^OffsetTime %))
        zone-opts #?(:clj (assoc options :accessor #(- (.getTotalSeconds (.getOffset ^OffsetTime %))))
                     :cljs (assoc options :accessor #(- (.totalSeconds (.offset ^js %)))))
        offset-gen (-zone-offset-gen schema zone-opts)]
    (ga/bind
     (-local-time-gen schema local-opts)
     (fn [local-time]
       (ga/fmap #(. OffsetTime of local-time %) offset-gen)))))

(defmethod mg/-schema-generator :time/offset-time [schema options]
  (-offset-time-gen schema options))

(comment
 (gen/sample (mg/-schema-generator (time/-offset-time-schema) nil)))

(defmethod mg/-schema-generator :time/local-date-time [schema options]
  (gen/fmap
   (fn [n]
     (. LocalDateTime of
        (. LocalDate ofEpochDay (quot n seconds-in-day))
        (. LocalTime ofSecondOfDay (mod n seconds-in-day))))
   (gen/large-integer* (-min-max schema options))))

(comment
 (gen/sample (mg/-schema-generator (time/-local-date-time-schema) nil) 1000))

(defn -zoned-date-time-gen [schema options]
  (gen/bind
   (-instant-gen schema options)
   (fn [instant]
     (gen/fmap #(. ZonedDateTime ofInstant instant %) (zone-id-gen)))))

(defmethod mg/-schema-generator :time/zoned-date-time [schema options]
  (-zoned-date-time-gen schema options))

(comment
 (gen/sample (mg/-schema-generator (time/-zoned-date-time-schema) nil) 100))

(defn -offset-date-time-gen [schema options]
  (gen/fmap #(. OffsetDateTime from %) (-zoned-date-time-gen schema options)))

(defmethod mg/-schema-generator :time/offset-date-time [schema options]
  (-offset-date-time-gen schema options))

(defmethod mg/-schema-generator :time/duration [schema options]
  (gen/fmap #(. Duration ofNanos %) (gen/large-integer* (-min-max schema options))))

;; Years, Months, Days of periods are never nil, they just return zero, so we treat zero as nil.
(defmethod mg/-schema-generator :time/period [schema options]
  (let [zero->nil (fn [v] (if (zero? v) nil v))
        max-int #?(:clj Integer/MAX_VALUE :cljs (.-MAX_SAFE_INTEGER js/Number))
        min-int #?(:clj Integer/MIN_VALUE :cljs (.-MIN_SAFE_INTEGER js/Number))
        ceil-max (fn [v] (if (nil? v) max-int (min max-int v)))
        floor-min (fn [v] (if (nil? v) min-int (max min-int v)))
        {^Period mn :min ^Period mx :max ^Period gen-min :gen/min ^Period gen-max :gen/max}
        (merge
         (m/type-properties schema options)
         (m/properties schema options))
        _ (when (and mn gen-min (not (pos? (time/compare-periods gen-min min))))
            (m/-fail! ::mg/invalid-property {:key :gen/min, :value gen-min, :min min}))
        _ (when (and mx gen-max (not (pos? (time/compare-periods max gen-max))))
            (m/-fail! ::mg/invalid-property {:key :gen/max, :value gen-min, :max min}))
        mn (or mn gen-min)
        mx (or mx gen-max)
        min-years (when mn (zero->nil (.getYears mn))), max-years (when mx (zero->nil (.getYears mx)))
        min-months (when mn (zero->nil (.getMonths mn))), max-months (when mx (zero->nil (.getMonths mx)))
        min-days (when mn (zero->nil (.getDays mn))), max-days (when mx (zero->nil (.getDays mx)))]
    (->>
     (gen/tuple
      ;; Period constructor only accepts java type `int` not `long`, clamp the values
      (gen/large-integer* {:min (floor-min min-years) :max (ceil-max max-years)})
      (gen/large-integer* {:min (floor-min min-months) :max (ceil-max max-months)})
      (gen/large-integer* {:min (floor-min min-days) :max (ceil-max max-days)}))
     (gen/fmap (fn [[years months days]]
                 (. Period of years months days))))))
