(ns malli.experimental.js-joda-time
  (:refer-clojure :exclude [<=])
  (:require
    [malli.core :as m]
    [goog.object]
    ["@js-joda/core" :as js-joda]))

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
       :pred (fn pred [x] (instance? ^Class class x))
       :property-pred (-min-max-pred nil)}
      type-properties
      (assoc :type-properties type-properties))))

(defn -duration-schema [] (-temporal-schema {:type :time/duration :class (goog.object/get js-joda "Duration")}))

(defn schemas []
  { ;:time/zone-id (-zone-id-schema)
   ;:time/instant (-instant-schema)
   :time/duration (-duration-schema)
   ;:time/zoned-date-time (-zoned-date-time-schema)
   ;:time/offset-date-time (-offset-date-time-schema)
   ;:time/local-date (-local-date-schema)
   ;:time/local-time (-local-time-schema)
   ;:time/offset-time (-offset-time-schema)
   ;:time/zone-offset (-zone-offset-schema)
   ;:time/local-date-time (-local-date-time-schema)
   })
