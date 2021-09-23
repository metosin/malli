(ns malli.time
  (:require
   [clojure.string :as str]
   [malli.core :as m])
  (:import
   (java.time Duration LocalDate OffsetDateTime OffsetTime)))

(defn ->safe-parser
  [f]
  (fn -parse [x]
    (if (string? x)
      (try
        (f x)
        (catch Exception _ x))
      x)))

(defn ->error-reporter
  [parser message]
  (fn -report [value]
    (if (string? value)
      (try
        (parser value)
        (catch Exception e (ex-message e)))
      message)))

(defn -string->duration [x]
  (Duration/parse x))

(defn -string->offset-date-time [x]
  (OffsetDateTime/parse x))

(defn -string->local-date [x]
  (LocalDate/parse x))

(defn -string->offset-time [x]
  (OffsetTime/parse x))

(def default-date-types
  {:local-date {:class LocalDate :parser -string->local-date :json-schema/type :date}
   :offset-time {:class OffsetTime :parser -string->offset-time :json-schema/type :time}
   :offset-date-time {:class OffsetDateTime :parser -string->offset-date-time :json-schema/type :date-time}
   :duration {:class Duration :parser -string->duration}})

(defn class-base-name
  [^Class c]
  (peek (str/split (.getName c) #"\.")))

(defn time-schema
  [type]
  (when-let [props (get default-date-types type)]
    (let [{klass :class parser :parser} props
          pred #(instance? klass %)
          safe-parser (->safe-parser parser)
          -name (name type)
          message (str "Should be " -name " or " (class-base-name klass))
          error-fn (->error-reporter parser message)]
      (m/-simple-schema
       {:type type
        :type-properties
        {:error/fn error-fn
         :decode/json {:enter safe-parser}
         :json-schema/type (:json-schema/type props -name)}
        :pred pred}))))

(defn time-schemas []
  (reduce
   (fn [m k] (assoc m k (time-schema k)))
   {}
   (keys default-date-types)))
