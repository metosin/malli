(ns malli.transform
  #?(:cljs (:refer-clojure :exclude [Inst Keyword UUID]))
  (:require #?@(:cljs [[goog.date.UtcDateTime]
                       [goog.date.Date]])
            [malli.core :as m])
  #?(:clj
     (:import (java.util Date UUID)
              (java.time Instant ZoneId)
              (java.time.format DateTimeFormatter DateTimeFormatterBuilder)
              (java.time.temporal ChronoField))))

;;
;; Strings
;;

(defn string->long [x]
  (if (string? x)
    (try
      #?(:clj  (Long/parseLong x)
         :cljs (let [x' (js/parseInt x 10)]
                 (if (js/isNaN x') x x')))
      (catch #?(:clj Exception, :cljs js/Error) _ x))
    x))

(defn string->double [x]
  (if (string? x)
    (try
      #?(:clj  (Double/parseDouble x)
         :cljs (let [x' (js/parseFloat x)]
                 (if (js/isNaN x') x x')))
      (catch #?(:clj Exception, :cljs js/Error) _ x))
    x))

(defn string->keyword [x]
  (if (string? x)
    (keyword x)
    x))

(defn string->boolean [x]
  (if (string? x)
    (cond
      (= "true" x) true
      (= "false" x) false
      :else x)
    x))

(defn string->uuid [x]
  (if (string? x)
    (try
      #?(:clj  (UUID/fromString x)
         ;; http://stackoverflow.com/questions/7905929/how-to-test-valid-uuid-guid
         :cljs (if (re-find #"^[0-9a-f]{8}-[0-9a-f]{4}-[1-5][0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$" x)
                 (uuid x)
                 x))
      (catch #?(:clj Exception, :cljs js/Error) _ x))
    x))

#?(:clj
   (def ^DateTimeFormatter +string->date-format+
     (-> (DateTimeFormatterBuilder.)
         (.appendPattern "yyyy-MM-dd['T'HH:mm:ss[.SSS][XXXX][XXXXX]]")
         (.parseDefaulting ChronoField/HOUR_OF_DAY 0)
         (.parseDefaulting ChronoField/OFFSET_SECONDS 0)
         (.toFormatter))))

(defn string->date [x]
  (if (string? x)
    (try
      #?(:clj  (Date/from (Instant/from (.parse +string->date-format+ x)))
         :cljs (js/Date. (.getTime (goog.date.UtcDateTime.fromIsoString x))))
      (catch #?(:clj Exception, :cljs js/Error) _ x))
    x))

#?(:clj
   (def ^DateTimeFormatter +date->string-format+
     (-> (DateTimeFormatter/ofPattern "yyyy-MM-dd'T'HH:mm:ss.SSSXXX")
         (.withZone (ZoneId/of "UTC")))))

(defn date->string [x]
  (if (inst? x)
    (try
      #?(:clj  (.format +date->string-format+ (Instant/ofEpochMilli (inst-ms x)))
         :cljs (.toISOString x))
      (catch #?(:clj Exception, :cljs js/Error) _ x))
    x))

(defn string->symbol [x]
  (if (string? x)
    (symbol x)
    x))

(defn string->nil [x]
  (if (= "" x)
    nil
    x))

(defn any->string [x]
  (if-not (nil? x)
    (str x)))

(defn number->double [x]
  (if (number? x)
    (double x)
    x))

(defn any->any [x] x)

;;
;; transformers
;;

(def +json-decoders+
  {'ident? string->keyword
   'simple-ident? string->keyword
   'qualified-ident? string->keyword

   'keyword? string->keyword
   'simple-keyword? string->keyword
   'qualified-keyword? string->keyword

   'symbol? string->symbol
   'simple-symbol? string->symbol
   'qualified-symbol? string->symbol

   'uuid? string->uuid

   'inst? string->date})

(def +string-encoders+
  (merge
    +json-decoders+
    {'integer? string->long
     'int? string->long
     'pos-int? string->long
     'neg-int? string->long
     'nat-int? string->long
     'zero? string->long

     :> string->long
     :>= string->long
     :< string->long
     :<= string->long
     := string->long
     :not= string->long

     'number? string->double
     'float? string->double
     'double? string->double
     #?@(:clj ['rational? string->double])

     'boolean? string->boolean
     'false? string->boolean
     'true? string->boolean}))

(defn string-transformer [schema]
  (get +string-encoders+ (m/dispatch-name schema)))

(defn json-transformer [schema]
  (get +json-decoders+ (m/dispatch-name schema)))
