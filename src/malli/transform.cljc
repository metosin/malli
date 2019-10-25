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

(defn transformer [& ?options]
  (let [options (map #(if (satisfies? m/Transformer %) (m/-transformer-options %) %) ?options)
        name (-> options last :name)
        decoders (->> options (map :decoders) (apply merge))
        encoders (->> options (map :encoders) (apply merge))]
    (reify
      m/Transformer
      (-transformer-name [_] name)
      (-transformer-options [_] {:name name, :decoders decoders, :encoders encoders})
      (-value-transformer [_ schema context]
        (if-let [->transformer (get ({:encode encoders, :decode decoders} context) (m/name schema))]
          (->transformer schema))))))

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
;; decoders
;;

(def +json-decoders+
  {'ident? (constantly string->keyword)
   'simple-ident? (constantly string->keyword)
   'qualified-ident? (constantly string->keyword)

   'keyword? (constantly string->keyword)
   'simple-keyword? (constantly string->keyword)
   'qualified-keyword? (constantly string->keyword)

   'symbol? (constantly string->symbol)
   'simple-symbol? (constantly string->symbol)
   'qualified-symbol? (constantly string->symbol)

   'uuid? (constantly string->uuid)

   'inst? (constantly string->date)})

(def +string-decoders+
  (merge
    +json-decoders+
    {'integer? (constantly string->long)
     'int? (constantly string->long)
     'pos-int? (constantly string->long)
     'neg-int? (constantly string->long)
     'nat-int? (constantly string->long)
     'zero? (constantly string->long)

     :> (constantly string->long)
     :>= (constantly string->long)
     :< (constantly string->long)
     :<= (constantly string->long)
     := (constantly string->long)
     :not= (constantly string->long)

     'number? (constantly string->double)
     'float? (constantly string->double)
     'double? (constantly string->double)
     #?@(:clj ['rational? (constantly string->double)])

     'boolean? (constantly string->boolean)
     'false? (constantly string->boolean)
     'true? (constantly string->boolean)}))

(def +strip-extra-keys-decoders+
  {:map (fn [schema]
          (if-let [keys (seq (:keys (m/-parse-keys (m/children schema) nil)))]
            (fn [x] (select-keys x keys))))})

(defn +key-decoders+ [key-fn]
  {::m/map-key (constantly (fn [x] (key-fn x)))})

;;
;; transformers
;;

(def string-transformer
  (transformer
    {:name :string
     :decoders +string-decoders+}))

(def json-transformer
  (transformer
    {:name :json
     :decoders +json-decoders+}))

(def strip-extra-keys-transformer
  (transformer
    {:name ::strip-extra-keys
     :decoders +strip-extra-keys-decoders+}))

(defn key-transformer [key-fn]
  (transformer
    {:name ::key-transformer
     :decoders (+key-decoders+ key-fn)}))

(def collection-transformer
  (transformer
    {:name ::collection}))
