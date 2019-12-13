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

(def ^:dynamic *max-compile-depth* 10)

(defn- ->interceptor
  "Utility function to convert input into an interceptor. Works with functions,
  map and sequence of those."
  [?interceptor schema opts]
  (cond

    (fn? ?interceptor)
    {:enter ?interceptor}

    (and (map? ?interceptor) (contains? ?interceptor :compile))
    (let [compiled (::compiled opts 0)
          opts (assoc opts ::compiled (inc ^long compiled))]
      (when (>= ^long compiled ^long *max-compile-depth*)
        (m/fail! ::too-deep-compilation {:this ?interceptor, :schema schema, :opts opts}))
      (if-let [interceptor (->interceptor ((:compile ?interceptor) schema opts) schema opts)]
        (merge
          (dissoc ?interceptor :compile)
          interceptor)))

    (and (map? ?interceptor)
         (or (contains? ?interceptor :enter)
             (contains? ?interceptor :leave))) ?interceptor

    (coll? ?interceptor)
    (reduce
      (fn [{:keys [enter leave]} {new-enter :enter new-leave :leave}]
        (let [enter (if (and enter new-enter)
                      (comp new-enter enter)
                      (or enter new-enter))
              leave (if (and leave new-leave)
                      (comp new-leave leave)
                      (or leave new-leave))]
          {:enter enter :leave leave}))
      (keep #(->interceptor % schema opts) ?interceptor))

    (nil? ?interceptor) nil

    :else (m/fail! ::invalid-transformer {:value ?interceptor})))

(defn transformer [& ?options]
  (let [->data (fn [ts name key] (cond-> {:transformers ts} name (assoc :key (keyword (str key "/" name)))))
        ->eval (fn [x] (if (map? x) (reduce-kv (fn [x k v] (assoc x k (m/eval v))) x x) (m/eval x)))
        chain (->> ?options (mapcat #(if (satisfies? m/Transformer %) (m/-transformer-chain %) [%])) (vec))
        chain' (->> chain (mapv #(let [name (some-> % :name name)]
                                   {:decode (->data (:decoders %) name "decode")
                                    :encode (->data (:encoders %) name "encode")})))
        opts (->> chain (map :opts) (apply merge))] ;; TODO: remove this
    (reify
      m/Transformer
      (-transformer-chain [_] chain)
      (-value-transformer [_ schema method]
        (reduce
          (fn [acc {{:keys [key transformers]} method}]
            (if-let [?interceptor (or (some-> (get (m/properties schema) key) ->eval)
                                      (get transformers (m/name schema)))]
              (let [interceptor (->interceptor ?interceptor schema opts)]
                (if (nil? acc) interceptor (->interceptor [acc interceptor] schema opts)))
              acc)) nil chain')))))

;;
;; From Strings
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

(def +json-encoders+
  {'keyword? m/keyword->string
   'simple-keyword? m/keyword->string
   'qualified-keyword? m/keyword->string

   'symbol? any->string
   'simple-symbol? any->string
   'qualified-symbol? any->string

   'uuid? any->string

   ;:uri any->string
   ;:bigdec any->string

   'inst? date->string
   #?@(:clj ['ratio? number->double])})

(def +string-decoders+
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

(def +string-encoders+
  (merge
    +json-encoders+
    {'integer? any->string
     'int? any->string
     'pos-int? any->string
     'neg-int? any->string
     'nat-int? any->string
     'zero? any->string

     :> any->string
     :>= any->string
     :< any->string
     :<= any->string
     := any->string
     :not= any->string

     'double any->string}))

(def +strip-extra-keys-transformers+
  {:map {:compile (fn [schema _]
                    (if-let [keys (seq (:keys (m/-parse-keys (m/children schema) nil)))]
                      (fn [x] (select-keys x keys))))}})

(defn +key-transformers+ [key-fn]
  (if key-fn {::m/map-key (fn [x] (key-fn x))}))

;;
;; transformers
;;

(def json-transformer
  (transformer
    {:name :json
     :decoders +json-decoders+
     :encoders +string-encoders+}))

(def string-transformer
  (transformer
    {:name :string
     :decoders +string-decoders+
     :encoders +string-encoders+}))

(def strip-extra-keys-transformer
  (transformer
    {:name ::strip-extra-keys
     :decoders +strip-extra-keys-transformers+
     :encoders +strip-extra-keys-transformers+}))

(defn key-transformer
  ([decode-key-fn]
   (key-transformer decode-key-fn nil))
  ([decode-key-fn encode-key-fn]
   (transformer
     {:name ::key-transformer
      :decoders (+key-transformers+ decode-key-fn)
      :encoders (+key-transformers+ encode-key-fn)})))

(def collection-transformer
  (transformer
    {:name ::collection}))
