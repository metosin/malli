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

(defn -interceptor
  "Utility function to convert input into an interceptor. Works with functions,
  map and sequence of those."
  [?interceptor schema options]
  (cond

    (fn? ?interceptor)
    {:enter ?interceptor}

    (and (map? ?interceptor) (contains? ?interceptor :compile))
    (let [compiled (::compiled options 0)
          options (assoc options ::compiled (inc ^long compiled))]
      (when (>= ^long compiled ^long *max-compile-depth*)
        (m/-fail! ::too-deep-compilation {:this ?interceptor, :schema schema, :options options}))
      (if-let [interceptor (-interceptor ((:compile ?interceptor) schema options) schema options)]
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
      (keep #(-interceptor % schema options) ?interceptor))

    (nil? ?interceptor) nil

    (ifn? ?interceptor)
    {:enter ?interceptor}

    :else (m/-fail! ::invalid-transformer {:value ?interceptor})))

;;
;; from strings
;;

(defn -string->long [x]
  (if (string? x)
    (try
      #?(:clj  (Long/parseLong x)
         :cljs (let [x' (if (re-find #"\D" (subs x 1))
                          ##NaN
                          (js/parseInt x 10))]
                 (if (js/isNaN x') x x')))
      (catch #?(:clj Exception, :cljs js/Error) _ x))
    x))

(defn -string->double [x]
  (if (string? x)
    (try
      #?(:clj  (Double/parseDouble x)
         :cljs (let [x' (js/parseFloat x)]
                 (if (js/isNaN x') x x')))
      (catch #?(:clj Exception, :cljs js/Error) _ x))
    x))

(defn -number->double [x]
  (if (number? x) (double x) x))

(defn -string->keyword [x]
  (if (string? x)
    (keyword x)
    x))

(defn -string->boolean [x]
  (if (string? x)
    (cond
      (= "true" x) true
      (= "false" x) false
      :else x)
    x))

(defn -string->uuid [x]
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

(defn -string->date [x]
  (if (string? x)
    (try
      #?(:clj  (Date/from (Instant/from (.parse +string->date-format+ x)))
         :cljs (js/Date. (.getTime (goog.date.UtcDateTime.fromIsoString x))))
      (catch #?(:clj Exception, :cljs js/Error) _ x))
    x))

#?(:clj
   (defn -string->decimal [x]
     (if (string? x)
       (try
         (BigDecimal. ^String x)
         (catch Exception _ x))
       x)))

(defn -string->symbol [x]
  (if (string? x)
    (symbol x)
    x))

(defn -string->nil [x]
  (if (= "" x)
    nil
    x))

;;
;; misc
;;

(defn -any->string [x]
  (if-not (nil? x)
    (str x)))

(defn -any->any [x] x)

#?(:clj
   (def ^DateTimeFormatter +date->string-format+
     (-> (DateTimeFormatter/ofPattern "yyyy-MM-dd'T'HH:mm:ss.SSSXXX")
         (.withZone (ZoneId/of "UTC")))))

(defn -date->string [x]
  (if (inst? x)
    (try
      #?(:clj  (.format +date->string-format+ (Instant/ofEpochMilli (inst-ms x)))
         :cljs (.toISOString x))
      (catch #?(:clj Exception, :cljs js/Error) _ x))
    x))

(defn -transform-map-keys [f]
  #(cond->> % (map? %) (into {} (map (fn [[k v]] [(f k) v])))))

;;
;; sequential
;;

(defn -sequential->set [x]
  (cond
    (set? x) x
    (sequential? x) (set x)
    :else x))

(defn -sequential->vector [x]
  (cond
    (vector? x) x
    (sequential? x) (vec x)
    :else x))

(defn -sequential->seq [x]
  (cond
    (vector? x) (seq x)
    :else x))

;;
;; sequential or set
;;

(defn -sequential-or-set->vector [x]
  (cond
    (vector? x) x
    (set? x) (vec x)
    (sequential? x) (vec x)
    :else x))

(defn -sequential-or-set->seq [x]
  (cond
    (vector? x) (seq x)
    (set? x) (seq x)
    :else x))

;;
;; decoders
;;

(defn -json-decoders []
  {'ident? -string->keyword
   'simple-ident? -string->keyword
   'qualified-ident? -string->keyword

   'keyword? -string->keyword
   'simple-keyword? -string->keyword
   'qualified-keyword? -string->keyword

   'symbol? -string->symbol
   'simple-symbol? -string->symbol
   'qualified-symbol? -string->symbol

   'uuid? -string->uuid
   'double? -number->double
   'inst? -string->date

   :double -number->double
   :keyword -string->keyword
   :symbol -string->symbol
   :qualified-keyword -string->keyword
   :qualified-symbol -string->symbol
   :uuid -string->uuid

   :map-of (-transform-map-keys m/-keyword->string)
   :set -sequential->set
   :sequential -sequential->seq
   :list -sequential->seq})

(defn -json-encoders []
  {'keyword? m/-keyword->string
   'simple-keyword? m/-keyword->string
   'qualified-keyword? m/-keyword->string

   'symbol? -any->string
   'simple-symbol? -any->string
   'qualified-symbol? -any->string

   'uuid? -any->string

   :keyword m/-keyword->string
   :symbol -any->string
   :qualified-keyword m/-keyword->string
   :qualified-symbol -any->string
   :uuid -any->string
   ;:uri any->string
   ;:bigdec any->string

   'inst? -date->string
   #?@(:clj ['ratio? -number->double])})

(defn -string-decoders []
  (merge
    (-json-decoders)
    {'integer? -string->long
     'int? -string->long
     'pos-int? -string->long
     'neg-int? -string->long
     'nat-int? -string->long
     'zero? -string->long

     :int -string->long
     :double -string->double
     :boolean -string->boolean

     :> -string->long
     :>= -string->long
     :< -string->long
     :<= -string->long
     := -string->long
     :not= -string->long

     'number? -string->double
     'float? -string->double
     'double? -string->double
     #?@(:clj ['rational? -string->double])
     #?@(:clj ['decimal? -string->decimal])

     'boolean? -string->boolean
     'false? -string->boolean
     'true? -string->boolean

     :vector -sequential->vector}))

(defn -string-encoders []
  (merge
    (-json-encoders)
    {'integer? -any->string
     'int? -any->string
     'pos-int? -any->string
     'neg-int? -any->string
     'nat-int? -any->string
     'zero? -any->string

     :int -any->string
     :double -any->string
     ;:boolean -any->string

     :> -any->string
     :>= -any->string
     :< -any->string
     :<= -any->string
     := -any->string
     :not= -any->string

     'double -any->string}))

;;
;; transformers
;;

(defn transformer [& ?transformers]
  (let [->data (fn [ts default name key] {:transformers ts
                                          :default default
                                          :key (if name (keyword (str key "/" name)))})
        ->eval (fn [x] (if (map? x) (reduce-kv (fn [x k v] (assoc x k (m/eval v))) x x) (m/eval x)))
        ->chain (comp m/-transformer-chain m/-into-transformer)
        chain (->> ?transformers (keep identity) (mapcat #(if (map? %) [%] (->chain %))) (vec))
        chain' (->> chain (mapv #(let [name (some-> % :name name)]
                                   {:decode (->data (:decoders %) (:default-decoder %) name "decode")
                                    :encode (->data (:encoders %) (:default-encoder %) name "encode")})))]
    (if (seq chain)
      (reify
        m/Transformer
        (-transformer-chain [_] chain)
        (-value-transformer [_ schema method options]
          (reduce
            (fn [acc {{:keys [key default transformers]} method}]
              (if-let [?interceptor (or (some-> (get (m/properties schema) key) ->eval)
                                        (get transformers (m/type schema))
                                        default)]
                (let [interceptor (-interceptor ?interceptor schema options)]
                  (if (nil? acc) interceptor (-interceptor [acc interceptor] schema options)))
                acc)) nil chain'))))))

(defn json-transformer
  ([]
   (json-transformer nil))
  ([{::keys [json-vectors]}]
   (transformer
     {:name :json
      :decoders (cond-> (-json-decoders) json-vectors (assoc :vector -sequential->vector))
      :encoders (-json-encoders)})))

(defn string-transformer []
  (transformer
    {:name :string
     :decoders (-string-decoders)
     :encoders (-string-encoders)}))

(defn strip-extra-keys-transformer
  ([]
   (strip-extra-keys-transformer nil))
  ([{:keys [accept] :or {accept (comp (some-fn nil? true?) :closed m/properties)}}]
   (let [transform {:compile (fn [schema _]
                               (if (accept schema)
                                 (if-let [ks (some->> schema m/map-entries (map first) seq set)]
                                   (fn [x] (reduce (fn [acc k] (if-not (ks k) (dissoc acc k) acc)) x (keys x))))))}]
     (transformer
       {:decoders {:map transform}
        :encoders {:map transform}}))))

(defn key-transformer [{:keys [decode encode]}]
  (let [transform (fn [f stage] (if f {stage (-transform-map-keys f)}))]
    (transformer
      {:decoders {:map (transform decode :enter)}
       :encoders {:map (transform encode :leave)}})))

(defn default-value-transformer []
  (let [get-default (fn [schema] (some-> schema m/properties :default))
        set-default {:compile (fn [schema _]
                                (if-some [default (get-default schema)]
                                  (fn [x] (if (nil? x) default x))))}
        add-defaults {:compile (fn [schema _]
                                 (let [entries (m/map-entries schema)
                                       defaults (->> entries
                                                     (keep (fn [[k _ v]]
                                                             (if-some [default (get-default v)]
                                                               [k default])))
                                                     (into {}))]
                                   (if (seq defaults)
                                     (fn [x]
                                       (if (map? x)
                                         (reduce-kv
                                           (fn [acc k v]
                                             (if-not (contains? x k)
                                               (assoc acc k v)
                                               acc))
                                           x defaults)
                                         x)))))}]
    (transformer
      {:default-decoder set-default
       :default-encoder set-default}
      {:decoders {:map add-defaults}
       :encoders {:map add-defaults}})))

(defn collection-transformer []
  (let [coders {:vector -sequential-or-set->vector
                :list -sequential-or-set->seq
                :sequential -sequential-or-set->seq
                :set -sequential->set}]
    (transformer
      {:decoders coders
       :encoders coders})))
