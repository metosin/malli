(ns malli.transform
  #?(:cljs (:refer-clojure :exclude [Inst Keyword UUID]))
  (:require [malli.core :as m]
            #?(:cljs [goog.date.UtcDateTime])
            #?(:cljs [goog.date.Date]))
  #?(:clj (:import (java.time Instant ZoneId)
                   (java.time.format DateTimeFormatter DateTimeFormatterBuilder)
                   (java.time.temporal ChronoField)
                   (java.net URI)
                   (java.util Date UUID))))

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
      (when-let [interceptor (-interceptor ((:compile ?interceptor) schema options) schema options)]
        (merge
         (dissoc ?interceptor :compile)
         interceptor)))

    (and (map? ?interceptor)
         (or (contains? ?interceptor :enter)
             (contains? ?interceptor :leave))) ?interceptor

    (coll? ?interceptor)
    (reduce
     (fn [{:keys [enter leave]} {new-enter :enter new-leave :leave}]
       (let [enter (if (and enter new-enter) #(new-enter (enter %)) (or enter new-enter))
             leave (if (and leave new-leave) #(leave (new-leave %)) (or leave new-leave))]
         {:enter enter :leave leave}))
     (keep #(-interceptor % schema options) ?interceptor))

    (nil? ?interceptor) nil

    (ifn? ?interceptor)
    {:enter ?interceptor}

    :else (m/-fail! ::invalid-transformer {:value ?interceptor})))

(defn -safe [f] #(try (f %) (catch #?(:clj Exception, :cljs js/Error) _ %)))

;;
;; from strings
;;

(defn -string->long [x]
  (if (string? x)
    (try #?(:clj  (Long/parseLong x)
            :cljs (let [x' (if (re-find #"\D" (subs x 1)) ##NaN (js/parseInt x 10))]
                    (cond
                      (js/isNaN x') x
                      (> x' js/Number.MAX_SAFE_INTEGER) x
                      (< x' js/Number.MIN_SAFE_INTEGER) x
                      :else x')))
         (catch #?(:clj Exception, :cljs js/Error) _ x))
    x))

(defn -string->double [x]
  (if (string? x)
    (or (parse-double x) x)
    x))

(defn -number->double [x]
  (if (number? x) (double x) x))

(defn -string->keyword [x]
  (if (string? x) (keyword x) x))

(defn -string->boolean [x]
  (if (string? x)
    (cond (= "true" x) true
          (= "false" x) false
          :else x)
    x))

(def ^:private uuid-re
  #"(?i)^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$")

(defn -string->uuid [x]
  (if (string? x)
    (if-let [x (re-matches uuid-re x)]
      #?(:clj  (UUID/fromString x)
         :cljs (uuid x))
      x)
    x))

#?(:clj
   (defn -string->uri [x]
     (if (string? x)
       (try
         (URI. x)
         ;; TODO replace with URISyntaxException once we are on
         ;; babashka >= v1.3.186.
         (catch Exception _
           x))
       x)))

#?(:clj
   (def ^DateTimeFormatter +string->date-format+
     (-> (DateTimeFormatterBuilder.)
         (.appendPattern "yyyy-MM-dd['T'HH:mm:ss]")
         (.optionalStart)
         (.appendFraction ChronoField/MICRO_OF_SECOND, 0, 9, true)
         (.optionalEnd)
         (.optionalStart)
         (.appendOffset "+HHMMss", "Z")
         (.optionalEnd)
         (.optionalStart)
         (.appendOffset "+HH:MM:ss", "Z")
         (.optionalEnd)
         (.parseDefaulting ChronoField/HOUR_OF_DAY 0)
         (.parseDefaulting ChronoField/OFFSET_SECONDS 0)
         (.toFormatter))))

(defn -string->date [x]
  (if (string? x)
    (try #?(:clj  (Date/from (Instant/from (.parse +string->date-format+ x)))
            :cljs (js/Date. (.getTime (goog.date.UtcDateTime/fromIsoString x))))
         (catch #?(:clj Exception, :cljs js/Error) _ x))
    x))

#?(:clj
   (defn -string->decimal [x]
     (if (string? x)
       (try (BigDecimal. ^String x)
            (catch Exception _ x))
       x)))

(defn -string->symbol [x]
  (if (string? x) (symbol x) x))

(defn -string->nil [x]
  (if (= "" x) nil x))

;;
;; misc
;;

(defn -any->string [x]
  (when-not (nil? x) (str x)))

(defn -any->any [x] x)

#?(:clj
   (def ^DateTimeFormatter +date->string-format+
     (-> (DateTimeFormatter/ofPattern "yyyy-MM-dd'T'HH:mm:ss.SSSXXX")
         (.withZone (ZoneId/of "UTC")))))

(defn -date->string [x]
  (if (inst? x)
    (try #?(:clj  (.format +date->string-format+ (Instant/ofEpochMilli (inst-ms x)))
            :cljs (.toISOString x))
         (catch #?(:clj Exception, :cljs js/Error) _ x))
    x))

(defn -transform-map-keys [f]
  #(cond->> % (map? %) (into {} (map (fn [[k v]] [(f k) v])))))

(defn -transform-if-valid [f schema]
  (let [validator (m/-validator schema)]
    (fn [x] (let [out (f x)] (if (validator out) out x)))))

;;
;; sequential
;;

(defn -sequential->set [x]
  (cond (set? x) x
        (sequential? x) (set x)
        :else x))

(defn -sequential->vector [x]
  (cond (vector? x) x
        (sequential? x) (vec x)
        :else x))

;;
;; sequential or set
;;

(defn -sequential-or-set->vector [x]
  (cond (vector? x) x
        (set? x) (vec x)
        (sequential? x) (vec x)
        :else x))

(defn -sequential-or-set->seq [x]
  (cond (vector? x) (seq x)
        (set? x) (seq x)
        :else x))

(defn -infer-child-compiler [method]
  (fn [schema _]
    (some-> schema
            (m/children)
            (m/-infer)
            {:keyword {:decode -string->keyword
                       :encode m/-keyword->string}
             :symbol {:decode -string->symbol}
             :int {:decode -string->long}
             :double {:decode -string->double}}
            (method -any->string))))

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
   #?@(:clj ['uri? -string->uri])

   :enum {:compile (-infer-child-compiler :decode)}
   := {:compile (-infer-child-compiler :decode)}

   :double -number->double
   :keyword -string->keyword
   :symbol -string->symbol
   :qualified-keyword -string->keyword
   :qualified-symbol -string->symbol
   :uuid -string->uuid
   ;#?@(:clj [:uri -string->uri])

   :set -sequential->set})

(defn -json-encoders []
  {'keyword? m/-keyword->string
   'simple-keyword? m/-keyword->string
   'qualified-keyword? m/-keyword->string

   'symbol? -any->string
   'simple-symbol? -any->string
   'qualified-symbol? -any->string

   'uuid? -any->string
   #?@(:clj ['uri? -any->string])

   :enum {:compile (-infer-child-compiler :encode)}
   := {:compile (-infer-child-compiler :encode)}

   :keyword m/-keyword->string
   :symbol -any->string
   :qualified-keyword m/-keyword->string
   :qualified-symbol -any->string
   :uuid -any->string
   ;#?@(:clj [:uri -any->string])
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
    :not= -string->long

    'number? -string->double
    'float? -string->double
    'double? -string->double
    #?@(:clj ['rational? -string->double])
    #?@(:clj ['decimal? -string->decimal])

    'boolean? -string->boolean
    'false? -string->boolean
    'true? -string->boolean

    :map-of (-transform-map-keys m/-keyword->string)
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
    :not= -any->string

    'double -any->string}))

;;
;; transformers
;;

(defn transformer [& ?transformers]
  (let [->data (fn [ts default name key] {:transformers ts
                                          :default default
                                          :keys (when name
                                                  (cond-> [[(keyword key) name]]
                                                    (not (qualified-keyword? name))
                                                    (conj [(keyword key (clojure.core/name name))])))})
        ->eval (fn [x options] (if (map? x) (reduce-kv (fn [x k v] (assoc x k (m/eval v options))) x x) (m/eval x)))
        ->chain (m/-comp m/-transformer-chain m/-into-transformer)
        chain (->> ?transformers (keep identity) (mapcat #(if (map? %) [%] (->chain %))) (vec))
        chain' (->> chain (mapv #(let [name (:name %)]
                                   {:decode (->data (:decoders %) (:default-decoder %) name "decode")
                                    :encode (->data (:encoders %) (:default-encoder %) name "encode")})))]
    (when (seq chain)
      (reify
        m/Transformer
        (-transformer-chain [_] chain)
        (-value-transformer [_ schema method options]
          (reduce
           (fn [acc {{:keys [keys default transformers]} method}]
             (let [options (or options (m/options schema))
                   from (fn [f] #(some-> (get-in (f schema) %) (->eval options)))
                   from-properties (some-fn (from m/properties) (from m/type-properties))]
               (if-let [?interceptor (or (some from-properties keys) (get transformers (m/type schema)) default)]
                 (let [interceptor (-interceptor ?interceptor schema options)]
                   (if (nil? acc) interceptor (-interceptor [acc interceptor] schema options)))
                 acc))) nil chain'))))))

(defn json-transformer
  ([] (json-transformer nil))
  ([{::keys [json-vectors map-of-key-decoders] :or {map-of-key-decoders (-string-decoders)}}]
   (transformer
    {:name :json
     :decoders (-> (-json-decoders)
                   (assoc :map-of {:compile (fn [schema _]
                                              (let [key-schema (some-> schema (m/children) (first))]
                                                (or (some-> key-schema (m/type) map-of-key-decoders
                                                            (-interceptor schema {}) (m/-intercepting)
                                                            (m/-comp m/-keyword->string)
                                                            (-transform-if-valid key-schema)
                                                            (-transform-map-keys))
                                                    (-transform-map-keys m/-keyword->string))))})
                   (cond-> json-vectors (assoc :vector -sequential->vector)))
     :encoders (-json-encoders)})))

(defn string-transformer []
  (transformer
   {:name :string
    :decoders (-string-decoders)
    :encoders (-string-encoders)}))

(defn strip-extra-keys-transformer
  ([] (strip-extra-keys-transformer nil))
  ([{:keys [accept] :or {accept (m/-comp #(or (nil? %) (true? %)) :closed m/properties)}}]
   (let [strip-map {:compile (fn [schema _]
                               (let [default-schema (m/default-schema schema)
                                     ks (some->> schema (m/explicit-keys) (set))]
                                 (cond-> nil
                                   (accept schema)
                                   (assoc :enter (fn [x]
                                                   (if (and (map? x) (not default-schema))
                                                     (reduce-kv (fn [acc k _] (if-not (ks k) (dissoc acc k) acc)) x x)
                                                     x))))))}
         strip-map-of (fn [stage]
                        {:compile (fn [schema options]
                                    (let [entry-schema (m/into-schema :tuple nil (m/children schema) options)
                                          valid?       (m/validator entry-schema options)]
                                      {stage (fn [x]
                                              (reduce (fn [acc entry]
                                                              (if (valid? entry)
                                                                (apply assoc acc entry)
                                                                acc)) (empty x) x))}))})]
     (transformer
       {:decoders {:map strip-map, :map-of (strip-map-of :leave)}
        :encoders {:map strip-map, :map-of (strip-map-of :enter)}}))))

(defn key-transformer [{:keys [decode encode types] :or {types #{:map}}}]
  (let [transform (fn [f stage] (when f {stage (-transform-map-keys f)}))]
    (transformer (cond (set? types) {:decoders (zipmap types (repeat (transform decode :enter)))
                                     :encoders (zipmap types (repeat (transform encode :leave)))}
                       (= :default types) {:default-decoder (transform decode :enter)
                                           :default-encoder (transform encode :leave)}))))

(defn default-value-transformer
  ([] (default-value-transformer nil))
  ([{:keys [key default-fn defaults ::add-optional-keys] :or {key :default, default-fn (fn [_ x] x)}}]
   (let [get-default (fn [schema]
                       (or (some-> schema m/properties :default/fn m/eval)
                           (if-some [e (some-> schema m/properties (find key))]
                             (constantly (val e))
                             (some->> schema m/type (get defaults) (#(constantly (% schema)))))))
         set-default {:compile (fn [schema _]
                                 (when-some [f (get-default schema)]
                                   (fn [x] (if (nil? x) (default-fn schema (f)) x))))}
         add-defaults {:compile (fn [schema _]
                                  (let [defaults (into {}
                                                       (keep (fn [[k {:keys [optional] :as p} v]]
                                                               (when (or (not optional) add-optional-keys)
                                                                 (let [e (find p key)]
                                                                   (when-some [f (if e (constantly (val e))
                                                                                       (get-default v))]
                                                                     [k (fn [] (default-fn schema (f)))])))))
                                                       (m/children schema))]
                                    (when (seq defaults)
                                      (fn [x]
                                        (if (map? x)
                                          (reduce-kv
                                           (fn [acc k f]
                                             (if-not (contains? x k)
                                               (assoc acc k (f))
                                               acc))
                                           x defaults)
                                          x)))))}]
     (transformer
      {:default-decoder set-default
       :default-encoder set-default}
      {:decoders {:map add-defaults}
       :encoders {:map add-defaults}}))))

(defn collection-transformer []
  (let [coders {:vector -sequential-or-set->vector
                :sequential -sequential-or-set->seq
                :set -sequential->set
                :tuple -sequential->vector}]
    (transformer
     {:decoders coders
      :encoders coders})))
