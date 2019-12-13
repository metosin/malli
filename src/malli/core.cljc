(ns malli.core
  (:refer-clojure :exclude [-name eval name merge])
  (:require [sci.core :as sci])
  #?(:clj (:import (java.util.regex Pattern))))

;;
;; protocols and records
;;

(defprotocol IntoSchema
  (-into-schema [this properties children opts] "creates a new schema instance"))

(defprotocol Schema
  (-name [this] "returns name of the schema")
  (-validator [this] "returns a predicate function that checks if the schema is valid")
  (-explainer [this path] "returns a function of `x in acc -> maybe errors` to explain the errors for invalid values")
  (-transformer [this transformer method] "returns an interceptor map with :enter and :leave functions to transform the value for the given schema and method")
  (-accept [this visitor opts] "accepts the visitor to visit schema and it's children")
  (-properties [this] "returns original schema properties")
  (-form [this] "returns original form of the schema"))

(defprotocol Transformer
  (-transformer-chain [this] "returns transformer chain as a vector of maps with :name, :encoders, :decoders and :opts")
  (-value-transformer [this schema method] "returns an value transforming interceptor for the given schema and method"))

(defrecord SchemaError [path in schema value type message])

#?(:clj (defmethod print-method SchemaError [v ^java.io.Writer w]
          (.write w (str "#Error" (->> v (filter val) (into {}))))))

#?(:clj (defmethod print-method ::into-schema [v ^java.io.Writer w]
          (.write w (str "#IntoSchema{:class " v "}"))))

#?(:clj (defmethod print-method ::schema [v ^java.io.Writer w]
          (.write w (pr-str (-form v)))))

;;
;; impl
;;

(declare schema)
(declare properties)
(declare default-registry)
(declare map-key)

(defn keyword->string [x]
  (if (keyword? x)
    (if-let [nn (namespace x)]
      (str nn "/" (clojure.core/name x))
      (clojure.core/name x))
    x))

(defn eval [?code]
  (if (fn? ?code) ?code (sci/eval-string (str ?code) {:preset :termination-safe
                                                      :bindings {'m/properties properties}})))

(defn error
  ([path in schema value]
   (->SchemaError path in schema value nil nil))
  ([path in schema value type]
   (->SchemaError path in schema value type nil)))

(defn error? [x]
  (instance? SchemaError x))

(defn fail!
  ([type]
   (fail! type nil))
  ([type data]
   (throw (ex-info (str type) {:type type, :data data}))))

(defn create-form [name properties children]
  (cond
    (and (seq properties) (seq children)) (into [name properties] children)
    (seq properties) [name properties]
    (seq children) (into [name] children)
    :else name))

(defn- -leaf-schema [name ->validator-and-children]
  ^{:type ::into-schema}
  (reify IntoSchema
    (-into-schema [_ properties children opts]
      (let [[validator children] (->validator-and-children properties children opts)
            form (create-form name properties children)]
        ^{:type ::schema}
        (reify
          Schema
          (-name [_] name)
          (-validator [_] validator)
          (-explainer [this path]
            (fn [value in acc]
              (if-not (validator value) (conj acc (error path in this value)) acc)))
          (-transformer [this transformer method]
            (-value-transformer transformer this method))
          (-accept [this visitor opts] (visitor this (vec children) opts))
          (-properties [_] properties)
          (-form [_] form))))))

(defn fn-schema [name f]
  (-leaf-schema
    name
    (fn [properties children _]
      (when (seq children)
        (fail! ::child-error {:name name, :properties properties, :children children, :min 0, :max 0}))
      [f children])))

(defn- -partial-fn-schema [name f]
  (-leaf-schema
    name
    (fn [properties [child :as children] _]
      (when-not (= 1 (count children))
        (fail! ::child-error {:name name, :properties properties, :children children, :min 1, :max 1}))
      [#(try (f % child) (catch #?(:clj Exception, :cljs js/Error) _ false)) children])))

(defn- -composite-schema [name f short-circuit]
  ^{:type ::into-schema}
  (reify IntoSchema
    (-into-schema [_ properties children opts]
      (when-not (seq children)
        (fail! ::no-children {:name name, :properties properties}))
      (let [child-schemas (mapv #(schema % opts) children)
            validators (distinct (map -validator child-schemas))
            validator (apply f validators)]
        ^{:type ::schema}
        (reify Schema
          (-name [_] name)
          (-validator [_] validator)
          (-explainer [_ path]
            (let [distance (if (seq properties) 2 1)
                  explainers (mapv (fn [[i c]] (-explainer c (into path [(+ i distance)]))) (map-indexed vector child-schemas))]
              (fn explain [x in acc]
                (reduce
                  (fn [acc' explainer]
                    (let [acc'' (explainer x in acc')]
                      (cond
                        (and short-circuit (identical? acc' acc'')) (reduced acc)
                        (nil? acc'') acc'
                        :else acc'')))
                  acc explainers))))
          (-transformer [this transformer method]
            (let [value-transformer  (-value-transformer transformer this method)
                  child-transformers (map #(-transformer % transformer method) child-schemas)
                  build-transformer
                  (fn [phase]
                    (let [st  (phase value-transformer)
                          ?st (or st identity)
                          tvs (into [] (keep phase) child-transformers)]
                      (cond
                        (not (seq tvs)) st
                        short-circuit (fn [x]
                                        (let [x (?st x)]
                                          (reduce-kv
                                          (fn [_ _ t] (let [x' (t x)] (if-not (identical? x' x) (reduced x') x)))
                                          x tvs)))
                        :else (fn [x] (reduce-kv (fn [x' _ t] (t x')) (?st x) tvs)))))]
              {:enter (build-transformer :enter)
               :leave (build-transformer :leave)}))
          (-accept [this visitor opts]
            (visitor this (mapv #(-accept % visitor opts) child-schemas) opts))
          (-properties [_] properties)
          (-form [_] (create-form name properties (map -form child-schemas))))))))

(defn- -properties-and-children [xs]
  (if (map? (first xs))
    [(first xs) (rest xs)]
    [nil xs]))

(defn- -optional-entry? [[_ ?p]]
  (boolean (and (map? ?p) (true? (:optional ?p)))))

(defn- -optional-entry [[k ?p s :as entry] optional]
  (if (map? ?p)
    (cond
      optional (update entry 1 assoc :optional true)
      (= [:optional] (keys ?p)) [k s]
      :else (update entry 1 dissoc :optional))
    (if optional [k {:optional true} ?p] [k ?p])))

(defn- -expand-key [[k ?p ?v] opts f]
  (let [[p v] (if (map? ?p) [?p ?v] [nil ?p])]
    [k p (f (schema v opts))]))

(defn -parse-keys [children opts]
  (let [entries (mapv #(-expand-key % opts identity) children)]
    {:required (->> entries (filter (comp not :optional second)) (mapv first))
     :optional (->> entries (filter (comp :optional second)) (mapv first))
     :keys (->> entries (mapv first))
     :entries entries
     :forms (mapv (fn [[k p v]]
                    (let [v' (-form v)]
                      (if p [k p v'] [k v']))) entries)}))

(defn- -map-schema []
  ^{:type ::into-schema}
  (reify IntoSchema
    (-into-schema [_ properties children opts]
      (let [{:keys [entries forms]} (-parse-keys children opts)
            form (create-form :map properties forms)]
        ^{:type ::schema}
        (reify Schema
          (-name [_] :map)
          (-validator [_]
            (let [validators (mapv
                               (fn [[key {:keys [optional]} value]]
                                 (let [valid? (-validator value)
                                       default (boolean optional)]
                                   (fn [m] (if-let [map-entry (find m key)] (valid? (val map-entry)) default))))
                               entries)
                  validate (fn [m]
                             (boolean
                               #?(:clj  (let [it (.iterator ^Iterable validators)]
                                          (boolean
                                            (loop []
                                              (if (.hasNext it)
                                                (and ((.next it) m) (recur))
                                                true))))
                                  :cljs (reduce #(or (%2 m) (reduced false)) true validators))))]
              (fn [m] (and (map? m) (validate m)))))
          (-explainer [this path]
            (let [distance (if (seq properties) 2 1)
                  explainers (mapv
                               (fn [[i [key {:keys [optional] :as key-properties} schema]]]
                                 (let [key-distance (if (seq key-properties) 2 1)
                                       explainer (-explainer schema (into path [(+ i distance) key-distance]))
                                       key-path (into path [(+ i distance) 0])]
                                   (fn [x in acc]
                                     (if-let [e (find x key)]
                                       (explainer (val e) (conj in key) acc)
                                       (if-not optional
                                         (conj acc (error key-path (conj in key) this nil ::missing-key))
                                         acc)))))
                               (map-indexed vector entries))]
              (fn [x in acc]
                (if-not (map? x)
                  (conj acc (error path in this x ::invalid-type))
                  (reduce
                    (fn [acc explainer]
                      (explainer x in acc))
                    acc explainers)))))
          (-transformer [this transformer method]
            (let [key-transformers (-transformer (map-key) transformer method)
                  value-transformers (some->>
                                      entries
                                      (keep (fn [[k _ s]]
                                              (when-some [t (-transformer s transformer method)]
                                                [k t])))
                                      (into {}))
                  map-transformers (-value-transformer transformer this method)
                  build-transformer
                  (fn [phase]
                    (let [key-transformer (phase key-transformers)
                          value-transformers (->> value-transformers
                                                  (keep (fn extract-value-transformer-phase [[k t]]
                                                          (when-some [phase-t (phase t)]
                                                            [k phase-t])))
                                                  (into {}))
                          map-transformer (phase map-transformers)
                          apply-key-transformers (when key-transformer
                                                   #(reduce-kv
                                                     (fn reduce-key-transformers [m k v]
                                                       (let [new-k (key-transformer k)]
                                                         (-> m
                                                             (assoc new-k v)
                                                             (cond-> (not (identical? k new-k)) (dissoc k)))))
                                                     %
                                                     %))
                          apply-value-transformers (when (seq value-transformers)
                                                     #(reduce-kv
                                                       (fn reduce-value-transformers [m k t]
                                                         (if-let [entry (find m k)]
                                                           (assoc m k (t (val entry)))
                                                           m))
                                                       %
                                                       value-transformers))
                          transform-order (->> (case phase
                                                 :enter [map-transformer
                                                         apply-value-transformers
                                                         apply-key-transformers]
                                                 :leave [apply-key-transformers
                                                         apply-value-transformers
                                                         map-transformer])
                                               (remove nil?))

                          transform (if (empty? transform-order)
                                      nil
                                      (apply comp (reverse transform-order)))]
                      (when transform
                        (fn [x]
                          (if (map? x)
                            (transform x)
                            x)))))]
              {:enter (build-transformer :enter)
               :leave (build-transformer :leave)}))
          (-accept [this visitor opts]
            (visitor this (->> entries (map last) (mapv #(-accept % visitor opts))) opts))
          (-properties [_] properties)
          (-form [_] form))))))

(defn- -map-of-schema []
  ^{:type ::into-schema}
  (reify IntoSchema
    (-into-schema [_ properties children opts]
      (when-not (and (seq children) (= 2 (count children)))
        (fail! ::child-error {:name :vector, :properties properties, :children children, :min 2, :max 2}))
      (let [[key-schema value-schema :as schemas] (mapv #(schema % opts) children)
            key-valid? (-validator key-schema)
            value-valid? (-validator value-schema)
            validate (fn [m]
                       (reduce-kv
                         (fn [_ key value]
                           (or (and (key-valid? key) (value-valid? value)) (reduced false)))
                         true m))]
        ^{:type ::schema}
        (reify Schema
          (-name [_] :map-of)
          (-validator [_] (fn [m] (and (map? m) (validate m))))
          (-explainer [this path]
            (let [distance (if (seq properties) 2 1)
                  key-explainer (-explainer key-schema (conj path distance))
                  value-explainer (-explainer value-schema (conj path (inc distance)))]
              (fn explain [m in acc]
                (if-not (map? m)
                  (conj acc (error path in this m ::invalid-type))
                  (reduce-kv
                    (fn [acc key value]
                      (let [in (conj in key)]
                        (->> acc
                             (key-explainer key in)
                             (value-explainer value in))))
                    acc m)))))
          (-transformer [this transformer method]
            (let [transformers (-value-transformer transformer this method)
                  key-transformers (-transformer key-schema transformer method)
                  value-transformers (-transformer value-schema transformer method)
                  build-transformer
                  (fn [phase]
                    (let [tt (phase transformers)
                          key-transformer (when-let [t (phase key-transformers)]
                                            (fn [x] (t (keyword->string x))))
                          value-transformer (phase value-transformers)
                          kv-transform (cond
                                         (and key-transformer value-transformer)
                                         #(assoc %1 (key-transformer %2) (value-transformer %3))
                                         key-transformer
                                         #(assoc %1 (key-transformer %2) %3)
                                         value-transformer
                                         #(assoc %1 %2 (value-transformer %3)))
                          apply-kv-transform (when kv-transform
                                               #(reduce-kv kv-transform (empty %) %))

                          transform-order (->> (case phase
                                                 :enter [tt apply-kv-transform]
                                                 :leave [apply-kv-transform tt])
                                               (remove nil?))
                          transform (when (seq transform-order)
                                      (apply comp (reverse transform-order)))]
                      (when transform
                        (fn [x]
                          (if (map? x)
                            (transform x)
                            x)))))]
              {:enter (build-transformer :enter)
               :leave (build-transformer :leave)}))
          (-accept [this visitor opts]
            (visitor this (mapv #(-accept % visitor opts) schemas) opts))
          (-properties [_] properties)
          (-form [_] (create-form :map-of properties (mapv -form schemas))))))))

(defn- -collection-schema [name fpred fwrap fempty]
  ^{:type ::into-schema}
  (reify IntoSchema
    (-into-schema [_ {:keys [min max] :as properties} children opts]
      (when-not (= 1 (count children))
        (fail! ::child-error {:name name, :properties properties, :children children, :min 1, :max 1}))
      (let [schema (schema (first children) opts)
            form (create-form name properties [(-form schema)])
            fwrap (fn [x]
                    (if (coll? x)
                      (fwrap x)
                      x))
            validate-limits (cond
                              (not (or min max)) (constantly true)
                              (and min max) (fn [x] (let [size (count x)] (<= min size max)))
                              min (fn [x] (let [size (count x)] (<= min size)))
                              max (fn [x] (let [size (count x)] (<= size max))))]
        ^{:type ::schema}
        (reify Schema
          (-name [_] name)
          (-validator [_]
            (let [validator (-validator schema)]
              (fn [x] (and (fpred x)
                           (validate-limits x)
                           (reduce (fn [acc v] (if (validator v) acc (reduced false))) true x)))))
          (-explainer [this path]
            (let [distance (if (seq properties) 2 1)
                  explainer (-explainer schema (conj path distance))]
              (fn [x in acc]
                (cond
                  (not (fpred x)) (conj acc (error path in this x ::invalid-type))
                  (not (validate-limits x)) (conj acc (error path in this x ::limits))
                  :else (let [size (count x)]
                          (loop [acc acc, i 0, [x & xs] x]
                            (if (< i size)
                              (cond-> (or (explainer x (conj in i) acc) acc) xs (recur (inc i) xs))
                              acc)))))))
          (-transformer [this transformer method]
            (let [coll-transformers (-value-transformer transformer this method)
                  val-transformers (-transformer schema transformer method)
                  build-transformer
                  (fn [phase]
                    (let [coll-transform (or (phase coll-transformers)
                                             fwrap)
                          val-transform (when-some [val-transform (phase val-transformers)]
                                          (if fempty
                                            #(into fempty (map val-transform) %)
                                            #(map val-transform %)))

                          transform-order (->> (case phase
                                                 :enter [coll-transform val-transform]
                                                 :leave [val-transform coll-transform])
                                               (remove nil?))
                          transform (when (seq transform-order)
                                      (apply comp (reverse transform-order)))]
                      (when transform
                        (fn [x]
                          (if (coll? x)
                            (transform x)
                            x)))))]
              {:enter (build-transformer :enter)
               :leave (build-transformer :leave)}))
          (-accept [this visitor opts] (visitor this [(-accept schema visitor opts)] opts))
          (-properties [_] properties)
          (-form [_] form))))))

(defn- -tuple-schema []
  ^{:type ::into-schema}
  (reify IntoSchema
    (-into-schema [_ properties children opts]
      (let [schemas (mapv #(schema % opts) children)
            size (count schemas)
            form (create-form :tuple properties (map -form schemas))
            validators (into (array-map) (map-indexed vector (mapv -validator schemas)))]
        (when-not (seq children)
          (fail! ::child-error {:name :tuple, :properties properties, :children children, :min 1}))
        ^{:type ::schema}
        (reify Schema
          (-name [_] :tuple)
          (-validator [_]
            (fn [x] (and (vector? x)
                         (= (count x) size)
                         (reduce-kv
                           (fn [acc i validator]
                             (if (validator (nth x i)) acc (reduced false))) true validators))))
          (-explainer [this path]
            (let [distance (if (seq properties) 2 1)
                  explainers (mapv (fn [[i s]]
                                     (-explainer s (conj path (+ i distance))))
                                   (map-indexed vector schemas))]
              (fn [x in acc]
                (cond
                  (not (vector? x)) (conj acc (error path in this x ::invalid-type))
                  (not= (count x) size) (conj acc (error path in this x ::tuple-size))
                  :else (loop [acc acc, i 0, [x & xs] x, [e & es] explainers]
                          (cond-> (e x (conj in i) acc) xs (recur (inc i) xs es)))))))
          (-transformer [this transformer method]
            (let [tuple-transformers (-value-transformer transformer this method)
                  child-transformers (->> schemas
                                          (mapv #(-transformer % transformer method))
                                          (map-indexed vector)
                                          (into {}))
                  build-transformer
                  (fn [phase]
                    (let [tuple-transform (phase tuple-transformers)
                          child-transformers (->> child-transformers
                                                  (keep (fn [[k t]]
                                                          (when-some [t (phase t)]
                                                            [k t])))
                                                  (into {}))
                          transform-children #(if (vector? %)
                                                (reduce-kv (fn [acc i t]
                                                             (update acc i t))
                                                           %
                                                           child-transformers)
                                                %)
                          transform-tuple (when tuple-transform
                                            #(if (vector? %)
                                               (tuple-transform %)
                                               %))
                          transform-order (->> (case phase
                                                 :enter [transform-tuple transform-children]
                                                 :leave [transform-children transform-tuple])
                                               (remove nil?))
                          transform (when (seq transform-order)
                                      (apply comp (reverse transform-order)))]
                      transform))]
              {:enter (build-transformer :enter)
               :leave (build-transformer :leave)}))
          (-accept [this visitor opts] (visitor this (mapv #(-accept % visitor opts) schemas) opts))
          (-properties [_] properties)
          (-form [_] form))))))

(defn- -enum-schema []
  ^{:type ::into-schema}
  (reify IntoSchema
    (-into-schema [_ properties children _]
      (when-not (seq children)
        (fail! ::no-children {:name :enum, :properties properties}))
      (let [schema (set children)
            validator (fn [x] (contains? schema x))]
        ^{:type ::schema}
        (reify Schema
          (-name [_] :enum)
          (-validator [_] validator)
          (-explainer [this path]
            (fn explain [x in acc]
              (if-not (validator x) (conj acc (error path in this x)) acc)))
          ;; TODO: should we try to derive the type from values? e.g. [:enum 1 2] ~> int?
          (-transformer [this transformer method]
            (-value-transformer transformer this method))
          (-accept [this visitor opts] (visitor this (vec children) opts))
          (-properties [_] properties)
          (-form [_] (create-form :enum properties children)))))))

(defn- -re-schema [class?]
  ^{:type ::into-schema}
  (reify IntoSchema
    (-into-schema [_ properties [child :as children] _]
      (when-not (= 1 (count children))
        (fail! ::child-error {:name :re, :properties properties, :children children, :min 1, :max 1}))
      (let [re (re-pattern child)
            validator (fn [x] (try (boolean (re-find re x)) (catch #?(:clj Exception, :cljs js/Error) _ false)))
            form (if class? re (create-form :re properties children))]
        ^{:type ::schema}
        (reify Schema
          (-name [_] :re)
          (-validator [_] validator)
          (-explainer [this path]
            (fn explain [x in acc]
              (try
                (if-not (re-find re x)
                  (conj acc (error path in this x))
                  acc)
                (catch #?(:clj Exception, :cljs js/Error) e
                  (conj acc (error path in this x (:type (ex-data e))))))))
          (-transformer [this transformer method]
            (-value-transformer transformer this method))
          (-accept [this visitor opts] (visitor this [] opts))
          (-properties [_] properties)
          (-form [_] form))))))

(defn- -fn-schema []
  ^{:type ::into-schema}
  (reify IntoSchema
    (-into-schema [_ properties children _]
      (when-not (= 1 (count children))
        (fail! ::child-error {:name :fn, :properties properties, :children children, :min 1, :max 1}))
      (let [f (eval (first children))
            validator (fn [x] (try (f x) (catch #?(:clj Exception, :cljs js/Error) _ false)))]
        ^{:type ::schema}
        (reify Schema
          (-name [_] :fn)
          (-validator [_] validator)
          (-explainer [this path]
            (fn explain [x in acc]
              (try
                (if-not (f x)
                  (conj acc (error path in this x))
                  acc)
                (catch #?(:clj Exception, :cljs js/Error) e
                  (conj acc (error path in this x (:type (ex-data e))))))))
          (-transformer [this transformer method]
            (-value-transformer transformer this method))
          (-accept [this visitor opts] (visitor this [] opts))
          (-properties [_] properties)
          (-form [_] (create-form :fn properties children)))))))

(defn- -maybe-schema []
  ^{:type ::into-schema}
  (reify IntoSchema
    (-into-schema [_ properties children opts]
      (when-not (= 1 (count children))
        (fail! ::child-error {:name :vector, :properties properties, :children children, :min 1, :max 1}))
      (let [schema' (-> children first (schema opts))
            validator' (-validator schema')
            form (create-form :maybe properties [(-form schema')])]
        ^{:type ::schema}
        (reify Schema
          (-name [_] :maybe)
          (-validator [_] (fn [x] (or (nil? x) (validator' x))))
          (-explainer [this path]
            (fn explain [x in acc]
              (if-not (or (nil? x) (validator' x)) (conj acc (error path in this x)) acc)))
          (-transformer [this transformer method]
            (let [build-transformer
                  (fn [phase]
                    (let [tt (phase (-value-transformer transformer this method))
                          t (phase (-transformer schema' transformer method))]
                      (if (and tt t) (comp t tt) (or tt t))))]
              {:enter (build-transformer :enter)
               :leave (build-transformer :leave)}))
          (-accept [this visitor opts] (visitor this [(-accept schema' visitor opts)] opts))
          (-properties [_] properties)
          (-form [_] form))))))

(defn- -multi-schema []
  ^{:type ::into-schema}
  (reify IntoSchema
    (-into-schema [_ properties childs opts]
      (let [{:keys [entries forms]} (-parse-keys childs opts)
            dispatch (eval (:dispatch properties))
            dispatch-map (->> (for [[d _ s] entries] [d s]) (into {}))
            form (create-form :multi properties forms)]
        (when-not dispatch
          (fail! ::missing-property {:key :dispatch}))
        ^{:type ::schema}
        (reify Schema
          (-name [_] :multi)
          (-validator [_]
            (let [validators (reduce-kv (fn [acc k s] (assoc acc k (-validator s))) {} dispatch-map)]
              (fn [x]
                (if-let [validator (validators (dispatch x))]
                  (validator x)
                  false))))
          (-explainer [this path]
            (let [explainers (reduce-kv (fn [acc k s] (assoc acc k (-explainer s path))) {} dispatch-map)]
              (fn [x in acc]
                (if-let [explainer (explainers (dispatch x))]
                  (explainer x in acc)
                  (conj acc (error path in this x ::invalid-dispatch-value))))))
          (-transformer [this transformer method]
            (let [transformers (-value-transformer transformer this method)
                  child-transformers (reduce-kv
                                      #(assoc %1 %2 (-transformer %3 transformer method))
                                      {}
                                      dispatch-map)
                  build-transformer
                  (fn [phase]
                    (let [transform-val (phase transformers)
                          ts (->> child-transformers
                                  (keep (fn [[k v]] (when-some [t (phase v)]
                                                      [k t])))
                                  (into {}))
                          transform-child (when (seq ts)
                                            (fn [x] (if-let [t (ts (dispatch x))] (t x) x)))
                          transform-order (->>
                                           (case phase
                                             :enter [transform-val transform-child]
                                             :leave [transform-child transform-val])
                                           (remove nil?))

                          transform (when (seq transform-order)
                                      (apply comp (reverse transform-order)))]
                      transform))]
              {:enter (build-transformer :enter)
               :leave (build-transformer :leave)}))
          (-accept [this visitor opts]
            (visitor this (->> entries (map last) (mapv #(-accept % visitor opts))) opts))
          (-properties [_] properties)
          (-form [_] form))))))

(defn- -register [registry k schema]
  (if (contains? registry k)
    (fail! ::schema-already-registered {:key k, :registry registry}))
  (assoc registry k schema))

(defn- -register-var [registry v]
  (let [name (-> v meta :name)
        schema (fn-schema name @v)]
    (-> registry
        (-register name schema)
        (-register @v schema))))

;;
;; public api
;;

(defn schema? [x]
  (satisfies? Schema x))

(defn schema
  ([?schema]
   (schema ?schema nil))
  ([?schema {:keys [registry] :as opts :or {registry default-registry}}]
   (let [-get #(or (if (satisfies? IntoSchema %) %)
                   (get registry %)
                   (some-> registry (get (type %)) (-into-schema nil [%] opts)))]
     (cond
       (schema? ?schema) ?schema
       (satisfies? IntoSchema ?schema) (-into-schema ?schema nil nil opts)
       (vector? ?schema) (apply -into-schema (concat [(-get (first ?schema))]
                                                     (-properties-and-children (rest ?schema)) [opts]))
       :else (or (some-> ?schema -get (schema opts)) (fail! ::invalid-schema {:schema ?schema}))))))

(defn form
  ([?schema]
   (form ?schema nil))
  ([?schema opts]
   (-form (schema ?schema opts))))

(defn accept
  ([?schema visitor]
   (accept ?schema visitor nil))
  ([?schema visitor opts]
   (-accept (schema ?schema opts) visitor opts)))

(defn properties
  ([?schema]
   (properties ?schema nil))
  ([?schema opts]
   (-properties (schema ?schema opts))))

(defn children
  ([?schema]
   (children ?schema nil))
  ([?schema opts]
   (let [schema (schema ?schema opts)
         form (-form schema)]
     (if (vector? form)
       (->> form (drop (if (-properties schema) 2 1)))))))

(defn name
  ([?schema]
   (name ?schema nil))
  ([?schema opts]
   (-name (schema ?schema opts))))

(defn validator
  ([?schema]
   (validator ?schema nil))
  ([?schema opts]
   (-validator (schema ?schema opts))))

(defn validate
  ([?schema value]
   (validate ?schema value nil))
  ([?schema value opts]
   ((validator ?schema opts) value)))

(defn explainer
  ([?schema]
   (explainer ?schema nil))
  ([?schema opts]
   (let [schema' (schema ?schema opts)
         explainer' (-explainer schema' [])]
     (fn explainer
       ([value]
        (explainer value [] []))
       ([value in acc]
        (if-let [errors (seq (explainer' value in acc))]
          {:schema schema'
           :value value
           :errors errors}))))))

(defn explain
  ([?schema value]
   (explain ?schema value nil))
  ([?schema value opts]
   ((explainer ?schema opts) value [] [])))

(defn decoder
  "Creates a value decoding transformer given a transformer and a schema."
  ([?schema t]
   (decoder ?schema nil t))
  ([?schema opts t]
   (let [{:keys [enter leave]} (-transformer (schema ?schema opts) t :decode)]
     (cond
       (and enter leave) (comp leave enter)
       (or enter leave) (or enter leave)
       :else identity))))

(defn decode
  "Transforms a value with a given decoding transformer agains a schema."
  ([?schema value t]
   (decode ?schema value nil t))
  ([?schema value opts t]
   (if-let [transform (decoder ?schema opts t)]
     (transform value)
     value)))

(defn encoder
  "Creates a value encoding transformer given a transformer and a schema."
  ([?schema t]
   (encoder ?schema nil t))
  ([?schema opts t]
   (let [{:keys [enter leave]} (-transformer (schema ?schema opts) t :encode)]
     (cond
       (and enter leave) (comp leave enter)
       (or enter leave) (or enter leave)
       :else identity))))

(defn encode
  "Transforms a value with a given encoding transformer agains a schema."
  ([?schema value t]
   (encode ?schema value nil t))
  ([?schema value opts t]
   (if-let [transform (encoder ?schema opts t)]
     (transform value)
     value)))

(defn merge
  "Deep-merges two schemas into one with the following rules:

  * if either schemas is `nil`, the other one is used, regardless of value
  * with two :map schemas, both keys and values are merged
  * with any other schemas, 2-arity `:malli.core/merge` function is used, defaulting
    to constantly schema2"
  ([?schema1 ?schema2]
   (merge ?schema1 ?schema2 nil))
  ([?schema1 ?schema2 opts]
   (let [[schema1 schema2 :as schemas] [(if ?schema1 (schema ?schema1 opts))
                                        (if ?schema2 (schema ?schema2 opts))]
         merge' (::merge opts (constantly schema2))]
     (cond
       (not schema1) schema2
       (not schema2) schema1
       (not= :map (name schema1) (name schema2)) (merge' schema1 schema2)
       :else (let [p (clojure.core/merge (properties schema1) (properties schema2))]
               (-> [:map]
                   (cond-> p (conj p))
                   (into (:form
                           (reduce
                             (fn [{:keys [keys] :as acc} [k :as f]]
                               (if (keys k)
                                 (->> (reduce
                                        (fn [acc' [k' :as f']]
                                          (if-not (= k k')
                                            (conj acc' f')
                                            (let [f'' (-optional-entry
                                                        (if (= k k') f f')
                                                        (and (-optional-entry? f) (-optional-entry? f')))]
                                              (conj acc' (conj (pop f'') (merge (peek f') (peek f) opts))))))
                                        [] (:form acc))
                                      (assoc acc :form))
                                 (-> acc
                                     (update :form conj f)
                                     (update :keys conj k))))
                             {:keys #{}, :form []}
                             (mapcat #(-> % (children opts) (-parse-keys opts) :forms) schemas))))
                   (schema opts)))))))

(defn map-key []
  ^{:type ::schema}
  (reify Schema
    (-name [_] ::map-key)
    (-form [_] ::map-key)
    (-properties [_])
    (-transformer [this transformer method] (-value-transformer transformer this method))))

;;
;; registries
;;

(def predicate-registry
  (->> [#'any? #'some? #'number? #'integer? #'int? #'pos-int? #'neg-int? #'nat-int? #'float? #'double?
        #'boolean? #'string? #'ident? #'simple-ident? #'qualified-ident? #'keyword? #'simple-keyword?
        #'qualified-keyword? #'symbol? #'simple-symbol? #'qualified-symbol? #'uuid? #'uri? #?(:clj #'decimal?)
        #'inst? #'seqable? #'indexed? #'map? #'vector? #'list? #'seq? #'char? #'set? #'nil? #'false? #'true?
        #'zero? #?(:clj #'rational?) #'coll? #'empty? #'associative? #'sequential? #?(:clj #'ratio?) #?(:clj #'bytes?)]
       (reduce -register-var {})))

(def class-registry
  {#?(:clj Pattern, :cljs js/RegExp) (-re-schema true)})

(def comparator-registry
  (->> {:> >, :>= >=, :< <, :<= <=, := =, :not= not=}
       (map (fn [[k v]] [k (-partial-fn-schema k v)]))
       (into {})
       (reduce-kv -register nil)))

(def base-registry
  {:and (-composite-schema :and every-pred false)
   :or (-composite-schema :or some-fn true)
   :map (-map-schema)
   :map-of (-map-of-schema)
   :vector (-collection-schema :vector vector? vec [])
   :list (-collection-schema :list list? seq nil)
   :sequential (-collection-schema :sequential sequential? seq nil)
   :set (-collection-schema :set set? set #{})
   :enum (-enum-schema)
   :maybe (-maybe-schema)
   :tuple (-tuple-schema)
   :multi (-multi-schema)
   :re (-re-schema false)
   :fn (-fn-schema)})

(def default-registry
  (clojure.core/merge predicate-registry class-registry comparator-registry base-registry))
