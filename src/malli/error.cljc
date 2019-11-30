(ns malli.error
  (:require [malli.core :as m]))

(def default-errors
  {::unknown {:error/message {:en "unknown error"}}
   ::m/missing-key {:error/message {:en "missing required key"}}
   ::m/invalid-type {:error/message {:en "invalid type"}}
   'any? {:error/message {:en "should be any"}}
   'some? {:error/message {:en "shoud be some"}}
   'number? {:error/message {:en "should be number"}}
   'integer? {:error/message {:en "should be integer"}}
   'int? {:error/message {:en "should be int"}}
   'pos-int? {:error/message {:en "should be positive int"}}
   'neg-int? {:error/message {:en "should be negative int"}}
   'nat-int? {:error/message {:en "should be non-negative int"}}
   'float? {:error/message {:en "should be float"}}
   'double? {:error/message {:en "should be double"}}
   'boolean? {:error/message {:en "should be boolean"}}
   'string? {:error/message {:en "should be string"}}
   'ident? {:error/message {:en "should be ident"}}
   'simple-ident? {:error/message {:en "should be simple ident"}}
   'qualified-ident? {:error/message {:en "should be qualified ident"}}
   'keyword? {:error/message {:en "should be keyword"}}
   'simple-keyword? {:error/message {:en "should be simple keyword"}}
   'qualified-keyword? {:error/message {:en "should be qualified keyword"}}
   'symbol? {:error/message {:en "should be symbol"}}
   'simple-symbol? {:error/message {:en "should be simple symbol"}}
   'qualified-symbol? {:error/message {:en "should be qualified symbol"}}
   'uuid? {:error/message {:en "should be uuid"}}
   'uri? {:error/message {:en "should be uri"}}
   #?@(:clj ['decimal? {:error/message {:en "should be decimal"}}])
   'inst? {:error/message {:en "should be inst"}}
   'seqable? {:error/message {:en "should be seqable"}}
   'indexed? {:error/message {:en "should be indexed"}}
   'map? {:error/message {:en "should be map"}}
   'vector? {:error/message {:en "should be vector"}}
   'list? {:error/message {:en "should be list"}}
   'seq? {:error/message {:en "should be seq"}}
   'char? {:error/message {:en "should be char"}}
   'set? {:error/message {:en "should be set"}}
   'nil? {:error/message {:en "should be nil"}}
   'false? {:error/message {:en "should be false"}}
   'true? {:error/message {:en "should be true"}}
   'zero? {:error/message {:en "should be zero"}}
   #?@(:clj ['rational? {:error/message {:en "should be rational"}}])
   'coll? {:error/message {:en "should be coll"}}
   'empty? {:error/message {:en "should be empty"}}
   'associative? {:error/message {:en "should be associative"}}
   'sequential? {:error/message {:en "should be sequential"}}
   #?@(:clj ['ratio? {:error/message {:en "should be ratio"}}])
   #?@(:clj ['bytes? {:error/message {:en "should be bytes"}}])})

(defn- -maybe-localized [x locale]
  (if (map? x) (get x locale) x))

(defn- -message [error x locale opts]
  (or (if-let [fn (-maybe-localized (:error/fn x) locale)] ((m/eval fn) error opts))
      (-maybe-localized (:error/message x) locale)))

(defn- -ensure [x k]
  (if (sequential? x)
    (let [size' (count x)]
      (if (> k size') (into (vec x) (repeat (- (inc k) size') nil)) x))
    x))

(defn- -get [x k]
  (if (set? x) (-> x vec (get k)) (get x k)))

(defn- -put [x k v]
  (if (set? x) (conj x v) (update x k (fn [e]
                                        (cond (string? e) [e v]
                                              (sequential? v) v
                                              (nil? e) v
                                              :else (conj e v))))))

(defn- -assoc-in [acc value [p & ps] error]
  (cond
    p (let [acc' (-ensure (or acc (empty value)) p)
            value' (if ps (-assoc-in (-get acc p) (-get value p) ps error) error)]
        (-put acc' p value'))
    (map? value) (recur acc value [:malli/error] error)
    acc acc
    :else error))

(defn- -path [{:keys [schema]}
              {:keys [locale default-locale]
               :or {default-locale :en}}]
  (let [properties (m/properties schema)]
    (or (-maybe-localized (:error/path properties) locale)
        (-maybe-localized (:error/path properties) default-locale))))

;;
;; public api
;;

(defn error-path
  ([error]
   (error-path error nil))
  ([error opts]
   (into (:in error) (-path error opts))))

(defn error-message
  ([error]
   (error-message error nil))
  ([{:keys [schema type] :as error}
    {:keys [errors locale default-locale]
     :or {errors default-errors
          default-locale :en} :as opts}]
   (or (-message error (m/properties schema) locale opts)
       (-message error (errors (m/name schema)) locale opts)
       (-message error (errors type) locale opts)
       (-message error (m/properties schema) default-locale opts)
       (-message error (errors (m/name schema)) default-locale opts)
       (-message error (errors type) default-locale opts)
       (-message error (errors ::unknown) locale opts)
       (-message error (errors ::unknown) default-locale opts))))

(defn with-error-message
  ([error]
   (with-error-message error nil))
  ([error opts]
   (assoc error :message (error-message error opts))))

(defn with-error-messages
  ([explanation]
   (with-error-messages explanation nil))
  ([explanation {f :wrap :or {f identity} :as opts}]
   (update explanation :errors (partial map #(f (with-error-message % opts))))))

(defn humanize
  ([explanation]
   (humanize explanation nil))
  ([{:keys [value errors]} {f :wrap :or {f identity} :as opts}]
   (if errors
     (if (coll? value)
       (reduce
         (fn [acc error]
           (-assoc-in acc value (error-path error opts) (f (with-error-message error opts))))
         nil errors)
       (f (with-error-message (first errors) opts))))))
