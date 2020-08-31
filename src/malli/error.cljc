(ns malli.error
  (:require [malli.core :as m]
            [clojure.string :as str]))

(defprotocol SchemaError
  (-error [this] "error data structure for the Schema"))

(extend-protocol SchemaError
  #?(:clj Object, :cljs default)
  (-error [_]))

(def default-errors
  {::unknown {:error/message {:en "unknown error"}}
   ::m/missing-key {:error/message {:en "missing required key"}}
   ::m/invalid-type {:error/message {:en "invalid type"}}
   ::m/extra-key {:error/message {:en "disallowed key"}}
   ::misspelled-key {:error/fn {:en (fn [{::keys [likely-misspelling-of]} _]
                                      (str "should be spelled " (str/join " or " (map last likely-misspelling-of))))}}
   'any? {:error/message {:en "should be any"}}
   'some? {:error/message {:en "shoud be some"}}
   'number? {:error/message {:en "should be a number"}}
   'integer? {:error/message {:en "should be an integer"}}
   'int? {:error/message {:en "should be an int"}}
   'pos-int? {:error/message {:en "should be a positive int"}}
   'neg-int? {:error/message {:en "should be a negative int"}}
   'nat-int? {:error/message {:en "should be a non-negative int"}}
   'pos? {:error/message {:en "should be positive"}}
   'neg? {:error/message {:en "should be negative"}}
   'float? {:error/message {:en "should be a float"}}
   'double? {:error/message {:en "should be a double"}}
   'boolean? {:error/message {:en "should be a boolean"}}
   'string? {:error/message {:en "should be a string"}}
   'ident? {:error/message {:en "should be an ident"}}
   'simple-ident? {:error/message {:en "should be a simple ident"}}
   'qualified-ident? {:error/message {:en "should be a qualified ident"}}
   'keyword? {:error/message {:en "should be a keyword"}}
   'simple-keyword? {:error/message {:en "should be a simple keyword"}}
   'qualified-keyword? {:error/message {:en "should be a qualified keyword"}}
   'symbol? {:error/message {:en "should be a symbol"}}
   'simple-symbol? {:error/message {:en "should be a simple symbol"}}
   'qualified-symbol? {:error/message {:en "should be a qualified symbol"}}
   'uuid? {:error/message {:en "should be a uuid"}}
   'uri? {:error/message {:en "should be a uri"}}
   #?@(:clj ['decimal? {:error/message {:en "should be a decimal"}}])
   'inst? {:error/message {:en "should be an inst"}}
   'seqable? {:error/message {:en "should be a seqable"}}
   'indexed? {:error/message {:en "should be an indexed"}}
   'map? {:error/message {:en "should be a map"}}
   'vector? {:error/message {:en "should be a vector"}}
   'list? {:error/message {:en "should be a list"}}
   'seq? {:error/message {:en "should be a seq"}}
   'char? {:error/message {:en "should be a char"}}
   'set? {:error/message {:en "should be a set"}}
   'nil? {:error/message {:en "should be nil"}}
   'false? {:error/message {:en "should be false"}}
   'true? {:error/message {:en "should be true"}}
   'zero? {:error/message {:en "should be zero"}}
   #?@(:clj ['rational? {:error/message {:en "should be a rational"}}])
   'coll? {:error/message {:en "should be a coll"}}
   'empty? {:error/message {:en "should be empty"}}
   'associative? {:error/message {:en "should be an associative"}}
   'sequential? {:error/message {:en "should be a sequential"}}
   #?@(:clj ['ratio? {:error/message {:en "should be a ratio"}}])
   #?@(:clj ['bytes? {:error/message {:en "should be bytes"}}])
   :re {:error/message {:en "should match regex"}}
   :enum {:error/fn {:en (fn [{:keys [schema]} _]
                           (str "should be "
                                (if (= 1 (count (m/children schema)))
                                  (first (m/children schema))
                                  (str "either " (->> (m/children schema) butlast (str/join ", "))
                                       " or " (last (m/children schema))))))}}
   :int {:error/fn {:en (fn [{:keys [schema value]} _]
                          (let [{:keys [min max]} (m/properties schema)]
                            (cond
                              (not (int? value)) "should be an integer"
                              (and min (= min max)) (str "should be " min)
                              (and min max) (str "should be between " min " and " max)
                              min (str "should be at least " min)
                              max (str "should be at most " max))))}}
   :string {:error/fn {:en (fn [{:keys [schema value]} _]
                             (let [{:keys [min max]} (m/properties schema)]
                               (cond
                                 (not (string? value)) "should be a string"
                                 (and min (= min max)) (str "should be " min " characters")
                                 (and min max) (str "should be between " min " and " max " characters")
                                 min (str "should be at least " min " characters")
                                 max (str "should be at most " max " characters"))))}}})

(defn- -maybe-localized [x locale]
  (if (map? x) (get x locale) x))

(defn- -message [error x locale options]
  (if x (or (if-let [fn (-maybe-localized (:error/fn x) locale)] ((m/eval fn) error options))
            (-maybe-localized (:error/message x) locale))))

(defn- -ensure [x k]
  (if (sequential? x)
    (let [size' (count x)]
      (if (> k size') (into (vec x) (repeat (- (inc k) size') nil)) x))
    x))

(defn- -just-error? [x]
  (and (vector? x) (= 1 (count x)) (string? (first x))))

(defn- -get [x k]
  (if (set? x) (-> x vec (get k)) (get x k)))

(defn- -put [x k v]
  (if (set? x) (conj x v) (update x k (fn [e] (if (-just-error? v) (into (vec e) v) v)))))

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
;; spell checking (kudos to https://github.com/bhauman/spell-spec)
;;

(defn- -length->threshold [len]
  (condp #(<= %2 %1) len, 4 0, 5 1, 6 2, 11 3, 20 4 (int (* 0.2 len))))

(defn- -next-row [previous current other-seq]
  (reduce
    (fn [row [diagonal above other]]
      (let [update-val (if (= other current) diagonal (inc (min diagonal above (peek row))))]
        (conj row update-val)))
    [(inc (first previous))]
    (map vector previous (next previous) other-seq)))

(defn- -levenshtein [sequence1 sequence2]
  (peek (reduce (fn [previous current] (-next-row previous current sequence2))
                (map #(identity %2) (cons nil sequence2) (range))
                sequence1)))

(defn- -similar-key [ky ky2]
  (let [min-len (apply min (map (comp count #(if (str/starts-with? % ":") (subs % 1) %) str) [ky ky2]))
        dist (-levenshtein (str ky) (str ky2))]
    (when (<= dist (-length->threshold min-len)) dist)))

;; a tricky part is is that a keyword is not considered misspelled
;; if its substitute is already present in the original map
(defn- -likely-misspelled [value known-keys]
  (fn [key]
    (when-not (known-keys key)
      (->> known-keys
           (filter #(-similar-key % key))
           (remove (set (keys value)))
           not-empty))))

(defn- -most-similar-to [value key known-keys]
  (->> ((-likely-misspelled value known-keys) key)
       (map (juxt #(-levenshtein (str %) (str key)) identity))
       (filter first)
       (sort-by first)
       (map second)
       not-empty))

;;
;; public api
;;

(defn error-path
  ([error]
   (error-path error nil))
  ([error options]
   (into (:in error) (-path error options))))

(defn error-message
  ([error]
   (error-message error nil))
  ([{:keys [schema type] :as error}
    {:keys [errors locale default-locale]
     :or {errors default-errors
          default-locale :en} :as options}]
   (or (-message error (-error schema) locale options)
       (-message error (m/properties schema) locale options)
       (-message error (errors (m/type schema)) locale options)
       (-message error (errors type) locale options)
       (-message error (-error schema) default-locale options)
       (-message error (m/properties schema) default-locale options)
       (-message error (errors (m/type schema)) default-locale options)
       (-message error (errors type) default-locale options)
       (-message error (errors ::unknown) locale options)
       (-message error (errors ::unknown) default-locale options))))

(defn with-error-message
  ([error]
   (with-error-message error nil))
  ([error options]
   (assoc error :message (error-message error options))))

(defn with-error-messages
  ([explanation]
   (with-error-messages explanation nil))
  ([explanation {f :wrap :or {f identity} :as options}]
   (when explanation
     (update explanation :errors (partial map #(f (with-error-message % options)))))))

(defn with-spell-checking
  ([explanation]
   (with-spell-checking explanation nil))
  ([explanation {:keys [keep-likely-misspelled-of]}]
   (when explanation
     (let [!likely-misspelling-of (atom #{})]
       (update
         explanation
         :errors
         (fn [errors]
           (as-> errors $
                 (mapv (fn [{:keys [schema path type] :as error}]
                         (if (= type ::m/extra-key)
                           (let [keys (->> schema (m/map-entries) (map first) (set))
                                 value (get-in (:value explanation) (butlast path))
                                 similar (-most-similar-to value (last path) keys)
                                 likely-misspelling-of (mapv (partial conj (vec (butlast path))) (vec similar))]
                             (swap! !likely-misspelling-of into likely-misspelling-of)
                             (cond-> error similar (assoc :type ::misspelled-key
                                                          ::likely-misspelling-of likely-misspelling-of)))
                           error)) $)
                 (if-not keep-likely-misspelled-of
                   (remove (fn [{:keys [path type]}]
                             (and (@!likely-misspelling-of path)
                                  (= type ::m/missing-key))) $)
                   $))))))))

(defn humanize
  ([explanation]
   (humanize explanation nil))
  ([{:keys [value errors]} {f :wrap :or {f :message} :as options}]
   (if errors
     (if (coll? value)
       (reduce
         (fn [acc error]
           (-assoc-in acc value (error-path error options) [(f (with-error-message error options))]))
         nil errors)
       [(f (with-error-message (first errors) options))]))))
