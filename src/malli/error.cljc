(ns malli.error
  (:require [malli.core :as m]
            [clojure.string :as str]
            [malli.util :as mu]))

(defn -pred-min-max-error-fn [{:keys [pred message]}]
  (fn [{:keys [schema value]} _]
    (let [{:keys [min max]} (m/properties schema)]
      (cond
        (not (pred value)) message
        (and min (= min max)) (str "should be " min)
        (and min max) (str "should be between " min " and " max)
        min (str "should be at least " min)
        max (str "should be at most " max)))))

(def default-errors
  {::unknown {:error/message {:en "unknown error"}}
   ::m/missing-key {:error/message {:en "missing required key"}}
   ::m/limits {:error/fn {:en (fn [{:keys [schema _value]} _]
                                (let [{:keys [min max]} (m/properties schema)]
                                  (cond
                                    (and min (= min max)) (str "should have " min " elements")
                                    (and min max) (str "should have between " min " and " max " elements")
                                    min (str "should have at least " min " elements")
                                    max (str "should have at most " max " elements"))))}}
   ::m/tuple-size {:error/fn {:en (fn [{:keys [schema value]} _]
                                    (let [size (count (m/children schema))]
                                      (str "invalid tuple size " (count value) ", expected " size)))}}
   ::m/invalid-type {:error/message {:en "invalid type"}}
   ::m/extra-key {:error/message {:en "disallowed key"}}
   :malli.core/invalid-dispatch-value {:error/message {:en "invalid dispatch value"}}
   ::misspelled-key {:error/fn {:en (fn [{::keys [likely-misspelling-of]} _]
                                      (str "should be spelled " (str/join " or " (map last likely-misspelling-of))))}}
   ::misspelled-value {:error/fn {:en (fn [{::keys [likely-misspelling-of]} _]
                                        (str "did you mean " (str/join " or " (map last likely-misspelling-of))))}}
   ::m/input-remaining {:error/message {:en "input remaining"}}
   ::m/end-of-input {:error/message {:en "end of input"}}
   'any? {:error/message {:en "should be any"}}
   'some? {:error/message {:en "should be some"}}
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
   :=> {:error/message {:en "invalid function"}}
   'ifn? {:error/message {:en "should be an ifn"}}
   'fn? {:error/message {:en "should be an fn"}}
   :enum {:error/fn {:en (fn [{:keys [schema]} _]
                           (str "should be "
                                (if (= 1 (count (m/children schema)))
                                  (first (m/children schema))
                                  (str "either " (->> (m/children schema) butlast (str/join ", "))
                                       " or " (last (m/children schema))))))}}
   :any {:error/message {:en "should be any"}}
   :nil {:error/message {:en "should be nil"}}
   :string {:error/fn {:en (fn [{:keys [schema value]} _]
                             (let [{:keys [min max]} (m/properties schema)]
                               (cond
                                 (not (string? value)) "should be a string"
                                 (and min (= min max)) (str "should be " min " characters")
                                 (and min max) (str "should be between " min " and " max " characters")
                                 min (str "should be at least " min " characters")
                                 max (str "should be at most " max " characters"))))}}
   :int {:error/fn {:en (-pred-min-max-error-fn {:pred int?, :message "should be an integer"})}}
   :double {:error/fn {:en (-pred-min-max-error-fn {:pred double?, :message "should be a double"})}}
   :boolean {:error/message {:en "should be a boolean"}}
   :keyword {:error/message {:en "should be a keyword"}}
   :symbol {:error/message {:en "should be a symbol"}}
   :qualified-keyword {:error/message {:en "should be a qualified keyword"}}
   :qualified-symbol {:error/message {:en "should be a qualified symbol"}}
   :uuid {:error/message {:en "should be a uuid"}}
   :> {:error/fn {:en (fn [{:keys [schema value]} _]
                        (if (number? value)
                          (str "should be larger than " (first (m/children schema)))
                          "should be a number"))}}
   :>= {:error/fn {:en (fn [{:keys [schema value]} _]
                         (if (number? value)
                           (str "should be at least " (first (m/children schema)))
                           "should be a number"))}}
   :< {:error/fn {:en (fn [{:keys [schema value]} _]
                        (if (number? value)
                          (str "should be smaller than " (first (m/children schema)))
                          "should be a number"))}}
   :<= {:error/fn {:en (fn [{:keys [schema value]} _]
                         (if (number? value)
                           (str "should be at most " (first (m/children schema)))
                           "should be a number"))}}
   := {:error/fn {:en (fn [{:keys [schema]} _]
                        (str "should be " (first (m/children schema))))}}
   :not= {:error/fn {:en (fn [{:keys [schema]} _]
                           (str "should not be " (first (m/children schema))))}}})

(defn- -maybe-localized [x locale]
  (if (map? x) (get x locale) x))

(defn- -message [error props locale options]
  (let [options (or options (m/options (:schema error)))]
    (when props (or (when-let [fn (-maybe-localized (:error/fn props) locale)] ((m/eval fn options) error options))
                  (-maybe-localized (:error/message props) locale)))))

(defn -error [e] ^::error [e])
(defn -error? [x] (-> x meta ::error))

(defn- -get [x k] (when (or (set? x) (associative? x)) (get x k)))

(defn -push [x k v]
  (let [x' (if-let [x' (when (and (number? k) (or (set? x) (sequential? x))) (vec x))]
             (let [size' (count x')] (if (> k size') (into x (repeat (- (inc k) size') nil)) x)) x)]
    (if (set? x') (conj x' v) (assoc x' k v))))

(defn -assoc-in [a v [p & ps] e]
  (let [v' (-get v p)
        a' (or a (cond (sequential? v) [], (record? v) {}, :else (empty v)))]
    (cond
      ;; error present, let's not go deeper
      (and p (-error? a')) a
      ;; we can go deeper
      p (-push a' p (-assoc-in (-get a' p) v' ps e))
      ;; it's a map!
      (map? a) (-assoc-in a' v [:malli/error] e)
      ;; accumulate
      (-error? a') (conj a' e)
      ;; lose it
      (vector? (not-empty a')) a'
      ;; first blood
      :else (-error e))))

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
  (let [min-len (apply min (map (m/-comp count #(if (str/starts-with? % ":") (subs % 1) %) str) [ky ky2]))
        dist (-levenshtein (str ky) (str ky2))]
    (when (<= dist (-length->threshold min-len)) dist)))

(defn- -likely-misspelled [keys known-keys key]
  (when-not (known-keys key)
    (->> known-keys (filter #(-similar-key % key)) (remove keys) (not-empty))))

(defn- -most-similar-to [keys key known-keys]
  (->> (-likely-misspelled keys known-keys key)
       (map (juxt #(-levenshtein (str %) (str key)) identity))
       (filter first)
       (sort-by first)
       (map second)
       (not-empty)))

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
    {:keys [errors unknown locale default-locale]
     :or {errors default-errors
          unknown true
          default-locale :en} :as options}]
   (or (-message error (m/properties schema) locale options)
       (-message error (m/type-properties schema) locale options)
       (-message error (errors type) locale options)
       (-message error (errors (m/type schema)) locale options)
       (-message error (m/properties schema) default-locale options)
       (-message error (m/type-properties schema) default-locale options)
       (-message error (errors type) default-locale options)
       (-message error (errors (m/type schema)) default-locale options)
       (and unknown (-message error (errors ::unknown) locale options))
       (and unknown (-message error (errors ::unknown) default-locale options)))))

(defn -resolve-direct-error [_ error options]
  [(error-path error options) (error-message error options)])

(defn ^:no-doc -resolve-root-error [{:keys [schema]} {:keys [path] :as error} options]
  (let [options (assoc options :unknown false)]
    (loop [p path, l nil, mp path, m (error-message error options)]
      (let [[p' m'] (or (when-let [m' (error-message {:schema (mu/get-in schema p)} options)] [p m'])
                        (let [res (and l (mu/find (mu/get-in schema p) l))]
                          (when (vector? res)
                            (let [[_ props schema] res
                                  schema (mu/update-properties schema merge props)
                                  message (error-message {:schema schema} options)]
                              (when message [(conj p l) message]))))
                        (when m [mp m]))]
        (if (seq p) (recur (pop p) (last p) p' m') (when m [(mu/path->in schema p') m']))))))

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
     (update explanation :errors (fn [errors] (doall (map #(f (with-error-message % options)) errors)))))))

(defn with-spell-checking
  ([explanation]
   (with-spell-checking explanation nil))
  ([explanation {:keys [keep-likely-misspelled-of]}]
   (when explanation
     (let [!likely-misspelling-of (atom #{})
           handle-invalid-value (fn [schema _ value]
                                  (let [dispatch (:dispatch (m/properties schema))]
                                    (when (keyword? dispatch)
                                      (let [value (dispatch value)]
                                        [::misspelled-value value #{value}]))))
           types {::m/extra-key (fn [_ path value] [::misspelled-key (last path) (-> value keys set (or #{}))])
                  ::m/invalid-dispatch-value handle-invalid-value}]
       (update
         explanation
         :errors
         (fn [errors]
           (as-> errors $
                 (mapv (fn [{:keys [schema path type] :as error}]
                         (if-let [get-keys (types type)]
                           (let [known-keys (->> schema (m/entries) (map first) (set))
                                 value (get-in (:value explanation) (butlast path))
                                 [error-type key keys] (get-keys schema path value)
                                 similar (-most-similar-to keys key known-keys)
                                 likely-misspelling-of (mapv #(conj (vec (butlast path)) %) (vec similar))]
                             (swap! !likely-misspelling-of into likely-misspelling-of)
                             (cond-> error similar (assoc :type error-type
                                                          ::likely-misspelling-of likely-misspelling-of)))
                           error)) $)
                 (if-not keep-likely-misspelled-of
                   (remove (fn [{:keys [path type]}]
                             (and (@!likely-misspelling-of path)
                                  (= type ::m/missing-key))) $)
                   $))))))))

(defn humanize
  "Humanized a explanation. Accepts the following optitons:

  - `:wrap`, a function of `error -> message`, defaulting to `:message`
  - `:resolve`, a function of `explanation error options -> path message`"
  ([explanation]
   (humanize explanation nil))
  ([{:keys [value errors] :as explanation} {:keys [wrap resolve]
                                            :or {wrap :message
                                                 resolve -resolve-direct-error}
                                            :as options}]
   (when errors
     (reduce
       (fn [acc error]
         (let [[path message] (resolve explanation error options)]
           (-assoc-in acc value path (wrap (assoc error :message message)))))
       nil errors))))
