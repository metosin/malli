(ns malli.core
  (:refer-clojure :exclude [eval type -deref deref -lookup -key])
  #?(:cljs (:require-macros malli.core))
  (:require [malli.sci :as ms]
            [malli.impl.util :as miu]
            [malli.impl.regex :as re]
            [malli.registry :as mr])
  #?(:clj (:import (java.util.regex Pattern)
                   (clojure.lang Associative IPersistentCollection MapEntry IPersistentVector)
                   (malli.impl.util SchemaError)
                   (java.util.concurrent.atomic AtomicReference)
                   (java.util Collection LinkedList))))

(declare schema schema? into-schema into-schema? type eval default-registry
         -simple-schema -val-schema -ref-schema -schema-schema -registry
         parser unparser)

;;
;; protocols and records
;;

(defprotocol Schemas
  (-schema? [this])
  (-into-schema? [this]))

(defprotocol IntoSchema
  (-type [this] "returns type of the schema")
  (-type-properties [this] "returns schema type properties")
  (-properties-schema [this options] "maybe returns :map schema describing schema properties")
  (-children-schema [this options] "maybe returns sequence schema describing schema children")
  (-into-schema [this properties children options] "creates a new schema instance"))

(defprotocol Schema
  (-validator [this] "returns a predicate function that checks if the schema is valid")
  (-explainer [this path] "returns a function of `x in acc -> maybe errors` to explain the errors for invalid values")
  (-parser [this] "return a function of `x -> parsed-x | ::m/invalid` to explain how schema is valid.")
  (-unparser [this] "return the inverse (partial) function wrt. `-parser`; `parsed-x -> x | ::m/invalid`")
  (-transformer [this transformer method options]
    "returns a function to transform the value for the given schema and method.
    Can also return nil instead of `identity` so that more no-op transforms can be elided.")
  (-walk [this walker path options] "walks the schema and its children, ::m/walk-entry-vals, ::m/walk-refs, ::m/walk-schema-refs options effect how walking is done.")
  (-simplify [this] "returns a new simplified schema based on eg., whether its children are reachable.")
  (-unreachable? [this] "returns true if this schema never validates or generates values, otherwise false. false-negatives allowed.")
  (-properties [this] "returns original schema properties")
  (-options [this] "returns original options")
  (-children [this] "returns schema children")
  (-parent [this] "returns the IntoSchema instance")
  (-form [this] "returns original form of the schema"))

(defprotocol MapSchema
  (-entries [this] "returns sequence of `key -val-schema` MapEntries"))

(defprotocol LensSchema
  (-keep [this] "returns truthy if schema contributes to value path")
  (-get [this key default] "returns schema at key")
  (-set [this key value] "returns a copy with key having new value"))

(defprotocol RefSchema
  (-ref [this] "returns the reference name")
  (-deref [this] "returns the referenced schema"))

(defprotocol RegexSchema
  (-regex-op? [this] "is this a regex operator (e.g. :cat, :*...)")
  (-regex-validator [this] "returns the raw internal regex validator implementation")
  (-regex-explainer [this path] "returns the raw internal regex explainer implementation")
  (-regex-unparser [this] "returns the raw internal regex unparser implementation")
  (-regex-parser [this] "returns the raw internal regex parser implementation")
  (-regex-transformer [this transformer method options] "returns the raw internal regex transformer implementation")
  (-regex-min-max [this] "returns size of the sequence as [min max] vector. nil max means unbuond."))

(extend-type #?(:clj Object, :cljs default)
  RegexSchema
  (-regex-op? [_] false)

  (-regex-validator [this]
    (if (satisfies? RefSchema this)
      (-regex-validator (-deref this))
      (re/item-validator (-validator this))))

  (-regex-explainer [this path]
    (if (satisfies? RefSchema this)
      (-regex-explainer (-deref this) path)
      (re/item-explainer path this (-explainer this path))))

  (-regex-parser [this]
    (if (satisfies? RefSchema this)
      (-regex-parser (-deref this))
      (re/item-parser (parser this))))

  (-regex-unparser [this]
    (if (satisfies? RefSchema this)
      (-regex-unparser (-deref this))
      (re/item-unparser (unparser this))))

  (-regex-transformer [this transformer method options]
    (if (satisfies? RefSchema this)
      (-regex-transformer (-deref this) transformer method options)
      (re/item-transformer method (-validator this) (or (-transformer this transformer method options) identity))))

  (-regex-min-max [_] {:min 1, :max 1}))

(defprotocol Walker
  (-accept [this schema path options])
  (-inner [this schema path options])
  (-outer [this schema path children options]))

(defprotocol Transformer
  (-transformer-chain [this] "returns transformer chain as a vector of maps with :name, :encoders, :decoders and :options")
  (-value-transformer [this schema method options] "returns an value transforming interceptor for the given schema and method"))

#?(:clj (defmethod print-method SchemaError [v ^java.io.Writer w] (.write w (str "#Error" (->> v (filter val) (into {}))))))
#?(:clj (defmethod print-method ::into-schema [v ^java.io.Writer w] (.write w (str "#IntoSchema{:type " (pr-str (-type ^IntoSchema v)) "}"))))
#?(:clj (defmethod print-method ::schema [v ^java.io.Writer w] (.write w (pr-str (-form ^Schema v)))))

;;
;; impl
;;

(defn -fail!
  ([type] (-fail! type nil))
  ([type data] (throw (ex-info (str type " " (pr-str data)) {:type type, :message type, :data data}))))

(defn -safe-pred [f] #(try (boolean (f %)) (catch #?(:clj Exception, :cljs js/Error) _ false)))

(defn -keyword->string [x]
  (if (keyword? x)
    (if-let [nn (namespace x)]
      (str nn "/" (name x))
      (name x))
    x))

(defn -unlift-keys [m prefix]
  (reduce-kv #(if (= (name prefix) (namespace %2)) (assoc %1 (keyword (name %2)) %3) %1) {} m))

(defn -check-children! [type properties children {:keys [min max] :as opts}]
  (if (or (and min (< (count children) min)) (and max (> (count children) max)))
    (-fail! ::child-error (merge {:type type, :properties properties, :children children} opts))))

(defn -create-form [type properties children]
  (cond
    (and (seq properties) (seq children)) (into [type properties] children)
    (seq properties) [type properties]
    (seq children) (into [type] children)
    :else type))

(defn -pointer [id schema options] (-into-schema (-schema-schema {:id id}) nil [schema] options))

(defn -reference? [?schema] (or (string? ?schema) (qualified-keyword? ?schema)))

(defn -lazy [ref options] (-into-schema (-ref-schema {:lazy true}) nil [ref] options))

(defn -boolean-fn [x] (cond (boolean? x) (constantly x) (ifn? x) x :else (constantly false)))

(defn -comp
  ([] identity)
  ([f] f)
  ([f g] (fn [x] (f (g x))))
  ([f g h] (fn [x] (f (g (h x)))))
  #?@(:clj  [([f1 f2 f3 f4] (fn [x] (-> x f4 f3 f2 f1)))
             ([f1 f2 f3 f4 f5] (fn [x] (-> x f5 f4 f3 f2 f1)))
             ([f1 f2 f3 f4 f5 f6] (fn [x] (-> x f6 f5 f4 f3 f2 f1)))
             ([f1 f2 f3 f4 f5 f6 f7] (fn [x] (-> x f7 f6 f5 f4 f3 f2 f1)))
             ([f1 f2 f3 f4 f5 f6 f7 f8] (fn [x] (-> x f8 f7 f6 f5 f4 f3 f2 f1)))
             ([f1 f2 f3 f4 f5 f6 f7 f8 & fs]
              (-comp
                (apply -comp fs)
                (fn [x] (-> x f8 f7 f6 f5 f4 f3 f2 f1))))]
      :cljs [([f1 f2 f3 & fs]
              (-comp
                (apply -comp fs)
                (fn [x] (-> x f3 f2 f1))))]))

(defn -update [m k f] (assoc m k (f (get m k))))

(defn -memoize [f]
  (let [value #?(:clj (AtomicReference. nil), :cljs (atom nil))]
    (fn [] #?(:clj (or (.get value) (do (.set value (f)) (.get value))), :cljs (or @value (reset! value (f)))))))

(defn -inner-indexed [walker path children options]
  (mapv (fn [[i c]] (-inner walker c (conj path i) options)) (map-indexed vector children)))

(defn -inner-entries [walker path entries options]
  (mapv (fn [[k s]] [k (-properties s) (-inner walker s (conj path k) options)]) entries))

(defn -set-children [schema children]
  (-into-schema (-parent schema) (-properties schema) children (-options schema)))

(defn -update-options [schema f & args]
  (-into-schema (-parent schema) (-properties schema) (-children schema) (apply f (-options schema) args)))

(defn -update-properties [schema f & args]
  (-into-schema (-parent schema) (apply f (-properties schema) args) (-children schema) (-options schema)))

(defn -set-assoc-children [schema key value]
  (-set-children schema (assoc (-children schema) key value)))

(defn -get-entries [schema key default]
  (or (some (if (and (vector? key) (= ::find (nth key 0)))
              (fn [[k :as e]] (when (= k (nth key 1)) e))
              (fn [[k _ s]] (when (= k key) s)))
            (-children schema)) default))

(defn -set-entries [schema ?key value]
  (let [found (atom nil)
        [key props override] (if (vector? ?key) [(first ?key) (second ?key) true] [?key])
        children (cond-> (mapv (fn [[k p :as entry]]
                                 (if (= key k)
                                   (do (reset! found true) [key (if override props p) value])
                                   entry))
                               (-children schema))
                         (not @found) (conj (if key [key props value] (-fail! ::key-missing)))
                         :always (->> (filter (fn [e] (-> e last some?)))))]
    (-set-children schema children)))

(defn -parse-entries [children {:keys [naked-keys lazy-refs]} options]
  (let [-parse (fn [e] (let [[[k ?p ?v] f] (cond
                                             (not (sequential? e)) (if (and naked-keys (-reference? e)) [[e nil e] e] (-fail! ::invalid-ref {:ref e}))
                                             (and (= 1 (count e)) (-reference? (first e))) (if naked-keys [[(first e) nil (first e)] e])
                                             (and (= 2 (count e)) (-reference? (first e)) (map? (last e))) (if naked-keys [(conj e (first e)) e])
                                             :else [e (->> (-update (vec e) (dec (count e)) (-comp -form #(schema % options))) (keep identity) (vec))])
                             [p ?s] (if (or (nil? ?p) (map? ?p)) [?p ?v] [nil ?p])
                             s (cond-> (or ?s (if (-reference? k) f)) lazy-refs (-lazy options))
                             c [k p (schema s options)]]
                         {:children [c]
                          :entries [(miu/-tagged k (-val-schema (last c) p))]
                          :forms [f]}))
        es (reduce #(merge-with into %1 %2) {} (mapv -parse children))
        keys (->> es :entries (map first))]
    (when-not (= keys (distinct keys))
      (-fail! ::non-distinct-entry-keys {:keys keys}))
    es))

(defn -guard [pred tf]
  (when tf (fn [x] (if (pred x) (tf x) x))))

(defn -intercepting
  ([interceptor] (-intercepting interceptor nil))
  ([{:keys [enter leave]} f] (some->> [leave f enter] (keep identity) (seq) (apply -comp))))

(defn -parent-children-transformer [parent children transformer method options]
  (let [parent-transformer (-value-transformer transformer parent method options)
        child-transformers (into [] (keep #(-transformer % transformer method options)) children)
        child-transformer (if (seq child-transformers) (apply -comp (rseq child-transformers)))]
    (-intercepting parent-transformer child-transformer)))

(defn- -properties-and-children [[x :as xs]]
  (if (or (nil? x) (map? x))
    [x (rest xs)]
    [nil xs]))

(defn- -register-var [registry v]
  (let [name (-> v meta :name)
        schema (-simple-schema {:type name, :pred @v})]
    (-> registry
        (assoc name schema)
        (assoc @v schema))))

(defn -registry
  ([] default-registry)
  ([{:keys [registry]}] (or (mr/registry registry) default-registry)))

(defn- -lookup [?schema options]
  (let [registry (-registry options)]
    (or (mr/-schema registry ?schema)
        (some-> registry (mr/-schema (clojure.core/type ?schema)) (-into-schema nil [?schema] options)))))

(defn- -schema [?schema options]
  (or (and (or (schema? ?schema) (into-schema? ?schema)) ?schema)
      (-lookup ?schema options)
      (-fail! ::invalid-schema {:schema ?schema})))

(defn -into-transformer [x]
  (cond
    (satisfies? Transformer x) x
    (fn? x) (-into-transformer (x))
    :else (-fail! ::invalid-transformer {:value x})))

(defn- -property-registry [m options f]
  (let [options (assoc options ::allow-invalid-refs true)]
    (reduce-kv (fn [acc k v] (assoc acc k (f (schema v options)))) {} m)))

(defn -properties-and-options [properties options f]
  (if-let [r (some-> properties :registry)]
    (let [options (-update options :registry #(mr/composite-registry r (or % (-registry options))))]
      [(assoc properties :registry (-property-registry r options f)) options])
    [properties options]))

(defn -function-info [schema]
  (if (= (type schema) :=>)
    (let [[input output] (-children schema)
          {:keys [min max]} (-regex-min-max input)]
      (cond-> {:min min
               :arity (if (= min max) min :varargs)
               :input input
               :output output}
              max (assoc :max max)))))

(defn -map-transformer [ts]
  #?(:clj  (apply -comp (map (fn child-transformer [[k t]]
                               (fn [^Associative x]
                                 (if-let [e ^MapEntry (.entryAt x k)]
                                   (.assoc x k (t (.val e))) x))) (rseq ts)))
     :cljs (fn [x] (reduce (fn child-transformer [m [k t]]
                             (if-let [entry (find m k)]
                               (assoc m k (t (val entry)))
                               m)) x ts))))

(defn -tuple-transformer [ts]
  #?(:clj  (let [tl (LinkedList. ^Collection (mapv (fn [[k v]] (MapEntry/create k v)) ts))]
             (fn [x] (let [i (.iterator ^Iterable tl)]
                       (loop [x ^IPersistentVector x]
                         (if (.hasNext i)
                           (let [e ^MapEntry (.next i), k (.key e)]
                             (recur (.assoc x k ((.val e) (.nth x k)))))
                           x)))))
     :cljs (fn [x] (reduce-kv -update x ts))))

(defn -collection-transformer [t empty]
  #?(:clj  (fn [x] (let [i (.iterator ^Iterable x)]
                     (loop [x ^IPersistentCollection empty]
                       (if (.hasNext i)
                         (recur (.cons x (t (.next i))))
                         x))))
     :cljs (fn [x] (into (if x empty) (map t) x))))

;;
;; simple schema helpers
;;

(defn -min-max-pred [f]
  (fn [{:keys [min max]}]
    (cond
      (not (or min max)) nil
      (and (and min max) f) (fn [x] (let [size (f x)] (<= min size max)))
      (and min max) (fn [x] (<= min x max))
      (and min f) (fn [x] (<= min (f x)))
      min (fn [x] (<= min x))
      (and max f) (fn [x] (<= (f x) max))
      max (fn [x] (<= x max)))))

(defn -validate-limits
  [min max]
  (or ((-min-max-pred count) {:min min :max max}) (constantly true)))

(defn -qualified-keyword-pred [properties]
  (when-let [ns-name (some-> properties :namespace name)]
    (fn [x] (= (namespace x) ns-name))))

;;
;; Protocol Cache
;;

(let [extend (fn [protocol this]
               ;; cljs: class clojure.lang.PersistentList cannot be cast to class clojure.lang.Named
               #?(:clj (let [s? (satisfies? Schema this)
                             is? (satisfies? IntoSchema this)]
                         (extend-protocol Schemas (class this)
                           (-schema? [_] s?)
                           (-into-schema? [_] is?)))))]
  (extend-protocol Schemas
    nil
    (-schema? [_] false)
    (-into-schema? [_] false)
    #?(:clj Object, :cljs default)
    (-schema? [this] #?(:clj (extend Schema this)) (satisfies? Schema this))
    (-into-schema? [this] #?(:clj (extend IntoSchema this)) (satisfies? IntoSchema this))))

;;
;; Schemas
;;

(defn -simple-schema [?props]
  (let [props* (atom (if (map? ?props) ?props))]
    ^{:type ::into-schema}
    (reify IntoSchema
      (-type [_] (:type @props*))
      (-type-properties [_] (:type-properties @props*))
      (-properties-schema [_ _])
      (-children-schema [_ _])
      (-into-schema [parent properties children options]
        (if (fn? ?props)
          (-into-schema (-simple-schema (?props properties children)) properties children options)
          (let [{:keys [type pred property-pred min max], :or {min 0, max 0}} ?props]
            (reset! props* ?props)
            (-check-children! type properties children {:min min, :max max})
            (let [pvalidator (if property-pred (property-pred properties))
                  validator (if pvalidator (fn [x] (and (pred x) (pvalidator x))) pred)
                  form (-create-form type properties children)]
              ^{:type ::schema}
              (reify
                Schema
                (-validator [_] validator)
                (-explainer [this path]
                  (fn explain [x in acc]
                    (if-not (validator x) (conj acc (miu/-error path in this x)) acc)))
                (-parser [_] (fn [x] (if (validator x) x ::invalid)))
                (-unparser [this] (-parser this))
                (-transformer [this transformer method options]
                  (-intercepting (-value-transformer transformer this method options)))
                (-walk [this walker path options]
                  (if (-accept walker this path options)
                    (-outer walker this path (vec children) options)))
                (-simplify [this] this)
                (-unreachable? [this] (= :never type))
                (-properties [_] properties)
                (-options [_] options)
                (-children [_] children)
                (-parent [_] parent)
                (-form [_] form)
                LensSchema
                (-keep [_])
                (-get [_ _ default] default)
                (-set [this key _] (-fail! ::non-associative-schema {:schema this, :key key}))))))))))

(defn -nil-schema [] (-simple-schema {:type :nil, :pred nil?}))
(defn -any-schema [] (-simple-schema {:type :any, :pred any?}))
(defn -never-schema [] (-simple-schema {:type :never :pred (fn [_] false)}))
(defn -string-schema [] (-simple-schema {:type :string, :pred string?, :property-pred (-min-max-pred count)}))
(defn -int-schema [] (-simple-schema {:type :int, :pred int?, :property-pred (-min-max-pred nil)}))
(defn -double-schema [] (-simple-schema {:type :double, :pred double?, :property-pred (-min-max-pred nil)}))
(defn -boolean-schema [] (-simple-schema {:type :boolean, :pred boolean?}))
(defn -keyword-schema [] (-simple-schema {:type :keyword, :pred keyword?}))
(defn -symbol-schema [] (-simple-schema {:type :symbol, :pred symbol?}))
(defn -qualified-keyword-schema [] (-simple-schema {:type :qualified-keyword, :pred qualified-keyword?, :property-pred -qualified-keyword-pred}))
(defn -qualified-symbol-schema [] (-simple-schema {:type :qualified-symbol, :pred qualified-symbol?}))
(defn -uuid-schema [] (-simple-schema {:type :uuid, :pred uuid?}))

(defn -and-schema []
  ^{:type ::into-schema}
  (reify IntoSchema
    (-type [_] :and)
    (-type-properties [_])
    (-properties-schema [_ _])
    (-children-schema [_ _])
    (-into-schema [parent properties children options]
      (-check-children! :and properties children {:min 1})
      (let [children (mapv #(schema % options) children)
            form (-create-form :and properties (map -form children))
            ->parser (fn [f m] (let [parsers (m (mapv f children))]
                                 #(reduce (fn [x parser] (miu/-map-invalid reduced (parser x))) % parsers)))]
        ^{:type ::schema}
        (reify
          Schema
          (-validator [_]
            (let [validators (distinct (map -validator children))]
              #?(:clj  (miu/-every-pred validators)
                 :cljs (if (second validators) (apply every-pred validators) (first validators)))))
          (-explainer [_ path]
            (let [explainers (mapv (fn [[i c]] (-explainer c (conj path i))) (map-indexed vector children))]
              (fn explain [x in acc] (reduce (fn [acc' explainer] (explainer x in acc')) acc explainers))))
          (-parser [_] (->parser -parser seq))
          (-unparser [_] (->parser -unparser rseq))
          (-transformer [this transformer method options]
            (-parent-children-transformer this children transformer method options))
          (-walk [this walker path options]
            (if (-accept walker this path options)
              (-outer walker this path (-inner-indexed walker path children options) options)))
          (-simplify [this] (if (-unreachable? this)
                              (schema :never)
                              this))
          (-unreachable? [this] (boolean (some -unreachable? children)))
          (-properties [_] properties)
          (-options [_] options)
          (-children [_] children)
          (-parent [_] parent)
          (-form [_] form)
          LensSchema
          (-keep [_])
          (-get [_ key default] (get children key default))
          (-set [this key value] (-set-assoc-children this key value)))))))

(defn -or-schema []
  ^{:type ::into-schema}
  (reify IntoSchema
    (-type [_] :or)
    (-type-properties [_])
    (-properties-schema [_ _])
    (-children-schema [_ _])
    (-into-schema [parent properties children options]
      (-check-children! :or properties children {:min 1})
      (let [children (mapv #(schema % options) children)
            form (-create-form :or properties (map -form children))
            ->parser (fn [f] (let [parsers (mapv f children)]
                               #(reduce (fn [_ parser] (miu/-map-valid reduced (parser %))) ::invalid parsers)))]
        ^{:type ::schema}
        (reify
          Schema
          (-validator [_]
            (let [validators (distinct (map -validator children))]
              #?(:clj  (miu/-some-pred validators)
                 :cljs (if (second validators) (fn [x] (boolean (some #(% x) validators))) (first validators)))))
          (-explainer [_ path]
            (let [explainers (mapv (fn [[i c]] (-explainer c (conj path i))) (map-indexed vector children))]
              (fn explain [x in acc]
                (reduce
                  (fn [acc' explainer]
                    (let [acc'' (explainer x in acc')]
                      (if (identical? acc' acc'') (reduced acc) acc'')))
                  acc explainers))))
          (-parser [_] (->parser -parser))
          (-unparser [_] (->parser -unparser))
          (-transformer [this transformer method options]
            (let [this-transformer (-value-transformer transformer this method options)]
              (if (seq children)
                (let [transformers (mapv #(or (-transformer % transformer method options) identity) children)
                      validators (mapv -validator children)]
                  (-intercepting this-transformer
                                 (if (= :decode method)
                                   (fn [x]
                                     (reduce-kv
                                       (fn [x i transformer]
                                         (let [x* (transformer x)]
                                           (if ((nth validators i) x*) (reduced x*) x)))
                                       x transformers))
                                   (fn [x]
                                     (reduce-kv
                                       (fn [x i validator] (if (validator x) (reduced ((nth transformers i) x)) x))
                                       x validators)))))
                (-intercepting this-transformer))))
          (-walk [this walker path options]
            (if (-accept walker this path options)
              (-outer walker this path (-inner-indexed walker path children options) options)))
          (-simplify [this] (let [children (into [] (remove -unreachable?) children)]
                              (case (count children)
                                0 (schema :never)
                                1 (first children)
                                (-set-children this children))))
          (-unreachable? [this] (every? -unreachable? children))
          (-properties [_] properties)
          (-options [_] options)
          (-children [_] children)
          (-parent [_] parent)
          (-form [_] form)
          LensSchema
          (-keep [_])
          (-get [_ key default] (get children key default))
          (-set [this key value] (-set-assoc-children this key value)))))))

(defn -orn-schema []
  ^{:type ::into-schema}
  (reify IntoSchema
    (-type [_] :orn)
    (-type-properties [_])
    (-properties-schema [_ _])
    (-children-schema [_ _])
    (-into-schema [parent properties children options]
      (-check-children! :orn properties children {:min 1})
      (let [{:keys [children entries forms]} (-parse-entries children {:naked-keys true} options)
            form (-create-form :orn properties forms)]
        ^{:type ::schema}
        (reify
          Schema
          (-validator [_]
            (let [validators (distinct (map (fn [[_ _ c]] (-validator c)) children))]
              #?(:clj  (miu/-some-pred validators)
                 :cljs (if (second validators) (fn [x] (boolean (some #(% x) validators))) (first validators)))))
          (-explainer [_ path]
            (let [explainers (mapv (fn [[k _ c]] (-explainer c (conj path k))) children)]
              (fn explain [x in acc]
                (reduce
                  (fn [acc' explainer]
                    (let [acc'' (explainer x in acc')]
                      (if (identical? acc' acc'') (reduced acc) acc'')))
                  acc explainers))))
          (-parser [_]
            (let [parsers (mapv (fn [[k _ c]]
                                  (let [c (-parser c)]
                                    (fn [x] (miu/-map-valid #(reduced (miu/-tagged k %)) (c x)))))
                                children)]
              (fn [x] (reduce (fn [_ parser] (parser x)) x parsers))))
          (-unparser [_]
            (let [unparsers (into {} (map (fn [[k _ c]] [k (-unparser c)])) children)]
              (fn [x]
                (if (miu/-tagged? x)
                  (if-some [unparse (get unparsers (key x))]
                    (unparse (val x))
                    ::invalid)
                  ::invalid))))
          (-transformer [this transformer method options]
            (let [this-transformer (-value-transformer transformer this method options)]
              (if (seq children)
                (let [transformers (mapv (fn [[_ _ c]] (or (-transformer c transformer method options) identity))
                                         children)
                      validators (mapv (fn [[_ _ c]] (-validator c)) children)]
                  (-intercepting this-transformer
                                 (if (= :decode method)
                                   (fn [x]
                                     (reduce-kv
                                       (fn [x i transformer]
                                         (let [x* (transformer x)]
                                           (if ((nth validators i) x*) (reduced x*) x)))
                                       x transformers))
                                   (fn [x]
                                     (reduce-kv
                                       (fn [x i validator] (if (validator x) (reduced ((nth transformers i) x)) x))
                                       x validators)))))
                (-intercepting this-transformer))))
          (-walk [this walker path options]
            (if (-accept walker this path options)
              (-outer walker this path (-inner-entries walker path entries options) options)))
          (-simplify [this] (if (-unreachable? this)
                              (schema :never)
                              ;; TODO remove unreachable children
                              this))
          (-unreachable? [this] (every? -unreachable? children))
          (-properties [_] properties)
          (-options [_] options)
          (-children [_] children)
          (-parent [_] parent)
          (-form [_] form)

          LensSchema
          (-keep [_] true)
          (-get [this key default] (-get-entries this key default))
          (-set [this key value] (-set-entries this key value)))))))

(defn -not-schema []
  ^{:type ::into-schema}
  (reify IntoSchema
    (-type [_] :not)
    (-type-properties [_])
    (-properties-schema [_ _])
    (-children-schema [_ _])
    (-into-schema [parent properties children options]
      (-check-children! :not properties children {:min 1 :max 1})
      (let [[schema :as children] (map #(schema % options) children)
            validator (complement (-validator schema))
            form (-create-form :not properties (map -form children))]
        ^{:type ::schema}
        (reify
          Schema
          (-validator [_] validator)
          (-explainer [this path]
            (fn explain [x in acc]
              (if-not (validator x) (conj acc (miu/-error (conj path 0) in this x)) acc)))
          (-parser [_] (fn [x] (if (validator x) x ::invalid)))
          (-unparser [this] (-parser this))
          (-transformer [this transformer method options]
            (-parent-children-transformer this children transformer method options))
          (-walk [this walker path options]
            (if (-accept walker this path options)
              (-outer walker this path (-inner-indexed walker path children options) options)))
          (-simplify [this] (cond
                              ; [:not :any] => :never
                              (-unreachable? this) (schema :never)
                              ; [:not :never] => :any
                              (= :never (-type schema)) (schema :any)
                              :else this))
          (-unreachable? [this] (= :any (-type schema)))
          (-properties [_] properties)
          (-options [_] options)
          (-children [_] children)
          (-parent [_] parent)
          (-form [_] form)
          LensSchema
          (-keep [_])
          (-get [_ key default] (get children key default))
          (-set [this key value] (-set-assoc-children this key value)))))))

(defn -val-schema
  ([schema properties]
   (-into-schema (-val-schema) properties [schema] (-options schema)))
  ([]
   ^{:type ::into-schema}
   (reify IntoSchema
     (-type [_] ::val)
     (-type-properties [_])
     (-properties-schema [_ _])
     (-children-schema [_ _])
     (-into-schema [parent properties children options]
       (-check-children! ::val properties children {:min 1, :max 1})
       (let [[schema :as children] (map #(schema % options) children)
             form (-create-form ::val properties (map -form children))]
         ^{:type ::schema}
         (reify Schema
           (-validator [_] (-validator schema))
           (-explainer [_ path] (-explainer schema path))
           (-parser [_] (-parser schema))
           (-unparser [_] (-unparser schema))
           (-transformer [this transformer method options]
             (-parent-children-transformer this children transformer method options))
           (-walk [this walker path options]
             (if (::walk-entry-vals options)
               (if (-accept walker this path options)
                 (-outer walker this path [(-inner walker schema path options)] options))
               (-walk schema walker path options)))
           (-simplify [this] this)
           (-unreachable? [this] false)
           (-properties [_] properties)
           (-options [_] (-options schema))
           (-children [_] children)
           (-parent [_] parent)
           (-form [_] form)
           LensSchema
           (-keep [_])
           (-get [_ key default] (if (= 0 key) schema default))
           (-set [_ key value] (if (= 0 key) (-val-schema value properties)))
           RefSchema
           (-ref [_])
           (-deref [_] schema)))))))

(defn -map-schema
  ([]
   (-map-schema {:naked-keys true}))
  ([opts] ;; :naked-keys
   ^{:type ::into-schema}
   (reify IntoSchema
     (-type [_] :map)
     (-type-properties [_])
     (-properties-schema [_ _])
     (-children-schema [_ _])
     (-into-schema [parent {:keys [closed] :as properties} children options]
       (let [{:keys [children entries forms]} (-parse-entries children opts options)
             form (-create-form :map properties forms)
             keyset (->> entries (map first) (set))
             ->parser (fn [f] (let [parsers (cond-> (mapv
                                                      (fn [[key {:keys [optional]} schema]]
                                                        (let [parser (f schema)]
                                                          (fn [m]
                                                            (if-let [e (find m key)]
                                                              (let [v (val e)
                                                                    v* (parser v)]
                                                                (cond (miu/-invalid? v*) (reduced v*)
                                                                      (identical? v* v) m
                                                                      :else (assoc m key v*)))
                                                              (if optional m (reduced ::invalid))))))
                                                      children)
                                                    closed (into [(fn [m]
                                                                    (reduce
                                                                      (fn [m k] (if (contains? keyset k) m (reduced (reduced ::invalid))))
                                                                      m (keys m)))]))]
                                (fn [x] (if (map? x) (reduce (fn [m parser] (parser m)) x parsers) ::invalid))))]
         ^{:type ::schema}
         (reify
           Schema
           (-validator [_]
             (let [validators (cond-> (mapv
                                        (fn [[key {:keys [optional]} value]]
                                          (let [valid? (-validator value)
                                                default (boolean optional)]
                                            #?(:clj  (fn [^Associative m] (if-let [map-entry (.entryAt m key)] (valid? (.val map-entry)) default))
                                               :cljs (fn [m] (if-let [map-entry (find m key)] (valid? (val map-entry)) default)))))
                                        children)
                                      closed (into [(fn [m]
                                                      (reduce
                                                        (fn [acc k] (if (contains? keyset k) acc (reduced false)))
                                                        true (keys m)))]))
                   validate #?(:clj (miu/-every-pred validators)
                               :cljs (fn [m] (boolean (reduce #(or (%2 m) (reduced false)) true validators))))]
               (fn [m] (and (map? m) (validate m)))))
           (-explainer [this path]
             (let [explainers (cond-> (mapv
                                        (fn [[key {:keys [optional]} schema]]
                                          (let [explainer (-explainer schema (conj path key))]
                                            (fn [x in acc]
                                              (if-let [e (find x key)]
                                                (explainer (val e) (conj in key) acc)
                                                (if-not optional
                                                  (conj acc (miu/-error (conj path key) (conj in key) this nil ::missing-key))
                                                  acc)))))
                                        children)
                                      closed (into [(fn [x in acc]
                                                      (reduce
                                                        (fn [acc k]
                                                          (if (contains? keyset k)
                                                            acc
                                                            (conj acc (miu/-error (conj path k) (conj in k) this nil ::extra-key))))
                                                        acc (keys x)))]))]
               (fn [x in acc]
                 (if-not (map? x)
                   (conj acc (miu/-error path in this x ::invalid-type))
                   (reduce
                     (fn [acc explainer]
                       (explainer x in acc))
                     acc explainers)))))
           (-parser [_] (->parser -parser))
           (-unparser [_] (->parser -unparser))
           (-transformer [this transformer method options]
             (let [this-transformer (-value-transformer transformer this method options)
                   ->children (reduce (fn [acc [k s]]
                                        (let [t (-transformer s transformer method options)]
                                          (cond-> acc t (conj [k t])))) [] entries)
                   apply->children (when (seq ->children) (-map-transformer ->children))
                   apply->children (-guard map? apply->children)]
               (-intercepting this-transformer apply->children)))
           (-walk [this walker path options]
             (if (-accept walker this path options)
               (-outer walker this path (-inner-entries walker path entries options) options)))
           (-simplify [this] (if (-unreachable? this)
                               (schema :never)
                               (let [new-children (into []
                                                        (remove (fn [[_ {:keys [optional]} schema]]
                                                                  (and optional
                                                                       (-unreachable? schema))))
                                                        children)]
                                 (cond-> this
                                   (not= (count new-children)
                                         (count children))
                                   (-> (-set-children new-children)
                                       (-update-properties assoc :closed true))))))
           (-unreachable? [this] (boolean
                                   (some (fn [[_ {:keys [optional]} schema]]
                                           (and (not optional)
                                                (-unreachable? schema)))
                                         children)))
           (-properties [_] properties)
           (-options [_] options)
           (-children [_] children)
           (-parent [_] parent)
           (-form [_] form)
           MapSchema
           (-entries [_] entries)
           LensSchema
           (-keep [_] true)
           (-get [this key default] (-get-entries this key default))
           (-set [this key value] (-set-entries this key value))))))))

(defn -map-of-schema []
  ^{:type ::into-schema}
  (reify IntoSchema
    (-type [_] :map-of)
    (-type-properties [_])
    (-properties-schema [_ _])
    (-children-schema [_ _])
    (-into-schema [parent {:keys [min max] :as properties} children options]
      (-check-children! :map-of properties children {:min 2 :max 2})
      (let [[key-schema value-schema :as children] (mapv #(schema % options) children)
            form (-create-form :map-of properties (mapv -form children))
            validate-limits (-validate-limits min max)
            ->parser (fn [f] (let [key-parser (f key-schema)
                                   value-parser (f value-schema)]
                               (fn [x]
                                 (if (map? x)
                                   (reduce-kv (fn [acc k v]
                                                (let [k* (key-parser k)
                                                      v* (value-parser v)]
                                                  ;; OPTIMIZE: Restore `identical?` check + NOOP
                                                  (if (or (miu/-invalid? k*) (miu/-invalid? v*))
                                                    (reduced ::invalid)
                                                    (assoc acc k* v*))))
                                              (empty x) x)
                                   ::invalid))))]
        ^{:type ::schema}
        (reify
          Schema
          (-validator [_]
            (let [key-valid? (-validator key-schema)
                  value-valid? (-validator value-schema)]
              (fn [m]
                (and (map? m)
                     (validate-limits m)
                     (reduce-kv
                       (fn [_ key value]
                         (or (and (key-valid? key) (value-valid? value)) (reduced false)))
                       true m)))))
          (-explainer [this path]
            (let [key-explainer (-explainer key-schema (conj path 0))
                  value-explainer (-explainer value-schema (conj path 1))]
              (fn explain [m in acc]
                (if-not (map? m)
                  (conj acc (miu/-error path in this m ::invalid-type))
                  (if-not (validate-limits m)
                    (conj acc (miu/-error path in this m ::limits))
                    (reduce-kv
                     (fn [acc key value]
                       (let [in (conj in key)]
                         (->> acc
                              (key-explainer key in)
                              (value-explainer value in))))
                     acc m))))))
          (-parser [_] (->parser -parser))
          (-unparser [_] (->parser -unparser))
          (-transformer [this transformer method options]
            (let [this-transformer (-value-transformer transformer this method options)
                  ->key (-transformer key-schema transformer method options)
                  ->child (-transformer value-schema transformer method options)
                  ->key-child (cond
                                (and ->key ->child) #(assoc %1 (->key %2) (->child %3))
                                ->key #(assoc %1 (->key %2) %3)
                                ->child #(assoc %1 %2 (->child %3)))
                  apply->key-child (when ->key-child #(reduce-kv ->key-child (empty %) %))
                  apply->key-child (-guard map? apply->key-child)]
              (-intercepting this-transformer apply->key-child)))
          (-walk [this walker path options]
            (if (-accept walker this path options)
              (-outer walker this path (-inner-indexed walker path children options) options)))
          (-simplify [this] this)
          (-unreachable? [this] false)
          (-properties [_] properties)
          (-options [_] options)
          (-children [_] children)
          (-parent [_] parent)
          (-form [_] form)
          LensSchema
          (-keep [_])
          (-get [_ key default] (get children key default))
          (-set [this key value] (-set-assoc-children this key value)))))))

(defn -collection-schema [?props]
  (let [props* (atom (if (map? ?props) ?props))]
    ^{:type ::into-schema}
    (reify IntoSchema
      (-type [_] (:type @props*))
      (-type-properties [_] (:type-properties @props*))
      (-properties-schema [_ _])
      (-children-schema [_ _])
      (-into-schema [parent {:keys [min max] :as properties} children options]
        (if (fn? ?props)
          (-into-schema (-collection-schema (?props properties children)) properties children options)
          (let [{type :type fpred :pred, fempty :empty, fin :in :or {fin (fn [i _] i)}} ?props]
            (reset! props* ?props)
            (-check-children! type properties children {:min 1 :max 1})
            (let [[schema :as children] (mapv #(schema % options) children)
                  form (-create-form type properties (map -form children))
                  validate-limits (-validate-limits min max)
                  ->parser (fn [f] (let [child-parser (f schema)]
                                     (fn [x]
                                       (cond
                                         (not (fpred x)) ::invalid
                                         (not (validate-limits x)) ::invalid
                                         :else (let [x' (reduce
                                                          (fn [acc v]
                                                            (let [v' (child-parser v)]
                                                              (if (miu/-invalid? v') (reduced ::invalid) (conj acc v'))))
                                                          [] x)]
                                                 (cond
                                                   (miu/-invalid? x') x'
                                                   fempty (into fempty x')
                                                   :else x'))))))]
              ^{:type ::schema}
              (reify
                Schema
                (-validator [_]
                  (let [validator (-validator schema)]
                    (fn [x] (and (fpred x)
                                 (validate-limits x)
                                 (reduce (fn [acc v] (if (validator v) acc (reduced false))) true x)))))
                (-explainer [this path]
                  (let [explainer (-explainer schema (conj path 0))]
                    (fn [x in acc]
                      (cond
                        (not (fpred x)) (conj acc (miu/-error path in this x ::invalid-type))
                        (not (validate-limits x)) (conj acc (miu/-error path in this x ::limits))
                        :else (let [size (count x)]
                                (loop [acc acc, i 0, [x & xs] x]
                                  (if (< i size)
                                    (cond-> (or (explainer x (conj in (fin i x)) acc) acc) xs (recur (inc i) xs))
                                    acc)))))))
                (-parser [_] (->parser -parser))
                (-unparser [_] (->parser -unparser))
                (-transformer [this transformer method options]
                  (let [collection? #(or (sequential? %) (set? %))
                        this-transformer (-value-transformer transformer this method options)
                        child-transformer (-transformer schema transformer method options)
                        ->child (when child-transformer
                                  (if fempty
                                    (-collection-transformer child-transformer fempty)
                                    #(map child-transformer %)))
                        ->child (-guard collection? ->child)]
                    (-intercepting this-transformer ->child)))
                (-walk [this walker path options]
                  (if (-accept walker this path options)
                    (-outer walker this path [(-inner walker schema (conj path ::in) options)] options)))
                (-simplify [this]
                  (if (-unreachable? schema)
                    (if (some-> min pos?)
                      (malli.core/schema :never)
                      (-> this
                          (-set-children [(malli.core/schema :any)])
                          (-update-properties (constantly {:max 0}))))
                    this))
                (-unreachable? [this] false)
                (-properties [_] properties)
                (-options [_] options)
                (-children [_] children)
                (-parent [_] parent)
                (-form [_] form)
                LensSchema
                (-keep [_] true)
                (-get [_ _ _] schema)
                (-set [this _ value] (-set-children this [value]))))))))))

(defn -tuple-schema []
  ^{:type ::into-schema}
  (reify IntoSchema
    (-type [_] :tuple)
    (-type-properties [_])
    (-properties-schema [_ _])
    (-children-schema [_ _])
    (-into-schema [parent properties children options]
      (let [children (mapv #(schema % options) children)
            size (count children)
            form (-create-form :tuple properties (map -form children))
            ->parser (fn [f] (let [parsers (into {} (comp (map f) (map-indexed vector)) children)]
                               (fn [x]
                                 (cond
                                   (not (vector? x)) ::invalid
                                   (not= (count x) size) ::invalid
                                   :else (reduce-kv (fn [x i c]
                                                      (let [v (get x i)
                                                            v* (c v)]
                                                        (cond
                                                          (miu/-invalid? v*) (reduced v*)
                                                          (identical? v* v) x
                                                          :else (assoc x i v*))))
                                                    x parsers)))))]
        (-check-children! :tuple properties children {:min 0})
        ^{:type ::schema}
        (reify
          Schema
          (-validator [_]
            (let [validators (into (array-map) (map-indexed vector (mapv -validator children)))]
              (fn [x] (and (vector? x)
                           (= (count x) size)
                           (reduce-kv
                             (fn [acc i validator]
                               (if (validator (nth x i)) acc (reduced false))) true validators)))))
          (-explainer [this path]
            (let [explainers (mapv (fn [[i s]] (-explainer s (conj path i))) (map-indexed vector children))]
              (fn [x in acc]
                (cond
                  (not (vector? x)) (conj acc (miu/-error path in this x ::invalid-type))
                  (not= (count x) size) (conj acc (miu/-error path in this x ::tuple-size))
                  :else (loop [acc acc, i 0, [x & xs] x, [e & es] explainers]
                          (cond-> (e x (conj in i) acc) xs (recur (inc i) xs es)))))))
          (-parser [_] (->parser -parser))
          (-unparser [_] (->parser -unparser))
          (-transformer [this transformer method options]
            (let [this-transformer (-value-transformer transformer this method options)
                  ->children (into {} (comp (map-indexed vector)
                                            (keep (fn [[k c]]
                                                    (when-some [t (-transformer c transformer method options)]
                                                      [k t])))) children)
                  apply->children (when (seq ->children) (-tuple-transformer ->children))
                  apply->children (-guard vector? apply->children)]
              (-intercepting this-transformer apply->children)))
          (-walk [this walker path options]
            (if (-accept walker this path options)
              (-outer walker this path (-inner-indexed walker path children options) options)))
          (-simplify [this] (if (-unreachable? this)
                              (schema :never)
                              this))
          (-unreachable? [this] (boolean (some -unreachable? children)))
          (-properties [_] properties)
          (-options [_] options)
          (-children [_] children)
          (-parent [_] parent)
          (-form [_] form)
          LensSchema
          (-keep [_] true)
          (-get [_ key default] (get children key default))
          (-set [this key value] (-set-assoc-children this key value)))))))

(defn -enum-schema []
  ^{:type ::into-schema}
  (reify IntoSchema
    (-type [_] :enum)
    (-type-properties [_])
    (-into-schema [parent properties children options]
      (-check-children! :enum properties children {:min 1})
      (let [children (vec children)
            schema (set children)
            form (-create-form :enum properties children)]
        ^{:type ::schema}
        (reify
          Schema
          (-validator [_]
            (fn [x] (contains? schema x)))
          (-explainer [this path]
            (fn explain [x in acc]
              (if-not (contains? schema x) (conj acc (miu/-error (conj path 0) in this x)) acc)))
          (-parser [_] (fn [x] (if (contains? schema x) x ::invalid)))
          (-unparser [this] (-parser this))
          ;; TODO: should we try to derive the type from values? e.g. [:enum 1 2] ~> int?
          (-transformer [this transformer method options]
            (-intercepting (-value-transformer transformer this method options)))
          (-walk [this walker path options]
            (if (-accept walker this path options)
              (-outer walker this path children options)))
          (-simplify [this] this)
          (-unreachable? [this] false)
          (-properties [_] properties)
          (-options [_] options)
          (-children [_] children)
          (-parent [_] parent)
          (-form [_] form)
          LensSchema
          (-keep [_])
          (-get [_ key default] (get children key default))
          (-set [this key value] (-set-assoc-children this key value)))))))

(defn -re-schema [class?]
  ^{:type ::into-schema}
  (reify IntoSchema
    (-type [_] :re)
    (-type-properties [_])
    (-properties-schema [_ _])
    (-children-schema [_ _])
    (-into-schema [parent properties [child :as children] options]
      (-check-children! :re properties children {:min 1, :max 1})
      (let [children (vec children)
            re (re-pattern child)
            form (if class? re (-create-form :re properties children))]
        ^{:type ::schema}
        (reify
          Schema
          (-validator [_]
            (-safe-pred #(re-find re %)))
          (-explainer [this path]
            (fn explain [x in acc]
              (try
                (if-not (re-find re x)
                  (conj acc (miu/-error path in this x))
                  acc)
                (catch #?(:clj Exception, :cljs js/Error) e
                  (conj acc (miu/-error path in this x (:type (ex-data e))))))))
          (-transformer [this transformer method options]
            (-intercepting (-value-transformer transformer this method options)))
          (-parser [_]
            (let [find (-safe-pred #(re-find re %))]
              (fn [x] (if (find x) x ::invalid))))
          (-unparser [this] (-parser this))
          (-walk [this walker path options]
            (if (-accept walker this path options)
              (-outer walker this path children options)))
          (-simplify [this] this)
          (-unreachable? [this] false)
          (-properties [_] properties)
          (-options [_] options)
          (-children [_] children)
          (-parent [_] parent)
          (-form [_] form)
          LensSchema
          (-keep [_])
          (-get [_ key default] (get children key default))
          (-set [this key value] (-set-assoc-children this key value)))))))

(defn -fn-schema []
  ^{:type ::into-schema}
  (reify IntoSchema
    (-type [_] :fn)
    (-type-properties [_])
    (-into-schema [parent properties children options]
      (-check-children! :fn properties children {:min 1, :max 1})
      (let [children (vec children)
            f (eval (first children) options)
            form (-create-form :fn properties children)]
        ^{:type ::schema}
        (reify
          Schema
          (-validator [_] (-safe-pred f))
          (-explainer [this path]
            (fn explain [x in acc]
              (try
                (if-not (f x)
                  (conj acc (miu/-error path in this x))
                  acc)
                (catch #?(:clj Exception, :cljs js/Error) e
                  (conj acc (miu/-error path in this x (:type (ex-data e))))))))
          (-parser [this]
            (let [validator (-validator this)]
              (fn [x] (if (validator x) x ::invalid))))
          (-unparser [this] (-parser this))
          (-transformer [this transformer method options]
            (-intercepting (-value-transformer transformer this method options)))
          (-walk [this walker path options]
            (if (-accept walker this path options)
              (-outer walker this path children options)))
          (-simplify [this] this)
          (-unreachable? [this] false)
          (-properties [_] properties)
          (-options [_] options)
          (-children [_] children)
          (-parent [_] parent)
          (-form [_] form)
          LensSchema
          (-keep [_])
          (-get [_ key default] (get children key default))
          (-set [this key value] (-set-assoc-children this key value)))))))

(defn -maybe-schema []
  ^{:type ::into-schema}
  (reify IntoSchema
    (-type [_] :maybe)
    (-type-properties [_])
    (-properties-schema [_ _])
    (-children-schema [_ _])
    (-into-schema [parent properties children options]
      (-check-children! :maybe properties children {:min 1, :max 1})
      (let [[schema :as children] (map #(schema % options) children)
            form (-create-form :maybe properties (map -form children))
            ->parser (fn [f] (let [parser (f schema)]
                               (fn [x] (if (nil? x) x (parser x)))))]
        ^{:type ::schema}
        (reify
          Schema
          (-validator [_]
            (let [validator' (-validator schema)]
              (fn [x] (or (nil? x) (validator' x)))))
          (-explainer [_ path]
            (let [explainer' (-explainer schema (conj path 0))]
              (fn explain [x in acc]
                (if (nil? x) acc (explainer' x in acc)))))
          (-parser [_] (->parser -parser))
          (-unparser [_] (->parser -unparser))
          (-transformer [this transformer method options]
            (-parent-children-transformer this children transformer method options))
          (-walk [this walker path options]
            (if (-accept walker this path options)
              (-outer walker this path (-inner-indexed walker path children options) options)))
          (-simplify [this]
            (if (-unreachable? schema)
              (malli.core/schema :nil) ;; same as simplifying [:or :nil :never]
              (if (= :maybe (type schema))
                schema ;; [:maybe [:maybe s]] => [:maybe s]
                this)))
          (-unreachable? [this] false)
          (-properties [_] properties)
          (-options [_] options)
          (-children [_] children)
          (-parent [_] parent)
          (-form [_] form)
          LensSchema
          (-keep [_])
          (-get [_ key default] (if (= 0 key) schema default))
          (-set [this key value] (if (= 0 key)
                                   (-set-children this [value])
                                   (-fail! ::index-out-of-bounds {:schema this, :key key}))))))))

(defn -multi-schema
  ([]
   (-multi-schema {:naked-keys true}))
  ([opts]
   ^{:type ::into-schema}
   (reify IntoSchema
     (-type [_] :multi)
     (-type-properties [_] (:type-properties opts))
     (-properties-schema [_ _])
     (-children-schema [_ _])
     (-into-schema [parent properties children options]
       (let [type (or (:type opts) :multi)
             opts' (merge opts (select-keys properties [:lazy-refs]))
             {:keys [children entries forms]} (-parse-entries children opts' options)
             form (-create-form type properties forms)
             dispatch (eval (:dispatch properties) options)
             dispatch-map (->> (for [[k s] entries] [k s]) (into {}))
             finder (fn [{:keys [::default] :as m}] (fn [x] (m x default)))]
         (when-not dispatch
           (-fail! ::missing-property {:key :dispatch}))
         ^{:type ::schema}
         (reify
           Schema
           (-validator [_]
             (let [find (finder (reduce-kv (fn [acc k s] (assoc acc k (-validator s))) {} dispatch-map))]
               (fn [x] (if-let [validator (find (dispatch x))] (validator x) false))))
           (-explainer [this path]
             (let [find (finder (reduce (fn [acc [k s]] (assoc acc k (-explainer s (conj path k)))) {} entries))
                   ->path (if (keyword? dispatch) #(conj % dispatch) identity)]
               (fn [x in acc]
                 (if-let [explainer (find (dispatch x))]
                   (explainer x in acc)
                   (conj acc (miu/-error (->path path) (->path in) this x ::invalid-dispatch-value))))))
           (-parser [_]
             (let [parse (fn [k s] (let [p (-parser s)] (fn [x] (miu/-map-valid #(miu/-tagged k %) (p x)))))
                   find (finder (reduce-kv (fn [acc k s] (assoc acc k (parse k s))) {} dispatch-map))]
               (fn [x] (if-some [parser (find (dispatch x))] (parser x) ::invalid))))
           (-unparser [_]
             (let [unparsers (reduce-kv (fn [acc k s] (assoc acc k (-unparser s))) {} dispatch-map)]
               (fn [x] (if (miu/-tagged? x) (if-some [f (unparsers (key x))] (f (val x)) ::invalid) ::invalid))))
           (-transformer [this transformer method options]
             ;; FIXME: Probably should not use `dispatch`
             ;; Can't use `dispatch` as `x` might not be valid before it has been unparsed:
             (let [this-transformer (-value-transformer transformer this method options)
                   ->children (reduce-kv (fn [acc k s] (let [t (-transformer s transformer method options)]
                                                         (cond-> acc t (assoc k t)))) {} dispatch-map)
                   find (finder ->children)
                   child-transformer (if (seq ->children) (fn [x] (if-some [t (find (dispatch x))] (t x) x)))]
               (-intercepting this-transformer child-transformer)))
           (-walk [this walker path options]
             (if (-accept walker this path options)
               (-outer walker this path (-inner-entries walker path entries options) options)))
           
           (-simplify [this] this) ;;TODO
           (-unreachable? [this] false) ;;TODO
           (-properties [_] properties)
           (-options [_] options)
           (-children [_] children)
           (-parent [_] parent)
           (-form [_] form)
           MapSchema
           (-entries [_] entries)
           LensSchema
           (-keep [_])
           (-get [this key default] (-get-entries this key default))
           (-set [this key value] (-set-entries this key value))))))))

(defn -ref-schema
  ([]
   (-ref-schema nil))
  ([{:keys [lazy type-properties] :as opts}]
   ^{:type ::into-schema}
   (reify IntoSchema
     (-type [_] :ref)
     (-type-properties [_] type-properties)
     (-into-schema [parent properties [ref :as children] {::keys [allow-invalid-refs] :as options}]
       (-check-children! :ref properties children {:min 1, :max 1})
       (when-not (-reference? ref)
         (-fail! ::invalid-ref {:ref ref}))
       (let [-ref (or (and lazy (-memoize (fn [] (schema (mr/-schema (-registry options) ref) options))))
                      (if-let [s (mr/-schema (-registry options) ref)] (-memoize (fn [] (schema s options))))
                      (when-not allow-invalid-refs
                        (-fail! ::invalid-ref {:type :ref, :ref ref})))
             children (vec children)
             form (-create-form :ref properties children)
             ->parser (fn [f] (let [parser (-memoize (fn [] (f (-ref))))]
                                (fn [x] ((parser) x))))]
         ^{:type ::schema}
         (reify
           Schema
           (-validator [_]
             (let [validator (-memoize (fn [] (-validator (-ref))))]
               (fn [x] ((validator) x))))
           (-explainer [_ path]
             (let [explainer (-memoize (fn [] (-explainer (-ref) (conj path 0))))]
               (fn [x in acc] ((explainer) x in acc))))
           (-parser [_] (->parser -parser))
           (-unparser [_] (->parser -unparser))
           (-transformer [this transformer method options]
             (let [this-transformer (-value-transformer transformer this method options)
                   deref-transformer (-memoize (fn [] (-transformer (-ref) transformer method options)))]
               (-intercepting this-transformer (fn [x] (if-some [t (deref-transformer)] (t x) x)))))
           (-walk [this walker path options]
             (let [accept (fn [] (-inner walker (-ref) (into path [0 0])
                                         (-update options ::walked-refs #(conj (or % #{}) ref))))]
               (if (-accept walker this path options)
                 (if (or (not ((-boolean-fn (::walk-refs options false)) ref))
                         (contains? (::walked-refs options) ref))
                   (-outer walker this path [ref] options)
                   (-outer walker this path [(accept)] options)))))
           (-simplify [this] this)
           (-unreachable? [this] false)
           (-properties [_] properties)
           (-options [_] options)
           (-children [_] children)
           (-parent [_] parent)
           (-form [_] form)
           LensSchema
           (-get [_ key default] (if (= key 0) (-pointer ref (-ref) options) default))
           (-keep [_])
           (-set [this key value] (if (= key 0) (-set-children this [value])
                                                (-fail! ::index-out-of-bounds {:schema this, :key key})))
           RefSchema
           (-ref [_] ref)
           (-deref [_] (-ref))
           RegexSchema
           (-regex-op? [_] false)
           (-regex-validator [this] (-fail! ::potentially-recursive-seqex this))
           (-regex-explainer [this _] (-fail! ::potentially-recursive-seqex this))
           (-regex-parser [this] (-fail! ::potentially-recursive-seqex this))
           (-regex-unparser [this] (-fail! ::potentially-recursive-seqex this))
           (-regex-transformer [this _ _ _] (-fail! ::potentially-recursive-seqex this))
           (-regex-min-max [this] (-fail! ::potentially-recursive-seqex this))))))))

(defn -schema-schema [{:keys [id raw] :as opts}]
  ^{:type ::into-schema}
  (let [internal? (or id raw)
        type (if internal? ::schema :schema)]
    (reify IntoSchema
      (-type [_] type)
      (-type-properties [_])
      (-properties-schema [_ _])
      (-children-schema [_ _])
      (-into-schema [parent properties children options]
        (-check-children! type properties children {:min 1, :max 1})
        (let [[child :as children] (map #(schema % options) children)
              form (or (and (empty? properties) (or id (and raw (-form child))))
                       (-create-form type properties [(-form child)]))]
          ^{:type ::schema}
          (reify
            Schema
            (-validator [_] (-validator child))
            (-explainer [_ path] (-explainer child path))
            (-parser [_] (-parser child))
            (-unparser [_] (-unparser child))
            (-transformer [this transformer method options]
              (-parent-children-transformer this children transformer method options))
            (-walk [this walker path options]
              (if (-accept walker this path options)
                (if (or (not id) ((-boolean-fn (::walk-schema-refs options false)) id))
                  (-outer walker this path (-inner-indexed walker path children options) options)
                  (-outer walker this path [id] options))))
            (-simplify [this] (if (-unreachable? this)
                                (schema :never)
                                this))
            (-unreachable? [this] (-unreachable? child))
            (-properties [_] properties)
            (-options [_] options)
            (-children [_] children)
            (-parent [_] parent)
            (-form [_] form)
            LensSchema
            (-keep [_])
            (-get [_ key default] (if (= key 0) child default))
            (-set [this key value] (if (= key 0) (-set-children this [value])
                                                 (-fail! ::index-out-of-bounds {:schema this, :key key})))
            RefSchema
            (-ref [_] id)
            (-deref [_] child)

            RegexSchema
            (-regex-op? [_] false)
            (-regex-validator [_]
              (if internal?
                (-regex-validator child)
                (re/item-validator (-validator child))))
            (-regex-explainer [_ path]
              (if internal?
                (-regex-explainer child path)
                (re/item-explainer path child (-explainer child path))))
            (-regex-parser [_]
              (if internal?
                (-regex-parser child)
                (re/item-parser (parser child))))
            (-regex-unparser [_]
              (if internal?
                (-regex-unparser child)
                (re/item-unparser (unparser child))))
            (-regex-transformer [_ transformer method options]
              (if internal?
                (-regex-transformer child transformer method options)
                (re/item-transformer method (-validator child)
                                     (or (-transformer child transformer method options) identity))))
            (-regex-min-max [_] (-regex-min-max child))))))))

(defn -=>-schema []
  ^{:type ::into-schema}
  (reify IntoSchema
    (-type [_] :=>)
    (-type-properties [_])
    (-into-schema [parent properties children {::keys [function-checker] :as options}]
      (-check-children! :=> properties children {:min 2, :max 2})
      (let [[input :as children] (map #(schema % options) children)
            form (-create-form :=> properties (map -form children))
            ->checker (if function-checker #(function-checker % options) (constantly nil))]
        (when-not (#{:cat :catn} (type input))
          (-fail! ::invalid-input-schema {:input input}))
        ^{:type ::schema}
        (reify
          Schema
          (-validator [this]
            (if-let [checker (->checker this)]
              (let [validator (fn [x] (nil? (checker x)))]
                (fn [x] (and (ifn? x) (validator x)))) ifn?))
          (-explainer [this path]
            (if-let [checker (->checker this)]
              (fn explain [x in acc]
                (if (not (fn? x))
                  (conj acc (miu/-error path in this x))
                  (if-let [res (checker x)]
                    (conj acc (assoc (miu/-error path in this x) :check res))
                    acc)))
              (let [validator (-validator this)]
                (fn explain [x in acc]
                  (if-not (validator x) (conj acc (miu/-error path in this x)) acc)))))
          (-parser [this]
            (let [validator (-validator this)]
              (fn [x] (if (validator x) x ::invalid))))
          (-unparser [this] (-parser this))
          (-transformer [_ _ _ _])
          (-walk [this walker path options]
            (if (-accept walker this path options)
              (-outer walker this path (-inner-indexed walker path children options) options)))
          (-simplify [this] this)
          (-unreachable? [this] false)
          (-properties [_] properties)
          (-options [_] options)
          (-children [_] children)
          (-parent [_] parent)
          (-form [_] form)
          LensSchema
          (-keep [_])
          (-get [_ key default] (get children key default))
          (-set [this key value] (-set-assoc-children this key value)))))))

(defn -function-schema [_]
  ^{:type ::into-schema}
  (reify IntoSchema
    (-type [_] :function)
    (-type-properties [_])
    (-properties-schema [_ _])
    (-children-schema [_ _])
    (-into-schema [parent properties children {::keys [function-checker] :as options}]
      (-check-children! :function properties children {:min 1})
      (let [children (map #(schema % options) children)
            form (-create-form :function properties (map -form children))
            ->checker (if function-checker #(function-checker % options) (constantly nil))]
        (when-not (every? #(= :=> (type %)) children)
          (-fail! ::non-function-childs {:children children}))
        (let [infos (map -function-info children)]
          (when-not (= (count children) (count (distinct (map :arity infos))))
            (-fail! ::duplicate-arities {:infos infos}))
          (when-not (= (count children) (count (distinct (map :min infos))))
            (-fail! ::duplicate-min-arities {:infos infos})))
        ^{:type ::schema}
        (reify
          Schema
          (-validator [this]
            (if-let [checker (->checker this)]
              (let [validator (fn [x] (nil? (checker x)))]
                (fn [x] (and (ifn? x) (validator x)))) ifn?))
          (-explainer [this path]
            (if-let [checker (->checker this)]
              (fn explain [x in acc]
                (if (not (fn? x))
                  (conj acc (miu/-error path in this x))
                  (if-let [res (checker x)]
                    (conj acc (assoc (miu/-error path in this x) :check res))
                    acc)))
              (let [validator (-validator this)]
                (fn explain [x in acc]
                  (if-not (validator x) (conj acc (miu/-error path in this x)) acc)))))
          (-parser [this]
            (let [validator (-validator this)]
              (fn [x] (if (validator x) x ::invalid))))
          (-unparser [this] (-parser this))
          (-transformer [_ _ _ _])
          (-walk [this walker path options]
            (if (-accept walker this path options)
              (-outer walker this path (-inner-indexed walker path children options) options)))
          (-simplify [this] this)
          (-unreachable? [this] false)
          (-properties [_] properties)
          (-options [_] options)
          (-children [_] children)
          (-parent [_] parent)
          (-form [_] form)
          LensSchema
          (-keep [_])
          (-get [this key default] (-get-entries this key default))
          (-set [this key value] (-set-entries this key value)))))))

(defn- regex-validator [schema] (re/validator (-regex-validator schema)))

(defn- regex-explainer [schema path] (re/explainer schema path (-regex-explainer schema path)))

(defn- regex-parser [schema] (re/parser (-regex-parser schema)))

(defn- regex-transformer [schema transformer method options]
  (let [this-transformer (-value-transformer transformer schema method options)
        ->children (re/transformer (-regex-transformer schema transformer method options))]
    (-intercepting this-transformer ->children)))

(defn -sequence-schema
  [{:keys [type child-bounds re-validator re-explainer re-parser re-unparser re-transformer re-min-max] :as opts}]
  ^{:type ::into-schema}
  (reify IntoSchema
    (-type [_] type)
    (-type-properties [_])
    (-properties-schema [_ _])
    (-children-schema [_ _])
    (-into-schema [parent properties children options]
      (-check-children! type properties children child-bounds)
      (let [children (mapv #(schema % options) children)
            form (-create-form type properties (mapv -form children))]
        ^{:type ::schema}
        (reify
          Schema
          (-validator [this] (regex-validator this))
          (-explainer [this path] (regex-explainer this path))
          (-parser [this] (regex-parser this))
          (-unparser [this] (-regex-unparser this))
          (-transformer [this transformer method options] (regex-transformer this transformer method options))
          (-walk [this walker path options]
            (if (-accept walker this path options)
              (-outer walker this path (-inner-indexed walker path children options) options)))
          (-simplify [this] this)
          (-unreachable? [this] false)
          (-properties [_] properties)
          (-options [_] options)
          (-children [_] children)
          (-parent [_] parent)
          (-form [_] form)

          LensSchema
          (-keep [_] true)
          (-get [_ key default] (get children key default))
          (-set [this key value] (-set-assoc-children this key value))

          RegexSchema
          (-regex-op? [_] true)
          (-regex-validator [_] (re-validator properties (map -regex-validator children)))
          (-regex-explainer [_ path]
            (re-explainer properties (map-indexed (fn [i child] (-regex-explainer child (conj path i))) children)))
          (-regex-parser [_] (re-parser properties (map -regex-parser children)))
          (-regex-unparser [_] (re-unparser properties (map -regex-unparser children)))
          (-regex-transformer [_ transformer method options]
            (re-transformer properties (map #(-regex-transformer % transformer method options) children)))
          (-regex-min-max [_] (re-min-max properties children)))))))

(defn -sequence-entry-schema
  [{:keys [type child-bounds re-validator re-explainer re-parser re-unparser re-transformer re-min-max] :as opts}]
  ^{:type ::into-schema}
  (reify IntoSchema
    (-type [_] type)
    (-type-properties [_])
    (-properties-schema [_ _])
    (-children-schema [_ _])
    (-into-schema [parent properties children options]
      (-check-children! type properties children child-bounds)
      (let [{:keys [children entries forms]} (-parse-entries children opts options)
            form (-create-form type properties forms)]
        ^{:type ::schema}
        (reify
          Schema
          (-validator [this] (regex-validator this))
          (-explainer [this path] (regex-explainer this path))
          (-parser [this] (regex-parser this))
          (-unparser [this] (-regex-unparser this))
          (-transformer [this transformer method options] (regex-transformer this transformer method options))
          (-walk [this walker path options]
            (if (-accept walker this path options)
              (-outer walker this path (-inner-entries walker path entries options) options)))
          (-simplify [this] this) ;;TODO ?
          (-unreachable? [this] false)
          (-properties [_] properties)
          (-options [_] options)
          (-children [_] children)
          (-parent [_] parent)
          (-form [_] form)

          LensSchema
          (-keep [_] true)
          (-get [this key default] (-get-entries this key default))
          (-set [this key value] (-set-entries this key value))

          RegexSchema
          (-regex-op? [_] true)
          (-regex-validator [_] (re-validator properties (map (fn [[k _ s]] [k (-regex-validator s)]) children)))
          (-regex-explainer [_ path]
            (re-explainer properties (map (fn [[k _ s]] [k (-regex-explainer s (conj path k))]) children)))
          (-regex-parser [_] (re-parser properties (map (fn [[k _ s]] [k (-regex-parser s)]) children)))
          (-regex-unparser [_] (re-unparser properties (map (fn [[k _ s]] [k (-regex-unparser s)]) children)))
          (-regex-transformer [_ transformer method options]
            (re-transformer properties (map (fn [[k _ s]] [k (-regex-transformer s transformer method options)])
                                            children)))
          (-regex-min-max [_] (re-min-max properties children)))))))

;;
;; public api
;;

;;
;; into-schema
;;

(defn into-schema?
  "Checks if x is a IntoSchema instance"
  [x] (-into-schema? x))

(defn into-schema
  "Creates a Schema instance out of type, optional properties map and children"
  ([type properties children]
   (into-schema type properties children nil))
  ([type properties children options]
   (let [[properties options] (-properties-and-options properties options -form)]
     (-into-schema (-schema type options) (if (seq properties) properties) children options))))

(defn type
  "Returns the Schema type."
  ([?schema]
   (type ?schema nil))
  ([?schema options]
   (-type (-parent (schema ?schema options)))))

(defn type-properties
  "Returns the Schema type properties"
  ([?schema]
   (type-properties ?schema nil))
  ([?schema options]
   (-type-properties (-parent (schema ?schema options)))))

(defn properties-schema
  "Returns properties schema for Schema or IntoSchema."
  ([?schema]
   (properties-schema ?schema nil))
  ([?schema options]
   (if (into-schema? ?schema)
     (some-> ?schema (-properties-schema options) schema)
     (some-> (schema ?schema options) -parent (-properties-schema options)))))

(defn children-schema
  "Returns children schema for Schema or IntoSchema."
  ([?schema]
   (children-schema ?schema nil))
  ([?schema options]
   (if (into-schema? ?schema)
     (some-> ?schema (-children-schema options) schema)
     (some-> (schema ?schema options) -parent (-children-schema options)))))

;;
;; schema
;;

(defn schema?
  "Checks if x is a Schema instance"
  [x] (-schema? x))

(defn schema
  "Creates a Schema object from any of the following:

   - Schema instance (just returns it)
   - IntoSchema instance
   - Schema vector syntax, e.g. [:string {:min 1}]
   - Qualified Keyword or String, using a registry lookup"
  ([?schema]
   (schema ?schema nil))
  ([?schema options]
   (cond
     (schema? ?schema) ?schema
     (into-schema? ?schema) (-into-schema ?schema nil nil options)
     (vector? ?schema) (let [[p c] (-properties-and-children (rest ?schema))]
                         (into-schema (-schema (first ?schema) options) p c options))
     :else (if-let [?schema' (and (-reference? ?schema) (-lookup ?schema options))]
             (-pointer ?schema (schema ?schema' options) options)
             (-> ?schema (-schema options) (schema options))))))

(defn form
  "Returns the Schema form"
  ([?schema]
   (form ?schema nil))
  ([?schema options]
   (-form (schema ?schema options))))

(defn properties
  "Returns the Schema properties"
  ([?schema]
   (properties ?schema nil))
  ([?schema options]
   (-properties (schema ?schema options))))

(defn options
  "Returns options used in creating the Schema"
  ([?schema]
   (options ?schema nil))
  ([?schema options]
   (-options (schema ?schema options))))

(defn children
  "Returns the Schema children with all Child Schemas resolved. For
  `MapEntry` Schemas, returns a always tuple3 of `key ?properties child`"
  ([?schema]
   (children ?schema nil))
  ([?schema options]
   (let [schema (schema ?schema options)]
     (-children schema))))

(defn parent
  "Returns the IntoSchema instance that created the Schema"
  ([?schema]
   (parent ?schema nil))
  ([?schema options]
   (-parent (schema ?schema options))))

(defn walk
  "Postwalks recursively over the Schema and its children.
   The walker callback is a arity4 function with the following
   arguments: schema, path, (walked) children, and options."
  ([?schema f]
   (walk ?schema f nil))
  ([?schema f options]
   (-walk
     (schema ?schema options)
     (reify Walker
       (-accept [_ s _ _] s)
       (-inner [this s p options] (-walk s this p options))
       (-outer [_ s p c options] (f s p c options)))
     [] options)))

(defn validator
  "Returns an pure validation function of type `x -> boolean` for a given Schema"
  ([?schema]
   (validator ?schema nil))
  ([?schema options]
   (-validator (schema ?schema options))))

(defn validate
  "Validates a value againsta a given schema. Creates the `validator` for every call.
   When performance matters, (re-)use `validator` instead."
  ([?schema value]
   (validate ?schema value nil))
  ([?schema value options]
   ((validator ?schema options) value)))

(defn explainer
  "Returns an pure explainer function of type `x -> explanation` for a given Schema"
  ([?schema]
   (explainer ?schema nil))
  ([?schema options]
   (let [schema' (schema ?schema options)
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
  "Explains a value against a given schema. Creates the `explainer` for every call.
   When performance matters, (re-)use `explainer` instead."
  ([?schema value]
   (explain ?schema value nil))
  ([?schema value options]
   ((explainer ?schema options) value [] [])))

(defn parser
  "Returns an pure parser function of type `x -> either parsed-x ::invalid` for a given Schema"
  ([?schema]
   (parser ?schema nil))
  ([?schema options]
   (-parser (schema ?schema options))))

(defn parse
  "parses a value against a given schema. Creates the `parser` for every call.
   When performance matters, (re-)use `parser` instead."
  ([?schema value]
   (parse ?schema value nil))
  ([?schema value options]
   ((parser ?schema options) value)))

(defn unparser
  "Returns an pure unparser function of type `parsed-x -> either x ::invalid` for a given Schema"
  ([?schema]
   (unparser ?schema nil))
  ([?schema options]
   (-unparser (schema ?schema options))))

(defn unparse
  "Unparses a value against a given schema. Creates the `unparser` for every call.
   When performance matters, (re-)use `unparser` instead."
  ([?schema value]
   (unparse ?schema value nil))
  ([?schema value options]
   ((unparser ?schema options) value)))

(defn decoder
  "Creates a value decoding function given a transformer and a schema."
  ([?schema t]
   (decoder ?schema nil t))
  ([?schema options t]
   (or (-transformer (schema ?schema options) (-into-transformer t) :decode options)
       identity)))

(defn decode
  "Transforms a value with a given decoding transformer against a schema."
  ([?schema value t]
   (decode ?schema value nil t))
  ([?schema value options t]
   (if-let [transform (decoder ?schema options t)]
     (transform value)
     value)))

(defn encoder
  "Creates a value encoding transformer given a transformer and a schema."
  ([?schema t]
   (encoder ?schema nil t))
  ([?schema options t]
   (or (-transformer (schema ?schema options) (-into-transformer t) :encode options)
       identity)))

(defn encode
  "Transforms a value with a given encoding transformer against a schema."
  ([?schema value t]
   (encode ?schema value nil t))
  ([?schema value options t]
   (if-let [transform (encoder ?schema options t)]
     (transform value)
     value)))

(defn entries
  "Returns `MapSchema` children as a sequence of `clojure.lang/MapEntry`s
   where the values child schemas wrapped in `:malli.core/val` Schemas,
   with the entry properties as properties.

   Using `entries` enable usage of entry properties in walking and value
   transformation.

      (def schema
        [:map
         [:x int?]
         [:y {:optional true} int?]])

      (m/children schema)
      ; [[:x nil int?]
      ;  [:y {:optional true} int?]]

      (m/entries schema)
      ; [[:x [:malli.core/val int?]]
      ;  [:y [:malli.core/val {:optional true} int?]]]

      (map key (m/entries schema))
      ; (:x :y)"
  ([?schema]
   (entries ?schema nil))
  ([?schema options]
   (if-let [schema (schema ?schema options)]
     (if (satisfies? MapSchema schema)
       (-entries schema)))))

(defn deref
  "Derefs top-level `RefSchema`s or returns original Schema."
  ([?schema]
   (deref ?schema nil))
  ([?schema options]
   (let [schema (schema ?schema options)]
     (cond-> schema (satisfies? RefSchema schema) (-deref)))))

(defn deref-all
  "Derefs top-level `RefSchema`s recursively or returns original Schema."
  ([?schema]
   (deref-all ?schema nil))
  ([?schema options]
   (let [schema (deref ?schema options)]
     (cond-> schema (satisfies? RefSchema schema) (recur options)))))

;;
;; eval
;;

(defn -default-sci-options []
  {:preset :termination-safe
   :namespaces {'malli.core {'properties properties
                             'type type
                             'children children
                             'entries entries}}})

(let [-fail! #(-fail! ::sci-not-available {:code %})
      -eval? #(or (symbol? %) (string? %) (sequential? %))
      -evaluator (memoize ms/evaluator)]
  (defn eval
    ([?code] (eval ?code nil))
    ([?code options]
     (cond (vector? ?code) ?code
           (-eval? ?code) (if (::disable-sci options)
                            (-fail! ?code)
                            (((-evaluator (or (::sci-options options) (-default-sci-options)) -fail!)) ?code))
           :else ?code))))

;;
;; schema walker
;;

(defn schema-walker [f]
  (fn [schema _ children _]
    (f (-set-children schema children))))

;;
;; registry
;;

(defn predicate-schemas []
  (->> [#'any? #'some? #'number? #'integer? #'int? #'pos-int? #'neg-int? #'nat-int? #'pos? #'neg? #'float? #'double?
        #'boolean? #'string? #'ident? #'simple-ident? #'qualified-ident? #'keyword? #'simple-keyword?
        #'qualified-keyword? #'symbol? #'simple-symbol? #'qualified-symbol? #'uuid? #'uri? #?(:clj #'decimal?)
        #'inst? #'seqable? #'indexed? #'map? #'vector? #'list? #'seq? #'char? #'set? #'nil? #'false? #'true?
        #'zero? #?(:clj #'rational?) #'coll? #'empty? #'associative? #'sequential? #?(:clj #'ratio?) #?(:clj #'bytes?)
        #'ifn? #'fn?]
       (reduce -register-var {})))

(defn class-schemas []
  {#?(:clj Pattern, :cljs js/RegExp) (-re-schema true)})

(defn comparator-schemas []
  (->> {:> >, :>= >=, :< <, :<= <=, := =, :not= not=}
       (map (fn [[k v]] [k (-simple-schema (fn [_ [child]] {:type k, :pred (-safe-pred #(v % child)), :min 1, :max 1}))]))
       (into {}) (reduce-kv assoc nil)))

(defn type-schemas []
  {:any (-any-schema)
   :never (-never-schema)
   :nil (-nil-schema)
   :string (-string-schema)
   :int (-int-schema)
   :double (-double-schema)
   :boolean (-boolean-schema)
   :keyword (-keyword-schema)
   :symbol (-symbol-schema)
   :qualified-keyword (-qualified-keyword-schema)
   :qualified-symbol (-qualified-symbol-schema)
   :uuid (-uuid-schema)})

(defn- -re-min-max [f {min' :min, max' :max} child]
  (let [{min'' :min max'' :max} (-regex-min-max child)]
    (cond-> {:min (f (or min' 0) min'')} (and max' max'') (assoc :max (f max' max'')))))

(defn- -re-alt-min-max [{min' :min, max' :max} child]
  (let [{min'' :min max'' :max} (-regex-min-max child)]
    (cond-> {:min (min (or min' miu/+max-size+) min'')} (and max' max'') (assoc :max (max max' max'')))))

(defn sequence-schemas []
  {:+ (-sequence-schema {:type :+, :child-bounds {:min 1, :max 1}
                         :re-validator (fn [_ [child]] (re/+-validator child))
                         :re-explainer (fn [_ [child]] (re/+-explainer child))
                         :re-parser (fn [_ [child]] (re/+-parser child))
                         :re-unparser (fn [_ [child]] (re/+-unparser child))
                         :re-transformer (fn [_ [child]] (re/+-transformer child))
                         :re-min-max (fn [_ [child]] {:min (:min (-regex-min-max child))})})
   :* (-sequence-schema {:type :*, :child-bounds {:min 1, :max 1}
                         :re-validator (fn [_ [child]] (re/*-validator child))
                         :re-explainer (fn [_ [child]] (re/*-explainer child))
                         :re-parser (fn [_ [child]] (re/*-parser child))
                         :re-unparser (fn [_ [child]] (re/*-unparser child))
                         :re-transformer (fn [_ [child]] (re/*-transformer child))
                         :re-min-max (fn [_ _] {:min 0})})
   :? (-sequence-schema {:type :?, :child-bounds {:min 1, :max 1}
                         :re-validator (fn [_ [child]] (re/?-validator child))
                         :re-explainer (fn [_ [child]] (re/?-explainer child))
                         :re-parser (fn [_ [child]] (re/?-parser child))
                         :re-unparser (fn [_ [child]] (re/?-unparser child))
                         :re-transformer (fn [_ [child]] (re/?-transformer child))
                         :re-min-max (fn [_ [child]] {:min 0, :max (:max (-regex-min-max child))})})
   :repeat (-sequence-schema {:type :repeat, :child-bounds {:min 1, :max 1}
                              :re-validator (fn [{:keys [min max] :or {min 0, max ##Inf}} [child]] (re/repeat-validator min max child))
                              :re-explainer (fn [{:keys [min max] :or {min 0, max ##Inf}} [child]] (re/repeat-explainer min max child))
                              :re-parser (fn [{:keys [min max] :or {min 0, max ##Inf}} [child]] (re/repeat-parser min max child))
                              :re-unparser (fn [{:keys [min max] :or {min 0, max ##Inf}} [child]] (re/repeat-unparser min max child))
                              :re-transformer (fn [{:keys [min max] :or {min 0, max ##Inf}} [child]] (re/repeat-transformer min max child))
                              :re-min-max (fn [props [child]] (-re-min-max * props child))})
   :cat (-sequence-schema {:type :cat, :child-bounds {}
                           :re-validator (fn [_ children] (apply re/cat-validator children))
                           :re-explainer (fn [_ children] (apply re/cat-explainer children))
                           :re-parser (fn [_ children] (apply re/cat-parser children))
                           :re-unparser (fn [_ children] (apply re/cat-unparser children))
                           :re-transformer (fn [_ children] (apply re/cat-transformer children))
                           :re-min-max (fn [_ children] (reduce (partial -re-min-max +) {:min 0, :max 0} children))})
   :alt (-sequence-schema {:type :alt, :child-bounds {:min 1}
                           :re-validator (fn [_ children] (apply re/alt-validator children))
                           :re-explainer (fn [_ children] (apply re/alt-explainer children))
                           :re-parser (fn [_ children] (apply re/alt-parser children))
                           :re-unparser (fn [_ children] (apply re/alt-unparser children))
                           :re-transformer (fn [_ children] (apply re/alt-transformer children))
                           :re-min-max (fn [_ children] (reduce -re-alt-min-max {:max 0} children))})
   :catn (-sequence-entry-schema {:type :catn, :child-bounds {}
                                  :re-validator (fn [_ children] (apply re/cat-validator children))
                                  :re-explainer (fn [_ children] (apply re/cat-explainer children))
                                  :re-parser (fn [_ children] (apply re/catn-parser children))
                                  :re-unparser (fn [_ children] (apply re/catn-unparser children))
                                  :re-transformer (fn [_ children] (apply re/cat-transformer children))
                                  :re-min-max (fn [_ children] (reduce (partial -re-min-max +) {:min 0, :max 0} (map last children)))})
   :altn (-sequence-entry-schema {:type :altn, :child-bounds {:min 1}
                                  :re-validator (fn [_ children] (apply re/alt-validator children))
                                  :re-explainer (fn [_ children] (apply re/alt-explainer children))
                                  :re-parser (fn [_ children] (apply re/altn-parser children))
                                  :re-unparser (fn [_ children] (apply re/altn-unparser children))
                                  :re-transformer (fn [_ children] (apply re/alt-transformer children))
                                  :re-min-max (fn [_ children] (reduce -re-alt-min-max {:max 0} (map last children)))})})

(defn base-schemas []
  {:and (-and-schema)
   :or (-or-schema)
   :orn (-orn-schema)
   :not (-not-schema)
   :map (-map-schema)
   :map-of (-map-of-schema)
   :vector (-collection-schema {:type :vector, :pred vector?, :empty []})
   :sequential (-collection-schema {:type :sequential, :pred sequential?})
   :set (-collection-schema {:type :set, :pred set?, :empty #{}, :in (fn [_ x] x)})
   :enum (-enum-schema)
   :maybe (-maybe-schema)
   :tuple (-tuple-schema)
   :multi (-multi-schema)
   :re (-re-schema false)
   :fn (-fn-schema)
   :ref (-ref-schema)
   :=> (-=>-schema)
   :function (-function-schema nil)
   :schema (-schema-schema nil)
   ::schema (-schema-schema {:raw true})})

(defn default-schemas []
  (merge (predicate-schemas) (class-schemas) (comparator-schemas) (type-schemas) (sequence-schemas) (base-schemas)))

(def default-registry
  (mr/registry (cond (identical? mr/type "default") (default-schemas)
                     (identical? mr/type "custom") (mr/custom-default-registry)
                     :else (-fail! ::invalid-registry.type {:type mr/type}))))

;;
;; function schemas (alpha, subject to change)
;;

(defonce ^:private -function-schemas* (atom {}))
(defn function-schemas [] @-function-schemas*)

(defn function-schema
  ([?schema]
   (function-schema ?schema nil))
  ([?schema options]
   (let [s (schema ?schema options), t (type s)]
     (cond-> s (not (#{:=> :function} t)) (-fail! :invalid-=>schema {:type t, :schema s})))))

(defn -register-function-schema! [ns name schema data]
  (swap! -function-schemas* assoc-in [ns name] (merge data {:schema (function-schema schema), :ns ns, :name name})))

#?(:clj
   (defmacro => [name value]
     (let [name' `'~(symbol (str name))
           ns' `'~(symbol (str *ns*))
           sym `'~(symbol (str *ns*) (str name))]
       `(do (-register-function-schema! ~ns' ~name' ~value ~(meta name)) ~sym))))

(defn -instrument
  "Takes an instrumentation properties map and a function and returns a wrapped function,
   which will validate function arguments and return values based on the function schema
   definition. The following properties are used:

   | key       | description |
   | ----------|-------------|
   | `:schema` | function schema
   | `:scope`  | optional set of scope definitions, defaults to `#{:input :output}`
   | `:report` | optional side-effecting function of `key data -> any` to report problems, defaults to `m/-fail!`
   | `:gen`    | optional function of `schema -> schema -> value` to be invoked on the args to get the return value"
  ([props]
   (-instrument props nil nil))
  ([props f]
   (-instrument props f nil))
  ([{:keys [scope report gen] :or {scope #{:input :output}, report -fail!} :as props} f options]
   (let [schema (-> props :schema (schema options))]
     (case (type schema)
       :=> (let [{:keys [min max input output]} (-function-info schema)
                 [validate-input validate-output] (map validator [input output])
                 [wrap-input wrap-output] (map (partial contains? scope) [:input :output])
                 f (or (if gen (gen schema) f) (-fail! ::missing-function {:props props}))]
             (fn [& args]
               (let [args (vec args), arity (count args)]
                 (when wrap-input
                   (when-not (<= min arity (or max miu/+max-size+))
                     (report ::invalid-arity {:arity arity, :arities #{{:min min :max max}}, :args args, :input input, :schema schema}))
                   (when-not (validate-input args)
                     (report ::invalid-input {:input input, :args args, :schema schema})))
                 (let [value (apply f args)]
                   (when wrap-output
                     (when-not (validate-output value)
                       (report ::invalid-output {:output output, :value value, :args args, :schema schema})))
                   value))))
       :function (let [arity->info (->> (for [schema (children schema)]
                                          (let [{:keys [arity] :as info} (-function-info schema)]
                                            [arity (assoc info :f (-instrument (assoc props :schema schema) f options))]))
                                        (into {}))
                       arities (-> arity->info keys set)
                       varargs-info (arity->info :varargs)]
                   (if (= 1 (count arities))
                     (-> arity->info first val :f)
                     (fn [& args]
                       (let [arity (count args)
                             {:keys [input] :as info} (arity->info arity)
                             report-arity #(report ::invalid-arity {:arity arity, :arities arities, :args args, :input input, :schema schema})]
                         (cond
                           info (apply (:f info) args)
                           varargs-info (if (< arity (:min varargs-info)) (report-arity) (apply (:f varargs-info) args))
                           :else (report-arity))))))))))
