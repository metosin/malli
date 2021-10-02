(ns malli.core
  (:refer-clojure :exclude [eval type -deref deref -lookup -key])
  #?(:cljs (:require-macros malli.core))
  (:require [malli.sci :as ms]
            [malli.impl.util :as miu]
            [malli.impl.regex :as re]
            [malli.registry :as mr])
  #?(:clj (:import (java.util.regex Pattern)
                   (clojure.lang Associative IPersistentCollection MapEntry IPersistentVector LazilyPersistentVector PersistentArrayMap)
                   (malli.impl.util SchemaError)
                   (java.util.concurrent.atomic AtomicReference)
                   (java.util Collection LinkedList))))

(declare schema schema? into-schema into-schema? type eval default-registry
         -simple-schema -val-schema -ref-schema -schema-schema -registry
         parser unparser ast from-ast)

;;
;; protocols and records
;;

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
  (-walk [this walker path options] "walks the schema and it's children, ::m/walk-entry-vals, ::m/walk-refs, ::m/walk-schema-refs options effect how walking is done.")
  (-properties [this] "returns original schema properties")
  (-options [this] "returns original options")
  (-children [this] "returns schema children")
  (-parent [this] "returns the IntoSchema instance")
  (-form [this] "returns original form of the schema"))

(defprotocol AST
  (-to-ast [this options] "schema to ast")
  (-from-ast [this ast options] "ast to schema"))

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

(defn -ref-schema? [x] (#?(:clj instance?, :cljs implements?) malli.core.RefSchema x))

(extend-type #?(:clj Object, :cljs default)
  RegexSchema
  (-regex-op? [_] false)

  (-regex-validator [this]
    (if (-ref-schema? this)
      (-regex-validator (-deref this))
      (re/item-validator (-validator this))))

  (-regex-explainer [this path]
    (if (-ref-schema? this)
      (-regex-explainer (-deref this) path)
      (re/item-explainer path this (-explainer this path))))

  (-regex-parser [this]
    (if (-ref-schema? this)
      (-regex-parser (-deref this))
      (re/item-parser (parser this))))

  (-regex-unparser [this]
    (if (-ref-schema? this)
      (-regex-unparser (-deref this))
      (re/item-unparser (unparser this))))

  (-regex-transformer [this transformer method options]
    (if (-ref-schema? this)
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

(defn -deprecated! [x] (println "DEPRECATED:" x))

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

(defn- -property-registry [m options f]
  (let [options (assoc options ::allow-invalid-refs true)]
    (reduce-kv (fn [acc k v] (assoc acc k (f (schema v options)))) {} m)))

(defn ^:no-doc -check-children? [] true)

(defn -check-children!
  ([type properties children props]
   (-deprecated! "use (m/-check-children! type properties children min max) instead.")
   (-check-children! type properties children (:min props) (:max props)))
  ([type properties children min max]
   (when (-check-children?)
     (let [size (count children)]
       (when (or (and min (< size ^long min)) (and max (> size ^long max)))
         (-fail! ::child-error {:type type, :properties properties, :children children, :min min, :max max}))))))

(defn -create-form [type properties children]
  (let [has-children (seq children), has-properties (seq properties)]
    (cond (and has-properties has-children) (reduce conj [type properties] children)
          has-properties [type properties]
          has-children (reduce conj [type] children)
          :else type)))

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

(defn -equals [x y] (or (identical? x y) (= x y)))

(defn -vmap [f os]
  (let [oa (object-array (count os))
        iter #?(:clj (.iterator ^Iterable os), :cljs (iter os))
        n (volatile! -1)]
    (while (.hasNext iter)
      (aset oa (vreset! n (inc ^int @n)) (f (.next iter))))
    #?(:clj (LazilyPersistentVector/createOwning oa), (vec os))))

(defn -memoize [f]
  (let [value #?(:clj (AtomicReference. nil), :cljs (atom nil))]
    (fn [] #?(:clj (or (.get value) (do (.set value (f)) (.get value))), :cljs (or @value (reset! value (f)))))))

(defn -inner-indexed [walker path children options]
  (mapv (fn [[i c]] (-inner walker c (conj path i) options)) (map-indexed vector children)))

(defn -inner-entries [walker path entries options]
  (mapv (fn [[k s]] [k (-properties s) (-inner walker s (conj path k) options)]) entries))

(defn -parsed [s] (-> s meta ::parsed))

(defn -set-children [schema children]
  (if (-equals children (-children schema))
    schema (-into-schema (-parent schema) (-properties schema) children (-options schema))))

(defn -set-properties [schema properties]
  (if (-equals properties (-properties schema))
    schema (-into-schema (-parent schema) properties (or (-parsed schema) (-children schema)) (-options schema))))

(defn -update-options [schema f]
  (-into-schema (-parent schema) (-properties schema) (-children schema) (f (-options schema))))

(defn -set-assoc-children [schema key value]
  (-set-children schema (assoc (-children schema) key value)))

(defn -get-entries [schema key default]
  (or (some (if (and (vector? key) (= ::find (nth key 0)))
              (fn [e] (when (= (nth e 0) (nth key 1)) e))
              (fn [e] (when (= (nth e 0) key) (nth e 2))))
            (-children schema)) default))

(defrecord Parsed [keyset children entries forms])

(defn- -update-parsed [{:keys [keyset children entries forms]} ?key value options]
  (let [[k p override] (if (vector? ?key) [(nth ?key 0) (second ?key) true] [?key])
        s (when value (schema value options))
        i (int (:order (keyset k)))]
    (if (nil? s)
      ;; remove
      (letfn [(cut [v] (into (subvec v 0 i) (subvec v (inc i))))]
        (->Parsed (dissoc keyset k) (cut children) (cut entries) (cut forms)))
      (let [c [k p s]
            e (miu/-tagged k (-val-schema s p))
            p (if i (if override p (nth (children i) 1)) p)
            f (if (seq p) [k p (-form s)] [k (-form s)])]
        (if i
          ;; update
          (->Parsed keyset (assoc children i c) (assoc entries i e) (assoc forms i f))
          ;; assoc
          (let [size (inc (count keyset))]
            (->Parsed (assoc keyset k size) (conj children c) (conj entries e) (conj forms f))))))))

(defn -set-entries
  ([schema ?key value]
   (if-let [parsed (-parsed schema)]
     (-set-children schema (-update-parsed parsed ?key value (-options schema)))
     (let [found (atom nil)
           [key props override] (if (vector? ?key) [(nth ?key 0) (second ?key) true] [?key])
           children (cond-> (mapv (fn [[k p :as entry]]
                                    (if (= key k)
                                      (do (reset! found true) [key (if override props p) value])
                                      entry))
                                  (-children schema))
                      (not @found) (conj (if key [key props value] (-fail! ::key-missing)))
                      :always (->> (filter (fn [e] (-> e last some?)))))]
       (-set-children schema children)))))

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

(defn- -register-var [registry v]
  (let [name (-> v meta :name)
        schema (-simple-schema {:type name, :pred @v})]
    (-> registry
        (assoc name schema)
        (assoc @v schema))))

(defn -registry
  {:arglists '([] [{:keys [registry]}])}
  ([] default-registry)
  ([opts] (or (when opts (mr/registry (opts :registry))) default-registry)))

(defn- -lookup [?schema options]
  (let [registry (-registry options)]
    (or (mr/-schema registry ?schema)
        (some-> registry (mr/-schema (clojure.core/type ?schema)) (-into-schema nil [?schema] options)))))

(defn- -lookup! [?schema f options]
  (or (and f (f ?schema) ?schema)
      (-lookup ?schema options)
      (-fail! ::invalid-schema {:schema ?schema})))

(defn -into-transformer [x]
  (cond
    (#?(:clj instance?, :cljs implements?) malli.core.Transformer x) x
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
;; parsing entry schemas
;;

(defn- -parse-entry [e naked-keys lazy-refs options i ^objects -children ^objects -entries ^objects -forms ^objects -keyset]
  (letfn [(-collect [k c e f i]
            (let [i (int i)]
              (aset -keyset (* 2 i) k)
              (aset -keyset (inc (* 2 i)) {:order i})
              (aset -children i c)
              (aset -entries i e)
              (aset -forms i f)
              (unchecked-inc-int i)))
          (-schema [e] (schema (cond-> e (and (-reference? e) lazy-refs) (-lazy options)) options))
          (-parse-ref-entry [e]
            (let [s (-schema e)
                  c [e nil s]
                  e' (miu/-tagged e (-val-schema s nil))]
              (-collect e c e' e i)))
          (-parse-ref-vector1 [e e0]
            (let [s (-schema e0)
                  c [e0 nil s]
                  e' (miu/-tagged e0 (-val-schema s nil))]
              (-collect e0 c e' e i)))
          (-parse-ref-vector2 [e e0 e1]
            (let [s (-schema e0)
                  c [e0 e1 s]
                  e' (miu/-tagged e0 (-val-schema s e1))]
              (-collect e0 c e' e i)))
          (-parse-entry-else2 [e0 e1]
            (let [s (-schema e1)
                  f [e0 (-form s)]
                  c [e0 nil s]
                  e' (miu/-tagged e0 (-val-schema s nil))]
              (-collect e0 c e' f i)))
          (-parse-entry-else3 [e0 e1 e2]
            (let [s (-schema e2)
                  f' (-form s)
                  f (if e1 [e0 e1 f'] [e0 f'])
                  c [e0 e1 s]
                  e' (miu/-tagged e0 (-val-schema s e1))]
              (-collect e0 c e' f i)))]
    (if (sequential? e)
      (let [n (count e), e0 (nth e 0)]
        (if (== n 1)
          (if (and (-reference? e0) naked-keys) (-parse-ref-vector1 e e0) i)
          (let [e1 (nth e 1)]
            (if (== n 2)
              (if (and (-reference? e0) (map? e1))
                (if naked-keys (-parse-ref-vector2 e e0 e1) i)
                (-parse-entry-else2 e0 e1))
              (let [e2 (nth e 2)]
                (-parse-entry-else3 e0 e1 e2))))))
      (if (and naked-keys (-reference? e))
        (-parse-ref-entry e)
        (-fail! ::invalid-ref {:ref e})))))

(defn -parse-entries [?children props options]
  (if (instance? Parsed ?children)
    ?children
    (letfn [(-vec [^objects arr] #?(:clj (LazilyPersistentVector/createOwning arr), :cljs (vec arr)))
            (-map [^objects arr] #?(:clj (PersistentArrayMap/createWithCheck arr)
                                    :cljs (let [m (apply array-map arr)]
                                            (when-not (= (* 2 (count m)) (count arr))
                                              (-fail! ::duplicate-keys)) m)))
            (-arange [^objects arr to]
             #?(:clj (let [-arr (object-array to)] (System/arraycopy arr 0 -arr 0 to) -arr)
                :cljs (.slice arr 0 to)))]
      (let [{:keys [naked-keys lazy-refs]} props
            n (count ?children)
            -children (object-array n)
            -entries (object-array n)
            -forms (object-array n)
            -keyset (object-array (* 2 n))]
        (loop [i (int 0), ci (int 0)]
          (if (== ci n)
            (let [f (if (== ci i) -vec #(-vec (-arange % i)))]
              (->Parsed (-map -keyset) (f -children) (f -entries) (f -forms)))
            (recur (int (-parse-entry (nth ?children i) naked-keys lazy-refs options i -children -entries -forms -keyset))
                   (unchecked-inc-int ci))))))))

;;
;; ast
;;

(defn -parse-entry-ast [ast options]
  (let [->child (fn [[k v]] [k (:properties v) (from-ast (:value v) options)])
        ast-entry-order (::ast-entry-order options)
        keyset (:keys ast)
        children (if ast-entry-order
                   (->> keyset (sort-by (fn [e] (:order (val e)))) (map ->child))
                   (->> keyset (map ->child)))
        entries (->> children (map (fn [[k p v]] (miu/-tagged k (-val-schema v p)))))
        forms (->> children (map (fn [[k p v]] (if p [k p (-form v)] [k (-form v)]))))]
    (->Parsed keyset children entries forms)))

(defn -from-entry-ast [parent ast options]
  (-into-schema parent (:properties ast) (-parse-entry-ast ast options) options))

(defn -ast [acc properties options]
  (let [registry (when-let [registry (:registry properties)]
                   (into {} (map (fn [[k v]] [k (ast v options)])) registry))
        properties (not-empty (cond-> properties registry (dissoc :registry)))]
    (cond-> acc properties (assoc :properties properties) registry (assoc :registry registry))))

(defn -entry-ast [schema keyset]
  (-ast {:type (type schema)
         :keys (reduce (fn [acc [k p s]] (assoc acc k (cond-> {:order (-> keyset (get k) :order),
                                                               :value (ast s)} p (assoc :properties p))))
                       {} (-children schema))}
        (-properties schema)
        (-options schema)))

(defn -from-child-ast [parent ast options]
  (-into-schema parent (:properties ast) [(from-ast (:child ast) options)] options))

(defn -to-child-ast [schema]
  (-ast {:type (type schema), :child (ast (nth (-children schema) 0))} (-properties schema) (-options schema)))

(defn -from-value-ast [parent ast options]
  (-into-schema parent (:properties ast) (if-let [value (:value ast)] [value]) options))

(defn -to-value-ast [schema]
  (-ast {:type (type schema), :value (nth (-children schema) 0)} (-properties schema) (-options schema)))

(defn -from-type-ast [parent ast options]
  (-into-schema parent (:properties ast) nil options))

(defn -to-type-ast [schema]
  (-ast {:type (type schema)} (-properties schema) (-options schema)))

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
;; Schemas
;;

(defn -simple-schema [?props]
  (let [{:keys [type type-properties pred property-pred min max from-ast to-ast]
         :or {min 0, max 0, from-ast -from-value-ast, to-ast -to-type-ast}} (if (map? ?props) ?props)]
    ^{:type ::into-schema}
    (reify
      AST
      (-from-ast [parent ast options] (from-ast parent ast options))
      IntoSchema
      (-type [_] type)
      (-type-properties [_] type-properties)
      (-properties-schema [_ _])
      (-children-schema [_ _])
      (-into-schema [parent properties children options]
        (if (fn? ?props)
          (-into-schema (-simple-schema (?props properties children)) properties children options)
          (let [_ (-check-children! type properties children min max)
                pvalidator (if property-pred (property-pred properties))
                validator (if pvalidator (fn [x] (and (pred x) (pvalidator x))) pred)
                form (delay (-create-form type properties children))]
            ^{:type ::schema}
            (reify
              AST
              (-to-ast [this _] (to-ast this))
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
                  (-outer walker this path children options)))
              (-properties [_] properties)
              (-options [_] options)
              (-children [_] children)
              (-parent [_] parent)
              (-form [_] @form)
              LensSchema
              (-keep [_])
              (-get [_ _ default] default)
              (-set [this key _] (-fail! ::non-associative-schema {:schema this, :key key})))))))))

(defn -nil-schema [] (-simple-schema {:type :nil, :pred nil?}))
(defn -any-schema [] (-simple-schema {:type :any, :pred any?}))
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
      (let [children (-vmap #(schema % options) children)
            form (delay (-create-form :and properties (map -form children)))
            ->parser (fn [f m] (let [parsers (m (mapv f children))]
                                 #(reduce (fn [x parser] (miu/-map-invalid reduced (parser x))) % parsers)))]
        ^{:type ::schema}
        (reify
          Schema
          (-validator [_]
            (let [validators (-vmap -validator children)]
              #?(:clj  (miu/-every-pred validators)
                 :cljs (if (nth validators 1) (apply every-pred validators) (nth validators 0)))))
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
          (-properties [_] properties)
          (-options [_] options)
          (-children [_] children)
          (-parent [_] parent)
          (-form [_] @form)
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
      (let [children (-vmap #(schema % options) children)
            form (delay (-create-form :or properties (map -form children)))
            ->parser (fn [f] (let [parsers (mapv f children)]
                               #(reduce (fn [_ parser] (miu/-map-valid reduced (parser %))) ::invalid parsers)))]
        ^{:type ::schema}
        (reify
          Schema
          (-validator [_]
            (let [validators (-vmap -validator children)]
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
          (-properties [_] properties)
          (-options [_] options)
          (-children [_] children)
          (-parent [_] parent)
          (-form [_] @form)
          LensSchema
          (-keep [_])
          (-get [_ key default] (get children key default))
          (-set [this key value] (-set-assoc-children this key value)))))))

(defn -orn-schema []
  ^{:type ::into-schema}
  (reify
    AST
    (-from-ast [parent ast options] (-from-entry-ast parent ast options))
    IntoSchema
    (-type [_] :orn)
    (-type-properties [_])
    (-properties-schema [_ _])
    (-children-schema [_ _])
    (-into-schema [parent properties children options]
      (-check-children! :orn properties children 1 nil)
      (let [{:keys [keyset children entries forms] :as parsed} (-parse-entries children {:naked-keys true} options)
            form (delay (-create-form :orn properties forms))]
        ^{:type ::schema
          ::parsed parsed}
        (reify
          AST
          (-to-ast [this _] (-entry-ast this keyset))
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
          (-properties [_] properties)
          (-options [_] options)
          (-children [_] children)
          (-parent [_] parent)
          (-form [_] @form)
          LensSchema
          (-keep [_] true)
          (-get [this key default] (-get-entries this key default))
          (-set [this key value] (-set-entries this key value)))))))

(defn -not-schema []
  ^{:type ::into-schema}
  (reify
    AST
    (-from-ast [parent ast options] (-from-child-ast parent ast options))
    IntoSchema
    (-type [_] :not)
    (-type-properties [_])
    (-properties-schema [_ _])
    (-children-schema [_ _])
    (-into-schema [parent properties children options]
      (-check-children! :not properties children 1 1)
      (let [[schema :as children] (mapv #(schema % options) children)
            validator (complement (-validator schema))
            form (-create-form :not properties (map -form children))]
        ^{:type ::schema}
        (reify
          AST
          (-to-ast [this _] (-to-child-ast this))
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
   (-into-schema (-val-schema) properties (list schema) (-options schema)))
  ([]
   ^{:type ::into-schema}
   (reify
     AST
     (-from-ast [parent ast options] (-from-child-ast parent ast options))
     IntoSchema
     (-type [_] ::val)
     (-type-properties [_])
     (-properties-schema [_ _])
     (-children-schema [_ _])
     (-into-schema [parent properties children options]
       #_(-check-children! ::val properties children 1 1)
       (let [schema (schema (first children) options)
             form (delay (-create-form ::val properties [(-form schema)]))]
         ^{:type ::schema}
         (reify
           AST
           (-to-ast [this _] (-to-child-ast this))
           Schema
           (-validator [_] (-validator schema))
           (-explainer [_ path] (-explainer schema path))
           (-parser [_] (-parser schema))
           (-unparser [_] (-unparser schema))
           (-transformer [this transformer method options]
             (-parent-children-transformer this (list schema) transformer method options))
           (-walk [this walker path options]
             (if (::walk-entry-vals options)
               (if (-accept walker this path options)
                 (-outer walker this path (list (-inner walker schema path options)) options))
               (-walk schema walker path options)))
           (-properties [_] properties)
           (-options [_] (-options schema))
           (-children [_] [schema])
           (-parent [_] parent)
           (-form [_] @form)
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
   (reify
     AST
     (-from-ast [parent ast options] (-from-entry-ast parent ast options))
     IntoSchema
     (-type [_] :map)
     (-type-properties [_])
     (-properties-schema [_ _])
     (-children-schema [_ _])
     (-into-schema [parent {:keys [closed] :as properties} children options]
       (let [{:keys [keyset children entries forms] :as parsed} (-parse-entries children opts options)
             form (delay (-create-form :map properties forms))
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
         ^{:type ::schema
           ::parsed parsed}
         (reify
           AST
           (-to-ast [this _] (-entry-ast this keyset))
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
           (-properties [_] properties)
           (-options [_] options)
           (-children [_] children)
           (-parent [_] parent)
           (-form [_] @form)
           MapSchema
           (-entries [_] entries)
           LensSchema
           (-keep [_] true)
           (-get [this key default] (-get-entries this key default))
           (-set [this key value] (-set-entries this key value))))))))

(defn -map-of-schema []
  ^{:type ::into-schema}
  (reify
    AST
    (-from-ast [parent ast options]
      (-into-schema parent (:properties ast) [(from-ast (:key ast) options) (from-ast (:value ast) options)] options))
    IntoSchema
    (-type [_] :map-of)
    (-type-properties [_])
    (-properties-schema [_ _])
    (-children-schema [_ _])
    (-into-schema [parent {:keys [min max] :as properties} children options]
      (-check-children! :map-of properties children 2 2)
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
          AST
          (-to-ast [this _]
            (-ast {:type :map-of, :key (ast key-schema), :value (ast value-schema)} properties options))
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
    (reify
      AST
      (-from-ast [parent ast options] (-from-child-ast parent ast options))
      IntoSchema
      (-type [_] (:type @props*))
      (-type-properties [_] (:type-properties @props*))
      (-properties-schema [_ _])
      (-children-schema [_ _])
      (-into-schema [parent {:keys [min max] :as properties} children options]
        (if (fn? ?props)
          (-into-schema (-collection-schema (?props properties children)) properties children options)
          (let [{type :type fpred :pred, fempty :empty, fin :in :or {fin (fn [i _] i)}} ?props]
            (reset! props* ?props)
            (-check-children! type properties children 1 1)
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
                AST
                (-to-ast [this _] (-to-child-ast this))
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
      (let [children (into [] (map #(schema % options)) children)
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
  (reify
    AST
    (-from-ast [parent ast options] (-into-schema parent (:properties ast) (:values ast) options))
    IntoSchema
    (-type [_] :enum)
    (-type-properties [_])
    (-into-schema [parent properties children options]
      (-check-children! :enum properties children 1 nil)
      (let [children (vec children)
            schema (set children)
            form (-create-form :enum properties children)]
        ^{:type ::schema}
        (reify
          AST
          (-to-ast [_ _] {:type :enum, :values children})
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
  (reify
    AST
    (-from-ast [parent ast options] (-from-value-ast parent ast options))
    IntoSchema
    (-type [_] :re)
    (-type-properties [_])
    (-properties-schema [_ _])
    (-children-schema [_ _])
    (-into-schema [parent properties [child :as children] options]
      (-check-children! :re properties children 1 1)
      (let [children (vec children)
            re (re-pattern child)
            form (if class? re (-create-form :re properties children))]
        ^{:type ::schema}
        (reify
          AST
          (-to-ast [this _] (-to-value-ast this))
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
  (reify
    AST
    (-from-ast [parent ast options] (-from-value-ast parent ast options))
    IntoSchema
    (-type [_] :fn)
    (-type-properties [_])
    (-into-schema [parent properties children options]
      (-check-children! :fn properties children 1 1)
      (let [children (vec children)
            f (eval (first children) options)
            form (-create-form :fn properties children)]
        ^{:type ::schema}
        (reify
          AST
          (-to-ast [this _] (-to-value-ast this))
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
  (reify
    AST
    (-from-ast [parent ast options] (-from-child-ast parent ast options))
    IntoSchema
    (-type [_] :maybe)
    (-type-properties [_])
    (-properties-schema [_ _])
    (-children-schema [_ _])
    (-into-schema [parent properties children options]
      (-check-children! :maybe properties children 1 1)
      (let [[schema :as children] (map #(schema % options) children)
            form (-create-form :maybe properties (map -form children))
            ->parser (fn [f] (let [parser (f schema)]
                               (fn [x] (if (nil? x) x (parser x)))))]
        ^{:type ::schema}
        (reify
          AST
          (-to-ast [this _] (-to-child-ast this))
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
   (reify
     AST
     (-from-ast [parent ast options] (-from-entry-ast parent ast options))
     IntoSchema
     (-type [_] :multi)
     (-type-properties [_] (:type-properties opts))
     (-properties-schema [_ _])
     (-children-schema [_ _])
     (-into-schema [parent properties children options]
       (let [type (or (:type opts) :multi)
             lazy-refs (:lazy-refs properties)
             opts (when lazy-refs (assoc opts :lazy-refs lazy-refs))
             {:keys [keyset children entries forms] :as parsed} (-parse-entries children opts options)
             form (delay (-create-form type properties forms))
             dispatch (eval (:dispatch properties) options)
             dispatch-map (delay (into {} entries))
             finder (fn [{:keys [::default] :as m}] (fn [x] (m x default)))]
         (when-not dispatch
           (-fail! ::missing-property {:key :dispatch}))
         ^{:type ::schema
           ::parsed parsed}
         (reify
           AST
           (-to-ast [this _] (-entry-ast this keyset))
           Schema
           (-validator [_]
             (let [find (finder (reduce-kv (fn [acc k s] (assoc acc k (-validator s))) {} @dispatch-map))]
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
                   find (finder (reduce-kv (fn [acc k s] (assoc acc k (parse k s))) {} @dispatch-map))]
               (fn [x] (if-some [parser (find (dispatch x))] (parser x) ::invalid))))
           (-unparser [_]
             (let [unparsers (reduce-kv (fn [acc k s] (assoc acc k (-unparser s))) {} @dispatch-map)]
               (fn [x] (if (miu/-tagged? x) (if-some [f (unparsers (key x))] (f (val x)) ::invalid) ::invalid))))
           (-transformer [this transformer method options]
             ;; FIXME: Probably should not use `dispatch`
             ;; Can't use `dispatch` as `x` might not be valid before it has been unparsed:
             (let [this-transformer (-value-transformer transformer this method options)
                   ->children (reduce-kv (fn [acc k s] (let [t (-transformer s transformer method options)]
                                                         (cond-> acc t (assoc k t)))) {} @dispatch-map)
                   find (finder ->children)
                   child-transformer (if (seq ->children) (fn [x] (if-some [t (find (dispatch x))] (t x) x)))]
               (-intercepting this-transformer child-transformer)))
           (-walk [this walker path options]
             (if (-accept walker this path options)
               (-outer walker this path (-inner-entries walker path entries options) options)))
           (-properties [_] properties)
           (-options [_] options)
           (-children [_] children)
           (-parent [_] parent)
           (-form [_] @form)
           MapSchema
           (-entries [_] entries)
           LensSchema
           (-keep [_])
           (-get [this key default] (-get-entries this key default))
           (-set [this key value] (-set-entries this key value))))))))

(defn -ref-schema
  ([]
   (-ref-schema nil))
  ([{:keys [lazy type-properties]}]
   ^{:type ::into-schema}
   (reify
     AST
     (-from-ast [parent ast options] (-from-value-ast parent ast options))
     IntoSchema
     (-type [_] :ref)
     (-type-properties [_] type-properties)
     (-into-schema [parent properties [ref :as children] {::keys [allow-invalid-refs] :as options}]
       (-check-children! :ref properties children 1 1)
       (when-not (-reference? ref)
         (-fail! ::invalid-ref {:ref ref}))
       (let [-ref (or (and lazy (-memoize (fn [] (schema (mr/-schema (-registry options) ref) options))))
                      (if-let [s (mr/-schema (-registry options) ref)] (-memoize (fn [] (schema s options))))
                      (when-not allow-invalid-refs
                        (-fail! ::invalid-ref {:type :ref, :ref ref})))
             children (vec children)
             form (delay (-create-form :ref properties children))
             ->parser (fn [f] (let [parser (-memoize (fn [] (f (-ref))))]
                                (fn [x] ((parser) x))))]
         ^{:type ::schema}
         (reify
           AST
           (-to-ast [this _] (-to-value-ast this))
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
           (-properties [_] properties)
           (-options [_] options)
           (-children [_] children)
           (-parent [_] parent)
           (-form [_] @form)
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

(defn -schema-schema [{:keys [id raw]}]
  ^{:type ::into-schema}
  (let [internal (or id raw)
        type (if internal ::schema :schema)]
    (reify
      AST
      (-from-ast [parent ast options] ((if internal -from-value-ast -from-child-ast) parent ast options))
      IntoSchema
      (-type [_] type)
      (-type-properties [_])
      (-properties-schema [_ _])
      (-children-schema [_ _])
      (-into-schema [parent properties children options]
        (-check-children! type properties children 1 1)
        (let [children (into [] (map #(schema % options)) children)
              child (nth children 0)
              form (delay (or (and (empty? properties) (or id (and raw (-form child))))
                              (-create-form type properties [(-form child)])))]
          ^{:type ::schema}
          (reify
            AST
            (-to-ast [this _]
              (if id (-ast {:type type, :value id} (-properties this) (-options this)) (-to-child-ast this)))
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
            (-properties [_] properties)
            (-options [_] options)
            (-children [_] children)
            (-parent [_] parent)
            (-form [_] @form)
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
              (if internal
                (-regex-validator child)
                (re/item-validator (-validator child))))
            (-regex-explainer [_ path]
              (if internal
                (-regex-explainer child path)
                (re/item-explainer path child (-explainer child path))))
            (-regex-parser [_]
              (if internal
                (-regex-parser child)
                (re/item-parser (parser child))))
            (-regex-unparser [_]
              (if internal
                (-regex-unparser child)
                (re/item-unparser (unparser child))))
            (-regex-transformer [_ transformer method options]
              (if internal
                (-regex-transformer child transformer method options)
                (re/item-transformer method (-validator child)
                                     (or (-transformer child transformer method options) identity))))
            (-regex-min-max [_] (-regex-min-max child))))))))

(defn -=>-schema []
  ^{:type ::into-schema}
  (reify
    AST
    (-from-ast [parent {:keys [input output properties]} options]
      (-into-schema parent properties [(from-ast input options) (from-ast output options)] options))
    IntoSchema
    (-type [_] :=>)
    (-type-properties [_])
    (-into-schema [parent properties children {::keys [function-checker] :as options}]
      (-check-children! :=> properties children 2 2)
      (let [[input output :as children] (map #(schema % options) children)
            form (-create-form :=> properties (map -form children))
            ->checker (if function-checker #(function-checker % options) (constantly nil))]
        (when-not (#{:cat :catn} (type input))
          (-fail! ::invalid-input-schema {:input input}))
        ^{:type ::schema}
        (reify
          AST
          (-to-ast [_ _]
            (cond-> {:type :=>, :input (ast input), :output (ast output)}
              properties (assoc :properties properties)))
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
      (-check-children! :function properties children 1 nil)
      (let [children (into [] (map #(schema % options)) children)
            form (delay (-create-form :function properties (map -form children)))
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
          (-properties [_] properties)
          (-options [_] options)
          (-children [_] children)
          (-parent [_] parent)
          (-form [_] @form)
          LensSchema
          (-keep [_])
          (-get [this key default] (get children key default))
          (-set [this key value] (-set-assoc-children this key value)))))))

(defn- regex-validator [schema] (re/validator (-regex-validator schema)))

(defn- regex-explainer [schema path] (re/explainer schema path (-regex-explainer schema path)))

(defn- regex-parser [schema] (re/parser (-regex-parser schema)))

(defn- regex-transformer [schema transformer method options]
  (let [this-transformer (-value-transformer transformer schema method options)
        ->children (re/transformer (-regex-transformer schema transformer method options))]
    (-intercepting this-transformer ->children)))

(defn -sequence-schema
  [{:keys [type re-validator re-explainer re-parser re-unparser re-transformer re-min-max] {:keys [min max]} :child-bounds :as opts}]
  ^{:type ::into-schema}
  (reify IntoSchema
    (-type [_] type)
    (-type-properties [_])
    (-properties-schema [_ _])
    (-children-schema [_ _])
    (-into-schema [parent properties children options]
      (-check-children! type properties children min max)
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
  [{:keys [type re-validator re-explainer re-parser re-unparser re-transformer re-min-max] {:keys [min max]} :child-bounds :as opts}]
  ^{:type ::into-schema}
  (reify
    AST
    (-from-ast [parent ast options] (-from-entry-ast parent ast options))
    IntoSchema
    (-type [_] type)
    (-type-properties [_])
    (-properties-schema [_ _])
    (-children-schema [_ _])
    (-into-schema [parent properties children options]
      (-check-children! type properties children min max)
      (let [{:keys [keyset children entries forms] :as parsed} (-parse-entries children opts options)
            form (delay (-create-form type properties forms))]
        ^{:type ::schema
          ::parsed parsed}
        (reify
          AST
          (-to-ast [this _] (-entry-ast this keyset))
          Schema
          (-validator [this] (regex-validator this))
          (-explainer [this path] (regex-explainer this path))
          (-parser [this] (regex-parser this))
          (-unparser [this] (-regex-unparser this))
          (-transformer [this transformer method options] (regex-transformer this transformer method options))
          (-walk [this walker path options]
            (if (-accept walker this path options)
              (-outer walker this path (-inner-entries walker path entries options) options)))
          (-properties [_] properties)
          (-options [_] options)
          (-children [_] children)
          (-parent [_] parent)
          (-form [_] @form)
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
            (re-transformer properties (map (fn [[k _ s]] [k (-regex-transformer s transformer method options)]) children)))
          (-regex-min-max [_] (re-min-max properties children)))))))

;;
;; public api
;;

;;
;; into-schema
;;

(defn into-schema?
  "Checks if x is a IntoSchema instance"
  [x] (#?(:clj instance?, :cljs implements?) malli.core.IntoSchema x))

(defn into-schema
  "Creates a Schema instance out of type, optional properties map and children"
  ([type properties children]
   (into-schema type properties children nil))
  ([type properties children options]
   (let [properties (when properties (when (pos? (count properties)) properties))
         r (when properties (properties :registry))
         options (if r (-update options :registry #(mr/composite-registry r (or % (-registry options)))) options)
         properties (if r (assoc properties :registry (-property-registry r options -form)) properties)]
     (-into-schema (-lookup! type into-schema? options) properties children options))))

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
  [x] (#?(:clj instance?, :cljs implements?) malli.core.Schema x))

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
     (map? ?schema) (from-ast ?schema options)
     (vector? ?schema) (let [v #?(:clj ^IPersistentVector ?schema, :cljs ?schema)
                             t #?(:clj (.nth v 0), :cljs (nth v 0))
                             n #?(:clj (.count v), :cljs (count v))
                             ?p (when (> n 1) #?(:clj (.nth v 1), :cljs (nth v 1)))]
                         (if (or (nil? ?p) (map? ?p))
                           (into-schema t ?p (when (< 2 n) (subvec ?schema 2 n)) options)
                           (into-schema t nil (when (< 1 n) (subvec ?schema 1 n)) options)))
     :else (if-let [?schema' (and (-reference? ?schema) (-lookup ?schema options))]
             (-pointer ?schema (schema ?schema' options) options)
             (-> ?schema (-lookup! nil options) (recur options))))))

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
  "Postwalks recursively over the Schema and it's children.
   The walker callback is a arity4 function with the following
   arguments: schema, path, (walked) children and options."
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
     (if (#?(:clj instance?, :cljs implements?) malli.core.MapSchema schema)
       (-entries schema)))))

(defn deref
  "Derefs top-level `RefSchema`s or returns original Schema."
  ([?schema]
   (deref ?schema nil))
  ([?schema options]
   (let [schema (schema ?schema options)]
     (cond-> schema (-ref-schema? schema) (-deref)))))

(defn deref-all
  "Derefs top-level `RefSchema`s recursively or returns original Schema."
  ([?schema]
   (deref-all ?schema nil))
  ([?schema options]
   (let [schema (deref ?schema options)]
     (cond-> schema (-ref-schema? schema) (recur options)))))

(defn from-ast
  "Creates a Schema from AST"
  ([ast] (from-ast ast nil))
  ([ast options]
   (if-let [s (-lookup (:type ast) options)]
     (let [r (:registry ast)
           p (:properties ast)
           options (cond-> options r (-update :registry #(mr/composite-registry r (or % (-registry options)))))
           p' (if r (assoc p :registry (-property-registry r options identity)))
           ast (cond-> ast p' (assoc :properties p'))]
       (if (#?(:clj instance?, :cljs implements?) malli.core.AST s)
         (-from-ast s ast options)
         (-into-schema s (:properties ast) (:children ast) options)))
     (-fail! ::invalid-ast {:ast ast}))))

(defn ast
  "Returns the Schema AST"
  ([?schema] (ast ?schema nil))
  ([?schema options]
   (let [s (schema ?schema options)]
     (if (#?(:clj instance?, :cljs implements?) malli.core.AST s)
       (-to-ast s options)
       (let [c (-children s)]
         (-ast (cond-> {:type (type s)}
                 c (assoc :children (into [] (map #(ast % options)) c)))
               (-properties s)
               (-options s)))))))
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
       (map (fn [[k v]] [k (-simple-schema (fn [_ [child]]
                                             {:type k
                                              :pred (-safe-pred #(v % child))
                                              :from-ast -from-value-ast
                                              :to-ast -to-value-ast
                                              :min 1
                                              :max 1}))]))
       (into {}) (reduce-kv assoc nil)))

(defn type-schemas []
  {:any (-any-schema)
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
  (mr/registry (cond (identical? mr/type "default") (mr/fast-registry (default-schemas))
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
     (if (#{:=> :function} t) s (-fail! :invalid-=>schema {:type t, :schema s})))))

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
