(ns malli.core
  (:refer-clojure :exclude [eval type -deref deref -lookup -key assert])
  #?(:cljs (:require-macros malli.core))
  (:require #?(:clj [clojure.walk :as walk])
            [clojure.core :as c]
            [malli.impl.regex :as re]
            [malli.impl.util :as miu]
            [malli.registry :as mr]
            [malli.sci :as ms])
  #?(:clj (:import #?(:bb  (clojure.lang Associative IPersistentCollection MapEntry IPersistentVector PersistentArrayMap)
                      :clj (clojure.lang Associative IPersistentCollection MapEntry IPersistentVector LazilyPersistentVector PersistentArrayMap))
                   (java.util.concurrent.atomic AtomicReference)
                   (java.util.regex Pattern))))

(declare schema schema? into-schema into-schema? type eval default-registry
         -simple-schema -val-schema -ref-schema -schema-schema -registry
         parser unparser ast from-ast -instrument ^:private -safely-countable?)

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

(defprotocol EntryParser
  (-entry-keyset [this])
  (-entry-children [this])
  (-entry-entries [this])
  (-entry-forms [this]))

(defprotocol EntrySchema
  (-entries [this] "returns sequence of `key -val-schema` entries")
  (-entry-parser [this]))

(defprotocol Cached
  (-cache [this]))

(defprotocol LensSchema
  (-keep [this] "returns truthy if schema contributes to value path")
  (-get [this key default] "returns schema at key")
  (-set [this key value] "returns a copy with key having new value"))

(defprotocol RefSchema
  (-ref [this] "returns the reference name")
  (-deref [this] "returns the referenced schema"))

(defprotocol Walker
  (-accept [this schema path options])
  (-inner [this schema path options])
  (-outer [this schema path children options]))

(defprotocol Transformer
  (-transformer-chain [this] "returns transformer chain as a vector of maps with :name, :encoders, :decoders and :options")
  (-value-transformer [this schema method options] "returns a value transforming interceptor for the given schema and method"))

(defprotocol RegexSchema
  (-regex-op? [this] "is this a regex operator (e.g. :cat, :*...)")
  (-regex-validator [this] "returns the raw internal regex validator implementation")
  (-regex-explainer [this path] "returns the raw internal regex explainer implementation")
  (-regex-unparser [this] "returns the raw internal regex unparser implementation")
  (-regex-parser [this] "returns the raw internal regex parser implementation")
  (-regex-transformer [this transformer method options] "returns the raw internal regex transformer implementation")
  (-regex-min-max [this nested?] "returns size of the sequence as {:min min :max max}. nil max means unbounded. nested? is true when this schema is nested inside an outer regex schema."))

(defprotocol FunctionSchema
  (-function-schema? [this])
  (-function-schema-arities [this])
  (-function-info [this])
  (-instrument-f [schema props f options]))

(defprotocol DistributiveSchema
  (-distributive-schema? [this])
  (-distribute-to-children [this f options]))

(defn -ref-schema? [x] (#?(:clj instance?, :cljs implements?) malli.core.RefSchema x))
(defn -entry-parser? [x] (#?(:clj instance?, :cljs implements?) malli.core.EntryParser x))
(defn -entry-schema? [x] (#?(:clj instance?, :cljs implements?) malli.core.EntrySchema x))
(defn -cached? [x] (#?(:clj instance?, :cljs implements?) malli.core.Cached x))
(defn -ast? [x] (#?(:clj instance?, :cljs implements?) malli.core.AST x))
(defn -transformer? [x] (#?(:clj instance?, :cljs implements?) malli.core.Transformer x))

(extend-type #?(:clj Object, :cljs default)
  FunctionSchema
  (-function-schema? [_] false)
  (-function-info [_])
  (-function-schema-arities [_])
  (-instrument-f [_ _ _ _])

  DistributiveSchema
  (-distributive-schema? [_] false)
  (-distribute-to-children [this _ _]
    (throw (ex-info "Not distributive" {:schema this})))

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

  (-regex-min-max [_ _] {:min 1, :max 1}))

#?(:clj (defmethod print-method ::into-schema [v ^java.io.Writer w] (.write w (str "#IntoSchema {:type " (pr-str (-type ^IntoSchema v)) "}"))))
#?(:clj (defmethod print-method ::schema [v ^java.io.Writer w] (.write w (pr-str (-form ^Schema v)))))
#?(:cljs (defn- pr-writer-into-schema [obj writer opts]
           (-write writer "#IntoSchema ")
           (-pr-writer {:type (-type ^IntoSchema obj)} writer opts)))
#?(:cljs (defn- pr-writer-schema [obj writer opts]
           (-pr-writer (-form ^Schema obj) writer opts)))

(defrecord Tag [key value])

(defn tag
  "A tagged value, used eg. for results of `parse` for `:orn` schemas."
  [key value] (->Tag key value))

(defn tag?
  "Is this a value constructed with `tag`?"
  [x] (instance? Tag x))

(defrecord Tags [values])

(defn tags
  "A collection of tagged values. `values` should be a map from tag to value.
   Used eg. for results of `parse` for `:catn` schemas."
  [values] (->Tags values))

(defn tags?
  "Is this a value constructed with `tags`?"
  [x] (instance? Tags x))

;;
;; impl
;;

(defn -deprecated! [x] (println "DEPRECATED:" x))

(defn -exception [type data] (ex-info (str type) {:type type, :message type, :data data}))

(defn -fail!
  ([type] (-fail! type nil))
  ([type data] (throw (-exception type data))))

(defn -safe-pred [f] #(try (boolean (f %)) (catch #?(:clj Exception, :cljs js/Error) _ false)))

(defn -keyword->string [x]
  (if (keyword? x)
    (if-let [nn (namespace x)]
      (str nn "/" (name x))
      (name x))
    x))

(defn -guard [pred tf] (when tf (fn [x] (if (pred x) (tf x) x))))

(defn -unlift-keys [m prefix]
  (reduce-kv #(if (= (name prefix) (namespace %2)) (assoc %1 (keyword (name %2)) %3) %1) {} m))

(defn ^:no-doc -check-children? [] true)

(defn -check-children!
  ([type properties children props]
   (-deprecated! "use (m/-check-children! type properties children min max) instead.")
   (-check-children! type properties children (:min props) (:max props)))
  ([type properties children min max]
   (when (-check-children?)
     (when-let [size (and (or (sequential? children) (nil? children)) (count children))]
       (when (or (and min (< size ^long min)) (and max (> size ^long max)))
         (-fail! ::child-error {:type type, :properties properties, :children children, :min min, :max max}))))))

(defn -pointer [id schema options] (-into-schema (-schema-schema {:id id}) nil [schema] options))

(defn -reference? [?schema] (or (string? ?schema) (qualified-ident? ?schema) (var? ?schema)))

(defn -lazy [ref options] (-into-schema (-ref-schema {:lazy true}) nil [ref] options))

(defn -boolean-fn [x] (cond (boolean? x) (constantly x) (ifn? x) x :else (constantly false)))

(defn -infer [children]
  (loop [[[s f] & fs] [[:string string?] [:keyword keyword?] [:symbol symbol?] [:int int?] [:double float?]]]
    (if (every? f children) s (when fs (recur fs)))))

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
             ([f1 f2 f3 f4 f5 f6 f7 f8 & fs] (let [f9 (apply -comp fs)]
                                               (fn [x] (-> x f9 f8 f7 f6 f5 f4 f3 f2 f1))))]
      :cljs [([f1 f2 f3 & fs] (let [f4 (apply -comp fs)]
                                (fn [x] (-> x f4 f3 f2 f1))))]))

(defn -update [x k f] (assoc x k (f (get x k))))

(defn -equals [x y] (or (identical? x y) (= x y)))

(defn -vmap ([os] (miu/-vmap identity os)) ([f os] (miu/-vmap f os)))

(defn -memoize [f]
  (let [value #?(:clj (AtomicReference. nil), :cljs (atom nil))]
    (fn [] #?(:clj (or (.get value) (do (.set value (f)) (.get value))), :cljs (or @value (reset! value (f)))))))

(defn -group-by-arity! [infos]
  (let [aritys (atom #{})]
    (reduce
     (fn [acc {:keys [min arity] :as info}]
       (let [vararg (= :varargs arity)
             min (if (and vararg (@aritys min)) (inc (apply max (filter int? @aritys))) min)]
         (cond (and vararg (@aritys arity))
               (-fail! ::multiple-varargs {:infos infos})

               (@aritys min)
               (-fail! ::duplicate-arities {:infos infos})

               :else
               (do (swap! aritys conj arity)
                   (assoc acc arity (assoc info :min min)))))) {} infos)))

(defn- -re-min-max [f {min' :min, max' :max} child]
  (let [{min'' :min max'' :max} (-regex-min-max child true)]
    (cond-> {:min (f (or min' 0) min'')} (and max' max'') (assoc :max (f max' max'')))))

(defn- -re-alt-min-max [{min' :min, max' :max} child]
  (let [{min'' :min max'' :max} (-regex-min-max child true)]
    (cond-> {:min (min (or min' miu/+max-size+) min'')} (and max' max'') (assoc :max (max max' max'')))))

;;
;; registry
;;

(defn- -register-var [registry ?v]
  (let [[v pred] (if (vector? ?v) ?v [?v @?v])
        name (-> v meta :name)
        schema (-simple-schema {:type name, :pred pred})]
    (-> registry
        (assoc name schema)
        (assoc @v schema))))

(defn -registry {:arglists '([] [{:keys [registry]}])}
  ([] default-registry)
  ([opts] (or (when opts (mr/registry (opts :registry))) default-registry)))

(defn -property-registry [m options f]
  (let [options (assoc options ::allow-invalid-refs true)]
    (reduce-kv (fn [acc k v] (assoc acc k (f (schema v options)))) {} m)))

(defn -delayed-registry [m f]
  (reduce-kv (fn [acc k v] (assoc acc k (reify IntoSchema (-into-schema [_ _ _ options] (f v options))))) {} m))

(defn- -lookup [?schema options]
  (let [registry (-registry options)]
    (or (mr/-schema registry ?schema)
        (when-some [p (some-> registry (mr/-schema (c/type ?schema)))]
          (when (schema? ?schema)
            (when (= p (-parent ?schema))
              (-fail! ::infinitely-expanding-schema {:schema ?schema})))
          (-into-schema p nil [?schema] options)))))

(defn- -lookup! [?schema ?form f rec options]
  (or (and f (f ?schema) ?schema)
      (if-let [?schema (-lookup ?schema options)]
        (cond-> ?schema rec (recur ?form f rec options))
        (-fail! ::invalid-schema {:schema ?schema, :form ?form}))))

(defn -properties-and-options [properties options f]
  (if-let [r (:registry properties)]
    (let [options (-update options :registry #(mr/composite-registry r (or % (-registry options))))]
      [(assoc properties :registry (-property-registry r options f)) options])
    [properties options]))

;;
;; cache
;;

(defn -create-cache [_options] (atom {}))

(defn -cached [s k f]
  (if (-cached? s)
    (let [c (-cache s)]
      (or (@c k) ((swap! c assoc k (f s)) k)))
    (f s)))

;;
;; forms
;;

(defn -raw-form [type properties children]
  (let [has-children (seq children), has-properties (seq properties)]
    (cond (and has-properties has-children) (reduce conj [type properties] children)
          has-properties [type properties]
          has-children (let [fchild (nth children 0)]
                         (reduce conj
                                 (cond-> [type]
                                   (or (map? fchild)
                                       (nil? fchild)) (conj nil))
                                 children))
          :else type)))

(defn -create-form [type properties children options]
  (let [properties (when (seq properties)
                     (let [registry (:registry properties)]
                       (cond-> properties registry (assoc :registry (-property-registry registry options -form)))))]
    (-raw-form type properties children)))

(defn -simple-form [parent properties children f options]
  (-create-form (-type parent) properties (-vmap f children) options))

(defn -create-entry-form [parent properties entry-parser options]
  (-create-form (-type parent) properties (-entry-forms entry-parser) options))

;;
;; walkers
;;

(defn -inner-indexed [walker path children options]
  (-vmap (fn [[i c]] (-inner walker c (conj path i) options)) (map-indexed vector children)))

(defn -inner-entries [walker path entries options]
  (-vmap (fn [[k s]] [k (-properties s) (-inner walker s (conj path k) options)]) entries))

(defn -walk-entries [schema walker path options]
  (when (-accept walker schema path options)
    (-outer walker schema path (-inner-entries walker path (-entries schema) options) options)))

(defn -walk-indexed [schema walker path options]
  (when (-accept walker schema path options)
    (-outer walker schema path (-inner-indexed walker path (-children schema) options) options)))

(defn -walk-leaf [schema walker path options]
  (when (-accept walker schema path options)
    (-outer walker schema path (-children schema) options)))

;;
;; lenses
;;

(defn -set-children [schema children]
  (if (-equals children (-children schema))
    schema (-into-schema (-parent schema) (-properties schema) children (-options schema))))

(defn -set-properties [schema properties]
  (if (-equals properties (-properties schema))
    schema (-into-schema (-parent schema) properties (or (and (-entry-schema? schema) (-entry-parser schema)) (-children schema)) (-options schema))))

(defn -update-properties [schema f & args]
  (-set-properties schema (not-empty (apply f (-properties schema) args))))

(defn -update-options [schema f]
  (-into-schema (-parent schema) (-properties schema) (-children schema) (f (-options schema))))

(defn -set-assoc-children [schema key value]
  (-set-children schema (assoc (-children schema) key value)))

(defn -get-entries [schema key default]
  (or (some (if (and (vector? key) (= ::find (nth key 0)))
              (fn [e] (when (= (nth e 0) (nth key 1)) e))
              (fn [e] (when (= (nth e 0) key) (nth e 2))))
            (-children schema)) default))

;;
;; entries
;;

(defn -simple-entry-parser [keyset children forms]
  (let [entries (map (fn [[k p s]] (miu/-entry k (-val-schema s p))) children)]
    (reify EntryParser
      (-entry-keyset [_] keyset)
      (-entry-children [_] children)
      (-entry-entries [_] entries)
      (-entry-forms [_] forms))))

(defn- -update-parsed [entry-parser ?key value options]
  (let [[override k p] (if (and (vector? ?key) (nth ?key 0)) (cons true ?key) [false ?key])
        keyset (-entry-keyset entry-parser)
        children (-entry-children entry-parser)
        forms (-entry-forms entry-parser)
        s (when value (schema value options))
        i (:order (keyset k))]
    (if (nil? s)
      ;; remove
      (letfn [(cut [v] (into (subvec v 0 i) (subvec v (inc i))))]
        (-simple-entry-parser (dissoc keyset k) (cut children) (cut forms)))
      (let [p (if i (if override p (nth (children i) 1)) p)
            c [k p s]
            f (if (seq p) [k p (-form s)] [k (-form s)])]
        (if i
          ;; update
          (-simple-entry-parser keyset (assoc children i c) (assoc forms i f))
          ;; assoc
          (-simple-entry-parser (assoc keyset k {:order (count keyset)}) (conj children c) (conj forms f)))))))

(defn -set-entries
  ([schema ?key value]
   (if-let [entry-parser (-entry-parser schema)]
     (-set-children schema (-update-parsed entry-parser ?key value (-options schema)))
     (let [found (atom nil)
           [key props override] (if (vector? ?key) [(nth ?key 0) (second ?key) true] [?key])
           children (cond-> (-vmap (fn [[k p :as entry]]
                                     (if (= key k)
                                       (do (reset! found true) [key (if override props p) value])
                                       entry))
                                   (-children schema))
                      (not @found) (conj (if key [key props value] (-fail! ::key-missing)))
                      :always (->> (filter (fn [e] (-> e last some?)))))]
       (-set-children schema children)))))

(defn- -parse-entry [e naked-keys lazy-refs options i ^objects -children ^objects -forms ^objects -keyset]
  (letfn [(-collect [k c f i]
            (let [i (int i)]
              (aset -keyset (* 2 i) k)
              (aset -keyset (inc (* 2 i)) {:order i})
              (aset -children i c)
              (aset -forms i f)
              (unchecked-inc-int i)))
          (-schema [e] (schema (cond-> e (and (-reference? e) lazy-refs) (-lazy options)) options))
          (-parse-ref-entry [e]
            (let [s (-schema e)
                  c [e nil s]]
              (-collect e c e i)))
          (-parse-ref-vector1 [e e0]
            (let [s (-schema e0)
                  c [e0 nil s]]
              (-collect e0 c e i)))
          (-parse-ref-vector2 [e e0 e1]
            (let [s (-schema e0)
                  c [e0 e1 s]]
              (-collect e0 c e i)))
          (-parse-entry-else2 [e0 e1]
            (let [s (-schema e1)
                  f [e0 (-form s)]
                  c [e0 nil s]]
              (-collect e0 c f i)))
          (-parse-entry-else3 [e0 e1 e2]
            (let [s (-schema e2)
                  f' (-form s)
                  f (if e1 [e0 e1 f'] [e0 f'])
                  c [e0 e1 s]]
              (-collect e0 c f i)))]
    (if (vector? e)
      (let [ea (object-array e)
            n (alength ea)
            e0 (aget ea 0)]
        (if (== n 1)
          (if (and (-reference? e0) naked-keys)
            (-parse-ref-vector1 e e0)
            (-fail! ::invalid-entry {:entry e}))
          (let [e1 (aget ea 1)]
            (if (== n 2)
              (if (and (-reference? e0) (map? e1))
                (if naked-keys (-parse-ref-vector2 e e0 e1) i)
                (-parse-entry-else2 e0 e1))
              (let [e2 (aget ea 2)]
                (-parse-entry-else3 e0 e1 e2))))))
      (if (and naked-keys (-reference? e))
        (-parse-ref-entry e)
        (-fail! ::invalid-entry {:entry e})))))

(defn -eager-entry-parser [children props options]
  (letfn [(-vec [^objects arr] #?(:bb (vec arr) :clj (LazilyPersistentVector/createOwning arr), :cljs (vec arr)))
          (-map [^objects arr] #?(:bb   (let [m (apply array-map arr)]
                                          (when-not (= (* 2 (count m)) (count arr))
                                            (-fail! ::duplicate-keys {:arr arr})) m)
                                  :clj (try (PersistentArrayMap/createWithCheck arr)
                                            (catch Exception _ (-fail! ::duplicate-keys {:arr arr})))
                                  :cljs (let [m (apply array-map arr)]
                                          (when-not (= (* 2 (count m)) (count arr))
                                            (-fail! ::duplicate-keys {:arr arr})) m)))
          (-arange [^objects arr to]
           #?(:clj (let [-arr (object-array to)] (System/arraycopy arr 0 -arr 0 to) -arr)
              :cljs (.slice arr 0 to)))]
    (let [{:keys [naked-keys lazy-refs]} props
          ca (object-array children)
          n (alength ca)
          -children (object-array n)
          -forms (object-array n)
          -keyset (object-array (* 2 n))]
      (loop [i (int 0), ci (int 0)]
        (if (== ci n)
          (let [f (if (== ci i) -vec #(-vec (-arange % i)))]
            (-simple-entry-parser (-map -keyset) (f -children) (f -forms)))
          (recur (int (-parse-entry (aget ca i) naked-keys lazy-refs options i -children -forms -keyset))
                 (unchecked-inc-int ci)))))))

(defn -lazy-entry-parser [?children props options]
  (let [parser (delay (-eager-entry-parser ?children props options))]
    (reify EntryParser
      (-entry-keyset [_] (-entry-keyset @parser))
      (-entry-children [_] (-entry-children @parser))
      (-entry-entries [_] (-entry-entries @parser))
      (-entry-forms [_] (-entry-forms @parser)))))

(defn -create-entry-parser [?children props options]
  (cond (-entry-parser? ?children) ?children
        (or (:lazy props) (::lazy-entries options)) (-lazy-entry-parser ?children props options)
        :else (-eager-entry-parser ?children props options)))

(defn -default-entry [e] (-equals (nth e 0) ::default))
(defn -default-entry-schema [children] (some (fn [e] (when (-default-entry e) (nth e 2))) children))

;;
;; transformers
;;

(defn -no-op-transformer []
  (reify Transformer
    (-transformer-chain [_])
    (-value-transformer [_ _ _ _])))

(defn -intercepting
  ([interceptor] (-intercepting interceptor nil))
  ([{:keys [enter leave]} f] (some->> [leave f enter] (keep identity) (seq) (apply -comp))))

(defn -into-transformer [x]
  (cond
    (-transformer? x) x
    (fn? x) (-into-transformer (x))
    (nil? x) (-no-op-transformer)
    :else (-fail! ::invalid-transformer {:value x})))

(defn -parent-children-transformer [parent children transformer method options]
  (let [parent-transformer (-value-transformer transformer parent method options)
        child-transformers (into [] (keep #(-transformer % transformer method options)) children)
        child-transformer (when (seq child-transformers) (apply -comp (rseq child-transformers)))]
    (-intercepting parent-transformer child-transformer)))

(defn -map-transformer [ts]
  #?(:bb   (fn [x] (reduce (fn child-transformer [m [k t]]
                             (if-let [entry (find m k)]
                               (assoc m k (t (val entry)))
                               m)) x ts))
     :clj  (let [not-found (Object.)]
             (apply -comp (map (fn child-transformer [[k t]]
                                 (fn [^Associative x]
                                   (let [val (.valAt x k not-found)]
                                     (if (identical? val not-found)
                                       x (.assoc x k (t val)))))) (rseq ts))))
     :cljs (fn [x] (reduce (fn child-transformer [m [k t]]
                             (if-let [entry (find m k)]
                               (assoc m k (t (val entry)))
                               m)) x ts))))

(defn -tuple-transformer [ts] (fn [x] (reduce-kv -update x ts)))

(defn -collection-transformer [t empty]
  #?(:bb   (fn [x] (into (when x empty) (map t) x))
     :clj  (fn [x] (let [i (.iterator ^Iterable x)]
                     (loop [x ^IPersistentCollection empty]
                       (if (.hasNext i)
                         (recur (.cons x (t (.next i))))
                         x))))
     :cljs (fn [x] (into (when x empty) (map t) x))))

(defn -or-transformer [this transformer child-schemas method options]
  (let [this-transformer (-value-transformer transformer this method options)]
    (if (seq child-schemas)
      (let [transformers (-vmap #(or (-transformer % transformer method options) identity) child-schemas)
            validators (-vmap -validator child-schemas)]
        (-intercepting this-transformer
                       (if (= :decode method)
                         (fn [x]
                           (reduce-kv
                            (fn [acc i transformer]
                              (let [x* (transformer x)]
                                (if ((nth validators i) x*)
                                  (reduced x*)
                                  (if (-equals acc ::nil) x* acc))))
                            ::nil transformers))
                         (fn [x]
                           (reduce-kv
                            (fn [x i validator] (if (validator x) (reduced ((nth transformers i) x)) x))
                            x validators)))))
      (-intercepting this-transformer))))

;;
;; ast
;;

(defn -parse-entry-ast [ast options]
  (let [ast-entry-order (::ast-entry-order options)
        keyset (:keys ast)
        ->child (fn [[k v]] [k (:properties v) (from-ast (:value v) options)])
        children (delay (-vmap ->child (cond->> keyset ast-entry-order (sort-by #(:order (val %)) keyset))))]
    (reify EntryParser
      (-entry-keyset [_] keyset)
      (-entry-children [_] @children)
      (-entry-entries [_] (-vmap (fn [[k p s]] (miu/-entry k (-val-schema s p))) @children))
      (-entry-forms [_] (->> @children (-vmap (fn [[k p v]] (if p [k p (-form v)] [k (-form v)]))))))))

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
  (-into-schema parent (:properties ast) (when-let [value (:value ast)] [value]) options))

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
      (and (and min max) f) (fn [x] (let [size (f x)]
                                      (and (<= min size) (<= size max))))
      (and min max) (fn [x] (and (<= min x) (<= x max)))
      (and min f) (fn [x] (<= min (f x)))
      min (fn [x] (<= min x))
      (and max f) (fn [x] (<= (f x) max))
      max (fn [x] (<= x max)))))

(defn- -safe-count [x]
  (if (-safely-countable? x)
    (count x)
    (reduce (fn [cnt _] (inc cnt)) 0 x)))

(defn -validate-limits [min max] (or ((-min-max-pred -safe-count) {:min min :max max}) (constantly true)))

(defn -needed-bounded-checks [min max options]
  (c/max (or (some-> max inc) 0)
         (or min 0)
         (::coll-check-limit options 101)))

(defn -validate-bounded-limits [needed min max]
  (or ((-min-max-pred #(bounded-count needed %)) {:min min :max max}) (constantly true)))

(defn -qualified-keyword-pred [properties]
  (when-let [ns-name (some-> properties :namespace name)]
    (fn [x] (= (namespace x) ns-name))))

;;
;; Schemas
;;

(defn -simple-schema [props]
  (let [{:keys [type type-properties pred property-pred min max from-ast to-ast compile]
         :or {min 0, max 0, from-ast -from-value-ast, to-ast -to-type-ast}} props]
    (if (fn? props)
      (do
        (-deprecated! "-simple-schema doesn't take fn-props, use :compile property instead")
        (-simple-schema {:compile (fn [c p _] (props c p))}))
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
          (if compile
            (-into-schema (-simple-schema (merge (dissoc props :compile) (compile properties children options))) properties children options)
            (let [form (delay (-simple-form parent properties children identity options))
                  cache (-create-cache options)]
              (-check-children! type properties children min max)
              ^{:type ::schema}
              (reify
                AST
                (-to-ast [this _] (to-ast this))
                Schema
                (-validator [_]
                  (if-let [pvalidator (when property-pred (property-pred properties))]
                    (fn [x] (and (pred x) (pvalidator x))) pred))
                (-explainer [this path]
                  (let [validator (-validator this)]
                    (fn explain [x in acc]
                      (if-not (validator x) (conj acc (miu/-error path in this x)) acc))))
                (-parser [this]
                  (let [validator (-validator this)]
                    (fn [x] (if (validator x) x ::invalid))))
                (-unparser [this] (-parser this))
                (-transformer [this transformer method options]
                  (-intercepting (-value-transformer transformer this method options)))
                (-walk [this walker path options] (-walk-leaf this walker path options))
                (-properties [_] properties)
                (-options [_] options)
                (-children [_] children)
                (-parent [_] parent)
                (-form [_] @form)
                Cached
                (-cache [_] cache)
                LensSchema
                (-keep [_])
                (-get [_ _ default] default)
                (-set [this key _] (-fail! ::non-associative-schema {:schema this, :key key}))
                #?@(:cljs [IPrintWithWriter (-pr-writer [this writer opts] (pr-writer-schema this writer opts))])))))
        #?@(:cljs [IPrintWithWriter (-pr-writer [this writer opts] (pr-writer-into-schema this writer opts))])))))

(defn -nil-schema [] (-simple-schema {:type :nil, :pred nil?}))
(defn -any-schema [] (-simple-schema {:type :any, :pred any?}))
(defn -some-schema [] (-simple-schema {:type :some, :pred some?}))
(defn -string-schema [] (-simple-schema {:type :string, :pred string?, :property-pred (-min-max-pred count)}))
(defn -int-schema [] (-simple-schema {:type :int, :pred int?, :property-pred (-min-max-pred nil)}))
(defn -float-schema [] (-simple-schema {:type :float, :pred float?, :property-pred (-min-max-pred nil)}))
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
      (-check-children! :and properties children 1 nil)
      (let [children (-vmap #(schema % options) children)
            form (delay (-simple-form parent properties children -form options))
            cache (-create-cache options)
            ->parser (fn [f m] (let [parsers (m (-vmap f children))]
                                 #(reduce (fn [x parser] (miu/-map-invalid reduced (parser x))) % parsers)))]
        ^{:type ::schema}
        (reify
          Schema
          (-validator [_]
            (let [validators (-vmap -validator children)] (miu/-every-pred validators)))
          (-explainer [_ path]
            (let [explainers (-vmap (fn [[i c]] (-explainer c (conj path i))) (map-indexed vector children))]
              (fn explain [x in acc] (reduce (fn [acc' explainer] (explainer x in acc')) acc explainers))))
          (-parser [_] (->parser -parser seq))
          (-unparser [_] (->parser -unparser rseq))
          (-transformer [this transformer method options]
            (-parent-children-transformer this children transformer method options))
          (-walk [this walker path options] (-walk-indexed this walker path options))
          (-properties [_] properties)
          (-options [_] options)
          (-children [_] children)
          (-parent [_] parent)
          (-form [_] @form)
          Cached
          (-cache [_] cache)
          LensSchema
          (-keep [_])
          (-get [_ key default] (get children key default))
          (-set [this key value] (-set-assoc-children this key value))
          #?@(:cljs [IPrintWithWriter (-pr-writer [this writer opts] (pr-writer-schema this writer opts))]))))
    #?@(:cljs [IPrintWithWriter (-pr-writer [this writer opts] (pr-writer-into-schema this writer opts))])))

(defn -or-schema []
  ^{:type ::into-schema}
  (reify IntoSchema
    (-type [_] :or)
    (-type-properties [_])
    (-properties-schema [_ _])
    (-children-schema [_ _])
    (-into-schema [parent properties children options]
      (-check-children! :or properties children 1 nil)
      (let [children (-vmap #(schema % options) children)
            form (delay (-simple-form parent properties children -form options))
            cache (-create-cache options)
            ->parser (fn [f] (let [parsers (-vmap f children)]
                               #(reduce (fn [_ parser] (miu/-map-valid reduced (parser %))) ::invalid parsers)))]
        ^{:type ::schema}
        (reify
          Schema
          (-validator [_]
            (let [validators (-vmap -validator children)] (miu/-some-pred validators)))
          (-explainer [_ path]
            (let [explainers (-vmap (fn [[i c]] (-explainer c (conj path i))) (map-indexed vector children))]
              (fn explain [x in acc]
                (reduce
                 (fn [acc' explainer]
                   (let [acc'' (explainer x in acc')]
                     (if (identical? acc' acc'') (reduced acc) acc'')))
                 acc explainers))))
          (-parser [_] (->parser -parser))
          (-unparser [_] (->parser -unparser))
          (-transformer [this transformer method options]
            (-or-transformer this transformer children method options))
          (-walk [this walker path options] (-walk-indexed this walker path options))
          (-properties [_] properties)
          (-options [_] options)
          (-children [_] children)
          (-parent [_] parent)
          (-form [_] @form)
          Cached
          (-cache [_] cache)
          LensSchema
          (-keep [_])
          (-get [_ key default] (get children key default))
          (-set [this key value] (-set-assoc-children this key value))
          #?@(:cljs [IPrintWithWriter (-pr-writer [this writer opts] (pr-writer-schema this writer opts))]))))
    #?@(:cljs [IPrintWithWriter (-pr-writer [this writer opts] (pr-writer-into-schema this writer opts))])))

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
      (let [entry-parser (-create-entry-parser children {:naked-keys true} options)
            form (delay (-create-entry-form parent properties entry-parser options))
            cache (-create-cache options)]
        ^{:type ::schema}
        (reify
          AST
          (-to-ast [this _] (-entry-ast this (-entry-keyset entry-parser)))
          Schema
          (-validator [this] (miu/-some-pred (-vmap (fn [[_ _ c]] (-validator c)) (-children this))))
          (-explainer [this path]
            (let [explainers (-vmap (fn [[k _ c]] (-explainer c (conj path k))) (-children this))]
              (fn explain [x in acc]
                (reduce
                 (fn [acc' explainer]
                   (let [acc'' (explainer x in acc')]
                     (if (identical? acc' acc'') (reduced acc) acc'')))
                 acc explainers))))
          (-parser [this]
            (let [parsers (-vmap (fn [[k _ c]]
                                   (let [c (-parser c)]
                                     (fn [x] (miu/-map-valid #(reduced (tag k %)) (c x)))))
                                 (-children this))]
              (fn [x] (reduce (fn [_ parser] (parser x)) x parsers))))
          (-unparser [this]
            (let [unparsers (into {} (map (fn [[k _ c]] [k (-unparser c)])) (-children this))]
              (fn [x]
                (if (tag? x)
                  (if-some [unparse (get unparsers (:key x))]
                    (unparse (:value x))
                    ::invalid)
                  ::invalid))))
          (-transformer [this transformer method options]
            (-or-transformer this transformer (-vmap #(nth % 2) (-children this)) method options))
          (-walk [this walker path options] (-walk-entries this walker path options))
          (-properties [_] properties)
          (-options [_] options)
          (-children [_] (-entry-children entry-parser))
          (-parent [_] parent)
          (-form [_] @form)
          EntrySchema
          (-entries [_] (-entry-entries entry-parser))
          (-entry-parser [_] entry-parser)
          Cached
          (-cache [_] cache)
          LensSchema
          (-keep [_])
          (-get [this key default] (-get-entries this key default))
          (-set [this key value] (-set-entries this key value))
          #?@(:cljs [IPrintWithWriter (-pr-writer [this writer opts] (pr-writer-schema this writer opts))]))))
    #?@(:cljs [IPrintWithWriter (-pr-writer [this writer opts] (pr-writer-into-schema this writer opts))])))

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
      (let [[schema :as children] (-vmap #(schema % options) children)
            form (delay (-simple-form parent properties children -form options))
            cache (-create-cache options)]
        ^{:type ::schema}
        (reify
          AST
          (-to-ast [this _] (-to-child-ast this))
          Schema
          (-validator [_] (complement (-validator schema)))
          (-explainer [this path]
            (let [validator (-validator this)]
              (fn explain [x in acc]
                (if-not (validator x) (conj acc (miu/-error (conj path 0) in this x)) acc))))
          (-parser [this]
            (let [validator (-validator this)]
              (fn [x] (if (validator x) x ::invalid))))
          (-unparser [this] (-parser this))
          (-transformer [this transformer method options]
            (-parent-children-transformer this children transformer method options))
          (-walk [this walker path options] (-walk-indexed this walker path options))
          (-properties [_] properties)
          (-options [_] options)
          (-children [_] children)
          (-parent [_] parent)
          (-form [_] @form)
          Cached
          (-cache [_] cache)
          LensSchema
          (-keep [_])
          (-get [_ key default] (get children key default))
          (-set [this key value] (-set-assoc-children this key value))
          #?@(:cljs [IPrintWithWriter (-pr-writer [this writer opts] (pr-writer-schema this writer opts))]))))
    #?@(:cljs [IPrintWithWriter (-pr-writer [this writer opts] (pr-writer-into-schema this writer opts))])))

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
       (let [children (-vmap #(schema % options) children)
             form (delay (-simple-form parent properties children -form options))
             schema (first children)
             cache (-create-cache options)]
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
               (when (-accept walker this path options)
                 (-outer walker this path (list (-inner walker schema path options)) options))
               (-walk schema walker path options)))
           (-properties [_] properties)
           (-options [_] (-options schema))
           (-children [_] [schema])
           (-parent [_] parent)
           (-form [_] @form)
           Cached
           (-cache [_] cache)
           LensSchema
           (-keep [_])
           (-get [_ key default] (if (= 0 key) schema default))
           (-set [_ key value] (when (= 0 key) (-val-schema value properties)))
           RefSchema
           (-ref [_])
           (-deref [_] schema)
           #?@(:cljs [IPrintWithWriter (-pr-writer [this writer opts] (pr-writer-schema this writer opts))]))))
     #?@(:cljs [IPrintWithWriter (-pr-writer [this writer opts] (pr-writer-into-schema this writer opts))]))))

(defn -map-schema
  ([]
   (-map-schema {:naked-keys true}))
  ([opts] ;; :naked-keys, :lazy, :pred
   ^{:type ::into-schema}
   (reify
     AST
     (-from-ast [parent ast options] (-from-entry-ast parent ast options))
     IntoSchema
     (-type [_] (:type opts :map))
     (-type-properties [_] (:type-properties opts))
     (-properties-schema [_ _])
     (-children-schema [_ _])
     (-into-schema [parent {:keys [closed] :as properties} children options]
       (let [pred? (:pred opts map?)
             entry-parser (-create-entry-parser children opts options)
             form (delay (-create-entry-form parent properties entry-parser options))
             cache (-create-cache options)
             default-schema (delay (some-> entry-parser (-entry-children) (-default-entry-schema) (schema options)))
             explicit-children (delay (cond->> (-entry-children entry-parser) @default-schema (remove -default-entry)))
             ->parser (fn [this f]
                        (let [keyset (-entry-keyset (-entry-parser this))
                              default-parser (some-> @default-schema (f))
                              ;; prevent unparsing :catn/:orn/etc parse results as maps
                              ok? #(and (pred? %) (not (tag? %)) (not (tags? %)))
                              parsers (cond->> (-vmap
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
                                                @explicit-children)
                                        default-parser
                                        (cons (fn [m]
                                                (let [m' (default-parser
                                                          (reduce (fn [acc k] (dissoc acc k)) m (keys keyset)))]
                                                  (if (miu/-invalid? m')
                                                    (reduced m')
                                                    (merge (select-keys m (keys keyset)) m')))))
                                        closed
                                        (cons (fn [m]
                                                (reduce
                                                 (fn [m k] (if (contains? keyset k) m (reduced (reduced ::invalid))))
                                                 m (keys m)))))]
                          (fn [x] (if (ok? x) (reduce (fn [m parser] (parser m)) x parsers) ::invalid))))]
         ^{:type ::schema}
         (reify
           AST
           (-to-ast [this _] (-entry-ast this (-entry-keyset entry-parser)))
           Schema
           (-validator [this]
             (let [keyset (-entry-keyset (-entry-parser this))
                   default-validator (some-> @default-schema (-validator))
                   validators (cond-> (-vmap
                                       (fn [[key {:keys [optional]} value]]
                                         (let [valid? (-validator value)
                                               default (boolean optional)]
                                           #?(:bb   (fn [m] (if-let [map-entry (find m key)] (valid? (val map-entry)) default))
                                              :clj  (let [not-found (Object.)]
                                                      (fn [^Associative m]
                                                        (let [val (.valAt m key not-found)]
                                                          (if (identical? val not-found)
                                                            default
                                                            (valid? val)))))
                                              :cljs (fn [m] (if-let [map-entry (find m key)] (valid? (val map-entry)) default)))))
                                       @explicit-children)
                                default-validator
                                (conj (fn [m] (default-validator (reduce (fn [acc k] (dissoc acc k)) m (keys keyset)))))
                                (and closed (not default-validator))
                                (conj (fn [m] (reduce (fn [acc k] (if (contains? keyset k) acc (reduced false))) true (keys m)))))
                   validate (miu/-every-pred validators)]
               (fn [m] (and (pred? m) (validate m)))))
           (-explainer [this path]
             (let [keyset (-entry-keyset (-entry-parser this))
                   default-explainer (some-> @default-schema (-explainer (conj path ::default)))
                   explainers (cond-> (-vmap
                                       (fn [[key {:keys [optional]} schema]]
                                         (let [explainer (-explainer schema (conj path key))]
                                           (fn [x in acc]
                                             (if-let [e (find x key)]
                                               (explainer (val e) (conj in key) acc)
                                               (if-not optional
                                                 (conj acc (miu/-error (conj path key) (conj in key) this nil ::missing-key))
                                                 acc)))))
                                       @explicit-children)
                                default-explainer
                                (conj (fn [x in acc]
                                        (default-explainer
                                         (reduce (fn [acc k] (dissoc acc k)) x (keys keyset))
                                         in acc)))
                                (and closed (not default-explainer))
                                (conj (fn [x in acc]
                                        (reduce-kv
                                         (fn [acc k v]
                                           (if (contains? keyset k)
                                             acc
                                             (conj acc (miu/-error (conj path k) (conj in k) this v ::extra-key))))
                                         acc x))))]
               (fn [x in acc]
                 (if-not (pred? x)
                   (conj acc (miu/-error path in this x ::invalid-type))
                   (reduce
                    (fn [acc explainer]
                      (explainer x in acc))
                    acc explainers)))))
           (-parser [this] (->parser this -parser))
           (-unparser [this] (->parser this -unparser))
           (-transformer [this transformer method options]
             (let [keyset (-entry-keyset (-entry-parser this))
                   this-transformer (-value-transformer transformer this method options)
                   ->children (reduce (fn [acc [k s]]
                                        (let [t (-transformer s transformer method options)]
                                          (cond-> acc t (conj [k t]))))
                                      [] (cond->> (-entries this) @default-schema (remove -default-entry)))
                   apply->children (when (seq ->children) (-map-transformer ->children))
                   apply->default (when-let [dt (some-> @default-schema (-transformer transformer method options))]
                                    (fn [x] (merge (dt (reduce (fn [acc k] (dissoc acc k)) x (keys keyset))) (select-keys x (keys keyset)))))
                   apply->children (some->> [apply->default apply->children] (keep identity) (seq) (apply -comp))
                   apply->children (-guard pred? apply->children)]
               (-intercepting this-transformer apply->children)))
           (-walk [this walker path options] (-walk-entries this walker path options))
           (-properties [_] properties)
           (-options [_] options)
           (-children [_] (-entry-children entry-parser))
           (-parent [_] parent)
           (-form [_] @form)
           EntrySchema
           (-entries [_] (-entry-entries entry-parser))
           (-entry-parser [_] entry-parser)
           Cached
           (-cache [_] cache)
           LensSchema
           (-keep [_] true)
           (-get [this key default] (-get-entries this key default))
           (-set [this key value] (-set-entries this key value))
           #?@(:cljs [IPrintWithWriter (-pr-writer [this writer opts] (pr-writer-schema this writer opts))]))))
     #?@(:cljs [IPrintWithWriter (-pr-writer [this writer opts] (pr-writer-into-schema this writer opts))]))))

(defn -map-of-schema
  ([]
   (-map-of-schema {}))
  ([opts]
   ^{:type ::into-schema}
   (reify
     AST
     (-from-ast [parent ast options]
       (-into-schema parent (:properties ast) [(from-ast (:key ast) options) (from-ast (:value ast) options)] options))
     IntoSchema
     (-type [_] (:type opts :map-of))
     (-type-properties [_] (:type-properties opts))
     (-properties-schema [_ _])
     (-children-schema [_ _])
     (-into-schema [parent {:keys [min max] :as properties} children options]
       (-check-children! :map-of properties children 2 2)
       (let [[key-schema value-schema :as children] (-vmap #(schema % options) children)
             form (delay (-simple-form parent properties children -form options))
             cache (-create-cache options)
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
           (-to-ast [_ _]
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
           (-walk [this walker path options] (-walk-indexed this walker path options))
           (-properties [_] properties)
           (-options [_] options)
           (-children [_] children)
           (-parent [_] parent)
           (-form [_] @form)
           Cached
           (-cache [_] cache)
           LensSchema
           (-keep [_])
           (-get [_ key default] (get children key default))
           (-set [this key value] (-set-assoc-children this key value))
           #?@(:cljs [IPrintWithWriter (-pr-writer [this writer opts] (pr-writer-schema this writer opts))]))))
     #?@(:cljs [IPrintWithWriter (-pr-writer [this writer opts] (pr-writer-into-schema this writer opts))]))))

;; also doubles as a predicate for the :every schema to bound the number
;; of elements to check, so don't add potentially-infinite countable things like seq's.
(defn- -safely-countable? [x]
  (or (nil? x)
      (counted? x)
      (indexed? x)
      ;; note: js/Object not ISeqable
      #?(:clj (instance? java.util.Map x))
      ;; many Seq's are List's, so just pick some popular classes
      #?@(:bb  []
          :clj [(instance? java.util.AbstractList x)
                (instance? java.util.Vector x)])
      #?(:clj  (instance? CharSequence x)
         :cljs (string? x))
      #?(:clj  (.isArray (class x))
         :cljs (identical? js/Array (c/type x)))))

(defn -collection-schema [props]
  (if (fn? props)
    (do (-deprecated! "-collection-schema doesn't take fn-props, use :compiled property instead")
        (-collection-schema {:compile (fn [c p _] (props c p))}))
    ^{:type ::into-schema}
    (reify
      AST
      (-from-ast [parent ast options] (-from-child-ast parent ast options))
      IntoSchema
      (-type [_] (:type props))
      (-type-properties [_] (:type-properties props))
      (-properties-schema [_ _])
      (-children-schema [_ _])
      (-into-schema [parent {:keys [min max] :as properties} children options]
        (if-let [compile (:compile props)]
          (-into-schema (-collection-schema (merge (dissoc props :compile) (compile properties children options))) properties children options)
          (let [{:keys [type parse unparse], fpred :pred, fempty :empty, fin :in :or {fin (fn [i _] i)}} props]
            (-check-children! type properties children 1 1)
            (let [[schema :as children] (-vmap #(schema % options) children)
                  form (delay (-simple-form parent properties children -form options))
                  cache (-create-cache options)
                  bounded (when (:bounded props)
                            (when fempty
                              (-fail! ::cannot-provide-empty-and-bounded-props))
                            (-needed-bounded-checks min max options))
                  validate-limits (if bounded
                                    (-validate-bounded-limits (c/min bounded (or max bounded)) min max)
                                    (-validate-limits min max))
                  ->parser (fn [f g] (let [child-parser (f schema)]
                                       (fn [x]
                                         (cond
                                           (not (fpred x)) ::invalid
                                           (not (validate-limits x)) ::invalid
                                           :else (if bounded
                                                   (let [child-validator child-parser]
                                                     (reduce
                                                      (fn [x v]
                                                        (if (child-validator v) x (reduced ::invalid)))
                                                      x (cond->> x
                                                          (not (-safely-countable? x))
                                                          (eduction (take bounded)))))
                                                   (let [x' (reduce
                                                             (fn [acc v]
                                                               (let [v' (child-parser v)]
                                                                 (if (miu/-invalid? v') (reduced ::invalid) (conj acc v'))))
                                                             [] x)]
                                                     (cond
                                                       (miu/-invalid? x') x'
                                                       g (g x')
                                                       fempty (into fempty x')
                                                       :else x')))))))]
              ^{:type ::schema}
              (reify
                AST
                (-to-ast [this _] (-to-child-ast this))
                Schema
                (-validator [_]
                  (let [validator (-validator schema)]
                    (fn [x] (and (fpred x)
                                 (validate-limits x)
                                 (reduce (fn [acc v] (if (validator v) acc (reduced false))) true
                                         (cond->> x
                                           (and bounded (not (-safely-countable? x)))
                                           (eduction (take bounded))))))))
                (-explainer [this path]
                  (let [explainer (-explainer schema (conj path 0))]
                    (fn [x in acc]
                      (cond
                        (not (fpred x)) (conj acc (miu/-error path in this x ::invalid-type))
                        (not (validate-limits x)) (conj acc (miu/-error path in this x ::limits))
                        :else (let [size (when (and bounded (not (-safely-countable? x)))
                                           bounded)]
                                (loop [acc acc, i 0, [x & xs :as ne] (seq x)]
                                  (if (and ne (or (not size) (< i #?(:cljs    ^number size
                                                                     :default size))))
                                    (cond-> (or (explainer x (conj in (fin i x)) acc) acc) xs (recur (inc i) xs))
                                    acc)))))))
                (-parser [_] (->parser (if bounded -validator -parser) (if bounded identity parse)))
                (-unparser [_] (->parser (if bounded -validator -unparser) (if bounded identity unparse)))
                (-transformer [this transformer method options]
                  (let [collection? #(or (sequential? %) (set? %))
                        this-transformer (-value-transformer transformer this method options)
                        child-transformer (-transformer schema transformer method options)
                        ->child (when child-transformer
                                  (if fempty
                                    (-collection-transformer child-transformer fempty)
                                    #(-vmap child-transformer %)))
                        ->child (-guard collection? ->child)]
                    (-intercepting this-transformer ->child)))
                (-walk [this walker path options]
                  (when (-accept walker this path options)
                    (-outer walker this path [(-inner walker schema (conj path ::in) options)] options)))
                (-properties [_] properties)
                (-options [_] options)
                (-children [_] children)
                (-parent [_] parent)
                (-form [_] @form)
                Cached
                (-cache [_] cache)
                LensSchema
                (-keep [_] true)
                (-get [_ _ _] schema)
                (-set [this _ value] (-set-children this [value]))
               #?@(:cljs [IPrintWithWriter (-pr-writer [this writer opts] (pr-writer-schema this writer opts))]))))))
      #?@(:cljs [IPrintWithWriter (-pr-writer [this writer opts] (pr-writer-into-schema this writer opts))]))))

(defn -tuple-schema
  ([]
   (-tuple-schema {}))
  ([opts]
   ^{:type ::into-schema}
   (reify IntoSchema
     (-type [_] :tuple)
     (-type-properties [_] (:type-properties opts))
     (-properties-schema [_ _])
     (-children-schema [_ _])
     (-into-schema [parent properties children options]
       (let [children (-vmap #(schema % options) children)
             form (delay (-simple-form parent properties children -form options))
             size (count children)
             cache (-create-cache options)
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
             (let [explainers (-vmap (fn [[i s]] (-explainer s (conj path i))) (map-indexed vector children))]
               (fn [x in acc]
                 (cond
                   (not (vector? x)) (conj acc (miu/-error path in this x ::invalid-type))
                   (not= (count x) size) (conj acc (miu/-error path in this x ::tuple-size))
                   :else (if (zero? size)
                           acc
                           (loop [acc acc, i 0, [x & xs] x, [e & es] explainers]
                             (cond-> (e x (conj in i) acc) xs (recur (inc i) xs es))))))))
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
           (-walk [this walker path options] (-walk-indexed this walker path options))
           (-properties [_] properties)
           (-options [_] options)
           (-children [_] children)
           (-parent [_] parent)
           (-form [_] @form)
           Cached
           (-cache [_] cache)
           LensSchema
           (-keep [_] true)
           (-get [_ key default] (get children key default))
           (-set [this key value] (-set-assoc-children this key value))
           #?@(:cljs [IPrintWithWriter (-pr-writer [this writer opts] (pr-writer-schema this writer opts))]))))
     #?@(:cljs [IPrintWithWriter (-pr-writer [this writer opts] (pr-writer-into-schema this writer opts))]))))

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
            form (delay (-simple-form parent properties children identity options))
            cache (-create-cache options)]
        ^{:type ::schema}
        (reify
          AST
          (-to-ast [_ _] (-ast {:type :enum :values children} properties options))
          Schema
          (-validator [_]
            (fn [x] (contains? schema x)))
          (-explainer [this path]
            (let [validator (-validator this)]
              (fn explain [x in acc]
                (if-not (validator x) (conj acc (miu/-error path in this x)) acc))))
          (-parser [_] (fn [x] (if (contains? schema x) x ::invalid)))
          (-unparser [this] (-parser this))
          ;; TODO: should we try to derive the type from values? e.g. [:enum 1 2] ~> int?
          (-transformer [this transformer method options]
            (-intercepting (-value-transformer transformer this method options)))
          (-walk [this walker path options] (-walk-leaf this walker path options))
          (-properties [_] properties)
          (-options [_] options)
          (-children [_] children)
          (-parent [_] parent)
          (-form [_] @form)
          Cached
          (-cache [_] cache)
          LensSchema
          (-keep [_])
          (-get [_ key default] (get children key default))
          (-set [this key value] (-set-assoc-children this key value))
          #?@(:cljs [IPrintWithWriter (-pr-writer [this writer opts] (pr-writer-schema this writer opts))]))))
    #?@(:cljs [IPrintWithWriter (-pr-writer [this writer opts] (pr-writer-into-schema this writer opts))])))

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
            matches? #(and #?(:clj (instance? CharSequence %), :cljs (string? %))
                           (re-find re %))
            form (delay (if class? re (-simple-form parent properties children identity options)))
            cache (-create-cache options)]
        ^{:type ::schema}
        (reify
          AST
          (-to-ast [this _] (-to-value-ast this))
          Schema
          (-validator [_]
            (-safe-pred matches?))
          (-explainer [this path]
            (fn explain [x in acc]
              (try
                (if-not (matches? x)
                  (conj acc (miu/-error path in this x))
                  acc)
                (catch #?(:clj Exception, :cljs js/Error) e
                  (conj acc (miu/-error path in this x (:type (ex-data e))))))))
          (-transformer [this transformer method options]
            (-intercepting (-value-transformer transformer this method options)))
          (-parser [this]
            (let [valid? (-validator this)]
              (fn [x] (if (valid? x) x ::invalid))))
          (-unparser [this] (-parser this))
          (-walk [this walker path options] (-walk-leaf this walker path options))
          (-properties [_] properties)
          (-options [_] options)
          (-children [_] children)
          (-parent [_] parent)
          (-form [_] @form)
          Cached
          (-cache [_] cache)
          LensSchema
          (-keep [_])
          (-get [_ key default] (get children key default))
          (-set [this key value] (-set-assoc-children this key value))
          #?@(:cljs [IPrintWithWriter (-pr-writer [this writer opts] (pr-writer-schema this writer opts))]))))
    #?@(:cljs [IPrintWithWriter (-pr-writer [this writer opts] (pr-writer-into-schema this writer opts))])))

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
            form (delay (-simple-form parent properties children identity options))
            cache (-create-cache options)]
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
          (-walk [this walker path options] (-walk-leaf this walker path options))
          (-properties [_] properties)
          (-options [_] options)
          (-children [_] children)
          (-parent [_] parent)
          (-form [_] @form)
          Cached
          (-cache [_] cache)
          LensSchema
          (-keep [_])
          (-get [_ key default] (get children key default))
          (-set [this key value] (-set-assoc-children this key value))
          #?@(:cljs [IPrintWithWriter (-pr-writer [this writer opts] (pr-writer-schema this writer opts))]))))
    #?@(:cljs [IPrintWithWriter (-pr-writer [this writer opts] (pr-writer-into-schema this writer opts))])))

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
      (let [[schema :as children] (-vmap #(schema % options) children)
            form (delay (-simple-form parent properties children -form options))
            cache (-create-cache options)
            ->parser (fn [f] (let [parser (f schema)] (fn [x] (if (nil? x) x (parser x)))))]
        ^{:type ::schema}
        (reify
          AST
          (-to-ast [this _] (-to-child-ast this))
          Schema
          (-validator [_]
            (let [validator (-validator schema)]
              (fn [x] (or (nil? x) (validator x)))))
          (-explainer [_ path]
            (let [explainer (-explainer schema (conj path 0))]
              (fn explain [x in acc]
                (if (nil? x) acc (explainer x in acc)))))
          (-parser [_] (->parser -parser))
          (-unparser [_] (->parser -unparser))
          (-transformer [this transformer method options]
            (-parent-children-transformer this children transformer method options))
          (-walk [this walker path options] (-walk-indexed this walker path options))
          (-properties [_] properties)
          (-options [_] options)
          (-children [_] children)
          (-parent [_] parent)
          (-form [_] @form)
          Cached
          (-cache [_] cache)
          LensSchema
          (-keep [_])
          (-get [_ key default] (if (= 0 key) schema default))
          (-set [this key value] (if (= 0 key)
                                   (-set-children this [value])
                                   (-fail! ::index-out-of-bounds {:schema this, :key key})))
          #?@(:cljs [IPrintWithWriter (-pr-writer [this writer opts] (pr-writer-schema this writer opts))]))))
    #?@(:cljs [IPrintWithWriter (-pr-writer [this writer opts] (pr-writer-into-schema this writer opts))])))

(defn -multi-schema
  ([]
   (-multi-schema {:naked-keys true}))
  ([opts]
   ^{:type ::into-schema}
   (reify
     AST
     (-from-ast [parent ast options] (-from-entry-ast parent ast options))
     IntoSchema
     (-type [_] (or (:type opts) :multi))
     (-type-properties [_] (:type-properties opts))
     (-properties-schema [_ _])
     (-children-schema [_ _])
     (-into-schema [parent properties children options]
       (let [opts' (merge opts (select-keys properties [:lazy-refs]))
             entry-parser (-create-entry-parser children opts' options)
             form (delay (-create-entry-form parent properties entry-parser options))
             cache (-create-cache options)
             dispatch (eval (:dispatch properties) options)
             dispatch-map (delay (into {} (-entry-entries entry-parser)))
             finder (fn [{:keys [::default] :as m}] (fn [x] (m x default)))]
         (when-not dispatch
           (-fail! ::missing-property {:key :dispatch}))
         ^{:type ::schema}
         (reify
           AST
           (-to-ast [this _] (-entry-ast this (-entry-keyset entry-parser)))
           DistributiveSchema
           (-distributive-schema? [_] true)
           (-distribute-to-children [this f _]
             (-into-schema parent
                           properties
                           (mapv (fn [c] (update c 2 f options)) (-children this))
                           options))
           Schema
           (-validator [_]
             (let [find (finder (reduce-kv (fn [acc k s] (assoc acc k (-validator s))) {} @dispatch-map))]
               (fn [x] (if-let [validator (find (dispatch x))] (validator x) false))))
           (-explainer [this path]
             (let [find (finder (reduce (fn [acc [k s]] (assoc acc k (-explainer s (conj path k)))) {} (-entries this)))]
               (fn [x in acc]
                 (if-let [explainer (find (dispatch x))]
                   (explainer x in acc)
                   (let [->path (if (and (map? x) (keyword? dispatch)) #(conj % dispatch) identity)]
                     (conj acc (miu/-error (->path path) (->path in) this x ::invalid-dispatch-value)))))))
           (-parser [_]
             (let [parse (fn [k s] (let [p (-parser s)] (fn [x] (miu/-map-valid #(tag k %) (p x)))))
                   find (finder (reduce-kv (fn [acc k s] (assoc acc k (parse k s))) {} @dispatch-map))]
               (fn [x] (if-some [parser (find (dispatch x))] (parser x) ::invalid))))
           (-unparser [_]
             (let [unparsers (reduce-kv (fn [acc k s] (assoc acc k (-unparser s))) {} @dispatch-map)]
               (fn [x] (if (tag? x) (if-some [f (unparsers (:key x))] (f (:value x)) ::invalid) ::invalid))))
           (-transformer [this transformer method options]
            ;; FIXME: Probably should not use `dispatch`
            ;; Can't use `dispatch` as `x` might not be valid before it has been unparsed:
             (let [this-transformer (-value-transformer transformer this method options)
                   ->children (reduce-kv (fn [acc k s] (let [t (-transformer s transformer method options)]
                                                         (cond-> acc t (assoc k t)))) {} @dispatch-map)
                   find (finder ->children)
                   child-transformer (when (seq ->children) (fn [x] (if-some [t (find (dispatch x))] (t x) x)))]
               (-intercepting this-transformer child-transformer)))
           (-walk [this walker path options] (-walk-entries this walker path options))
           (-properties [_] properties)
           (-options [_] options)
           (-children [_] (-entry-children entry-parser))
           (-parent [_] parent)
           (-form [_] @form)
           EntrySchema
           (-entries [_] (-entry-entries entry-parser))
           (-entry-parser [_] entry-parser)
           Cached
           (-cache [_] cache)
           LensSchema
           (-keep [_])
           (-get [this key default] (-get-entries this key default))
           (-set [this key value] (-set-entries this key value))
           #?@(:cljs [IPrintWithWriter (-pr-writer [this writer opts] (pr-writer-schema this writer opts))]))))
     #?@(:cljs [IPrintWithWriter (-pr-writer [this writer opts] (pr-writer-into-schema this writer opts))]))))

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
       (let [rf (or (and lazy (-memoize (fn [] (schema (mr/-schema (-registry options) ref) options))))
                    (when-let [s (mr/-schema (-registry options) ref)] (-memoize (fn [] (schema s options))))
                    (when-not allow-invalid-refs
                      (-fail! ::invalid-ref {:type :ref, :ref ref})))
             children (vec children)
             form (delay (-simple-form parent properties children identity options))
             cache (-create-cache options)
             ->parser (fn [f] (let [parser (-memoize (fn [] (f (rf))))]
                                (fn [x] ((parser) x))))]
         ^{:type ::schema}
         (reify
           AST
           (-to-ast [this _] (-to-value-ast this))
           Schema
           (-validator [_]
             (let [validator (-memoize (fn [] (-validator (rf))))]
               (fn [x] ((validator) x))))
           (-explainer [_ path]
             (let [explainer (-memoize (fn [] (-explainer (rf) (into path [0 0]))))]
               (fn [x in acc] ((explainer) x in acc))))
           (-parser [_] (->parser -parser))
           (-unparser [_] (->parser -unparser))
           (-transformer [this transformer method options]
             (let [this-transformer (-value-transformer transformer this method options)
                   deref-transformer (-memoize (fn [] (-transformer (rf) transformer method options)))]
               (-intercepting this-transformer (fn [x] (if-some [t (deref-transformer)] (t x) x)))))
           (-walk [this walker path options]
             (let [accept (fn [] (-inner walker (rf) (into path [0 0])
                                         (-update options ::walked-refs #(conj (or % #{}) ref))))]
               (when (-accept walker this path options)
                 (if (or (not ((-boolean-fn (::walk-refs options false)) ref))
                         (contains? (::walked-refs options) ref))
                   (-outer walker this path [ref] options)
                   (-outer walker this path [(accept)] options)))))
           (-properties [_] properties)
           (-options [_] options)
           (-children [_] children)
           (-parent [_] parent)
           (-form [_] @form)
           Cached
           (-cache [_] cache)
           LensSchema
           (-get [_ key default] (if (= key 0) (-pointer ref (rf) options) default))
           (-keep [_])
           (-set [this key value] (if (= key 0) (-set-children this [value])
                                                (-fail! ::index-out-of-bounds {:schema this, :key key})))
           RefSchema
           (-ref [_] ref)
           (-deref [_] (rf))
           RegexSchema
           (-regex-op? [_] false)
           (-regex-validator [this] (-fail! ::potentially-recursive-seqex this))
           (-regex-explainer [this _] (-fail! ::potentially-recursive-seqex this))
           (-regex-parser [this] (-fail! ::potentially-recursive-seqex this))
           (-regex-unparser [this] (-fail! ::potentially-recursive-seqex this))
           (-regex-transformer [this _ _ _] (-fail! ::potentially-recursive-seqex this))
           (-regex-min-max [this _] (-fail! ::potentially-recursive-seqex this))
           #?@(:cljs [IPrintWithWriter (-pr-writer [this writer opts] (pr-writer-schema this writer opts))]))))
     #?@(:cljs [IPrintWithWriter (-pr-writer [this writer opts] (pr-writer-into-schema this writer opts))]))))

(defn -schema-schema [{:keys [id raw]}]
  ^{:type ::into-schema}
  (let [internal (or id raw)
        type (if internal ::schema :schema)]
    ^{:type ::into-schema}
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
        (let [children (-vmap #(schema % options) children)
              child (nth children 0)
              form (delay (or (and (empty? properties) (or id (and raw (-form child))))
                              (-simple-form parent properties children -form options)))
              cache (-create-cache options)]
          ^{:type ::schema}
          (reify
            AST
            (-to-ast [this _]
              (cond
                id (-ast {:type type, :value id} (-properties this) (-options this))
                raw (-to-value-ast this)
                :else (-to-child-ast this)))
            Schema
            (-validator [_] (-validator child))
            (-explainer [_ path] (-explainer child (conj path 0)))
            (-parser [_] (-parser child))
            (-unparser [_] (-unparser child))
            (-transformer [this transformer method options]
              (-parent-children-transformer this children transformer method options))
            (-walk [this walker path options]
              (when (-accept walker this path options)
                (if (or (not id) ((-boolean-fn (::walk-schema-refs options false)) id))
                  (-outer walker this path (-inner-indexed walker path children options) options)
                  (-outer walker this path children options))))
            (-properties [_] properties)
            (-options [_] options)
            (-children [_] children)
            (-parent [_] parent)
            (-form [_] @form)
            Cached
            (-cache [_] cache)
            LensSchema
            (-keep [_])
            (-get [_ key default] (if (= key 0) child default))
            (-set [this key value] (if (= key 0) (-set-children this [value])
                                                 (-fail! ::index-out-of-bounds {:schema this, :key key})))
            RefSchema
            (-ref [_] id)
            (-deref [_] child)
            RegexSchema
            (-regex-op? [_]
              (if internal
                (-regex-op? child)
                false))
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
            (-regex-min-max [_ nested?]
              (if (and nested? (not internal))
                {:min 1 :max 1}
                (-regex-min-max child nested?)))
            #?@(:cljs [IPrintWithWriter (-pr-writer [this writer opts] (pr-writer-schema this writer opts))]))))
      #?@(:cljs [IPrintWithWriter (-pr-writer [this writer opts] (pr-writer-into-schema this writer opts))]))))

(defn -=>-schema []
  ^{:type ::into-schema}
  (reify
    AST
    (-from-ast [parent {:keys [input output guard properties]} options]
      (-into-schema parent properties (cond-> [(from-ast input options) (from-ast output options)]
                                        guard (conj (from-ast guard))) options))
    IntoSchema
    (-type [_] :=>)
    (-type-properties [_])
    (-into-schema [parent properties children {::keys [function-checker] :as options}]
      (-check-children! :=> properties children 2 3)
      (let [[input output guard :as children] (-vmap #(schema % options) children)
            form (delay (-create-form (-type parent) properties (-vmap -form children) options))
            cache (-create-cache options)
            ->checker (if function-checker #(function-checker % options) (constantly nil))]
        (when-not (#{:cat :catn} (type input))
          (-fail! ::invalid-input-schema {:input input}))
        ^{:type ::schema}
        (reify
          AST
          (-to-ast [_ _]
            (cond-> {:type :=>, :input (ast input), :output (ast output)}
              guard (assoc :guard (ast guard)), properties (assoc :properties properties)))
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
                    (let [{::keys [explain-input explain-output explain-guard]} res
                          res (dissoc res ::explain-input ::explain-output ::explain-guard)
                          {:keys [path in] :as error} (assoc (miu/-error path in this x) :check res)
                          -push (fn [acc i e]
                                  (cond-> acc e (into (map #(assoc % :path (conj path i), :in in) (:errors e)))))]
                      (-> (conj acc error) (-push 0 explain-input) (-push 1 explain-output) (-push 2 explain-guard)))
                    acc)))
              (let [validator (-validator this)]
                (fn explain [x in acc]
                  (if-not (validator x) (conj acc (miu/-error path in this x)) acc)))))
          (-parser [this]
            (let [validator (-validator this)]
              (fn [x] (if (validator x) x ::invalid))))
          (-unparser [this] (-parser this))
          (-transformer [_ _ _ _])
          (-walk [this walker path options] (-walk-indexed this walker path options))
          (-properties [_] properties)
          (-options [_] options)
          (-children [_] children)
          (-parent [_] parent)
          (-form [_] @form)
          FunctionSchema
          (-function-schema? [_] true)
          (-function-schema-arities [this] [this])
          (-function-info [_]
            (let [{:keys [min max]} (-regex-min-max input false)]
              (cond-> {:min min
                       :arity (if (= min max) min :varargs)
                       :input input
                       :output output}
                guard (assoc :guard guard)
                max (assoc :max max))))
          (-instrument-f [schema {:keys [scope report gen] :as props} f _options]
            (let [{:keys [min max input output guard]} (-function-info schema)
                  [validate-input validate-output] (-vmap -validator [input output])
                  validate-guard (or (some-> guard -validator) any?)
                  [wrap-input wrap-output wrap-guard] (-vmap #(contains? scope %) [:input :output :guard])
                  f (or (if gen (gen schema) f) (-fail! ::missing-function {:props props}))]
              (fn [& args]
                (let [args (vec args), arity (count args)]
                  (when wrap-input
                    (when-not (<= min arity (or max miu/+max-size+))
                      (report ::invalid-arity {:arity arity, :arities #{{:min min :max max}}, :args args, :input input, :schema schema}))
                    (when-not (validate-input args)
                      (report ::invalid-input {:input input, :args args, :schema schema})))
                  (let [value (apply f args)]
                    (when (and wrap-output (not (validate-output value)))
                      (report ::invalid-output {:output output, :value value, :args args, :schema schema}))
                    (when (and wrap-guard (not (validate-guard [args value])))
                      (report ::invalid-guard {:guard guard, :value value, :args args, :schema schema}))
                    value)))))
          Cached
          (-cache [_] cache)
          LensSchema
          (-keep [_])
          (-get [_ key default] (get children key default))
          (-set [this key value] (-set-assoc-children this key value))
          #?@(:cljs [IPrintWithWriter (-pr-writer [this writer opts] (pr-writer-schema this writer opts))]))))
    #?@(:cljs [IPrintWithWriter (-pr-writer [this writer opts] (pr-writer-into-schema this writer opts))])))

(defn -function-schema [_]
  ^{:type ::into-schema}
  (reify IntoSchema
    (-type [_] :function)
    (-type-properties [_])
    (-properties-schema [_ _])
    (-children-schema [_ _])
    (-into-schema [parent properties children {::keys [function-checker] :as options}]
      (-check-children! :function properties children 1 nil)
      (let [children (-vmap #(schema % options) children)
            form (delay (-simple-form parent properties children -form options))
            cache (-create-cache options)
            ->checker (if function-checker #(function-checker % options) (constantly nil))]
        (when-not (every? (every-pred -function-schema? -function-info) children)
          (-fail! ::non-function-childs {:children children}))
        (-group-by-arity! (-vmap -function-info children))
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
          (-walk [this walker path options] (-walk-indexed this walker path options))
          (-properties [_] properties)
          (-options [_] options)
          (-children [_] children)
          (-parent [_] parent)
          (-form [_] @form)
          FunctionSchema
          (-function-schema? [_] true)
          (-function-schema-arities [_] children)
          (-function-info [_])
          (-instrument-f [this {:keys [_scope report] :as props} f options]
            (let [arity->info (->> children
                                   (map (fn [s] (assoc (-function-info s) :f (-instrument (assoc props :schema s) f options))))
                                   (-group-by-arity!))
                  arities (-> arity->info keys set)
                  varargs-info (arity->info :varargs)]
              (if (= 1 (count arities))
                (-> arity->info first val :f)
                (fn [& args]
                  (let [arity (count args)
                        {:keys [input] :as info} (arity->info arity)
                        report-arity #(report ::invalid-arity {:arity arity, :arities arities, :args args, :input input, :schema this})]
                    (cond
                      info (apply (:f info) args)
                      varargs-info (if (< arity (:min varargs-info)) (report-arity) (apply (:f varargs-info) args))
                      :else (report-arity)))))))
          Cached
          (-cache [_] cache)
          LensSchema
          (-keep [_])
          (-get [_ key default] (get children key default))
          (-set [this key value] (-set-assoc-children this key value))
          #?@(:cljs [IPrintWithWriter (-pr-writer [this writer opts] (pr-writer-schema this writer opts))]))))
    #?@(:cljs [IPrintWithWriter (-pr-writer [this writer opts] (pr-writer-into-schema this writer opts))])))

(defn -proxy-schema [{:keys [type min max childs type-properties fn]}]
  ^{:type ::into-schema}
  (reify IntoSchema
    (-type [_] type)
    (-type-properties [_] type-properties)
    (-properties-schema [_ _])
    (-children-schema [_ _])
    (-into-schema [parent properties children options]
      (-check-children! type properties children min max)
      (let [[children forms schema] (fn properties (vec children) options)
            schema (delay (force schema))
            form (delay (-create-form type properties forms options))
            cache (-create-cache options)]
        ^{:type ::schema}
        (reify
          Schema
          (-validator [_] (-validator @schema))
          (-explainer [_ path] (-explainer @schema (conj path ::in)))
          (-parser [_] (-parser @schema))
          (-unparser [_] (-unparser @schema))
          (-transformer [this transformer method options]
            (-parent-children-transformer this [@schema] transformer method options))
          (-walk [this walker path options]
            (let [children (if childs (subvec children 0 childs) children)]
              (when (-accept walker this path options)
                (-outer walker this path (-inner-indexed walker path children options) options))))
          (-properties [_] properties)
          (-options [_] options)
          (-children [_] children)
          (-parent [_] parent)
          (-form [_] @form)
          Cached
          (-cache [_] cache)
          LensSchema
          (-keep [_])
          (-get [_ key default] (if (= ::in key) @schema (get children key default)))
          (-set [_ key value] (into-schema type properties (assoc children key value)))
          DistributiveSchema
          (-distributive-schema? [_] (-distributive-schema? schema))
          (-distribute-to-children [_ f options] (-distribute-to-children schema f options))
          FunctionSchema
          (-function-schema? [_] (-function-schema? @schema))
          (-function-info [_] (-function-info @schema))
          (-function-schema-arities [_] (-function-schema-arities @schema))
          (-instrument-f [_ props f options] (-instrument-f @schema props f options))
          RegexSchema
          (-regex-op? [_] (-regex-op? @schema))
          (-regex-validator [_] (-regex-validator @schema))
          (-regex-explainer [_ path] (-regex-explainer @schema path))
          (-regex-unparser [_] (-regex-unparser @schema))
          (-regex-parser [_] (-regex-parser @schema))
          (-regex-transformer [_ transformer method options] (-regex-transformer @schema transformer method options))
          (-regex-min-max [_ nested?] (-regex-min-max @schema nested?))
          RefSchema
          (-ref [_])
          (-deref [_] @schema)
          #?@(:cljs [IPrintWithWriter (-pr-writer [this writer opts] (pr-writer-schema this writer opts))]))))
    #?@(:cljs [IPrintWithWriter (-pr-writer [this writer opts] (pr-writer-into-schema this writer opts))])))

(defn -->-schema
  "Experimental simple schema for :=> schema. AST and explain results subject to change."
  [_]
  (-proxy-schema {:type :->
                  :fn (fn [{:keys [guard] :as p} c o]
                        (-check-children! :-> p c 1 nil)
                        (let [c (mapv #(schema % o) c)]
                          [c (map -form c) (delay (let [cc (cond-> [(into [:cat] (pop c)) (peek c)]
                                                             guard (conj [:fn guard]))]
                                                    (into-schema :=> (dissoc p :guard) cc o)))]))}))

(defn- regex-validator [schema] (re/validator (-regex-validator schema)))

(defn- regex-explainer [schema path] (re/explainer schema path (-regex-explainer schema path)))

(defn- regex-parser [schema] (re/parser (-regex-parser schema)))

(defn- regex-transformer [schema transformer method options]
  (let [this-transformer (-value-transformer transformer schema method options)
        ->children (re/transformer (-regex-transformer schema transformer method options))]
    (-intercepting this-transformer ->children)))

(defn -sequence-schema
  [{:keys [type re-validator re-explainer re-parser re-unparser re-transformer re-min-max] {:keys [min max]} :child-bounds}]
  ^{:type ::into-schema}
  (reify IntoSchema
    (-type [_] type)
    (-type-properties [_])
    (-properties-schema [_ _])
    (-children-schema [_ _])
    (-into-schema [parent properties children options]
      (-check-children! type properties children min max)
      (let [children (-vmap #(schema % options) children)
            form (delay (-simple-form parent properties children -form options))
            cache (-create-cache options)]
        ^{:type ::schema}
        (reify
          Schema
          (-validator [this] (regex-validator this))
          (-explainer [this path] (regex-explainer this path))
          (-parser [this] (regex-parser this))
          (-unparser [this] (-regex-unparser this))
          (-transformer [this transformer method options] (regex-transformer this transformer method options))
          (-walk [this walker path options] (-walk-indexed this walker path options))
          (-properties [_] properties)
          (-options [_] options)
          (-children [_] children)
          (-parent [_] parent)
          (-form [_] @form)
          Cached
          (-cache [_] cache)
          LensSchema
          (-keep [_] true)
          (-get [_ key default] (get children key default))
          (-set [this key value] (-set-assoc-children this key value))
          RegexSchema
          (-regex-op? [_] true)
          (-regex-validator [_] (re-validator properties (-vmap -regex-validator children)))
          (-regex-explainer [_ path]
            (re-explainer properties (map-indexed (fn [i child] (-regex-explainer child (conj path i))) children)))
          (-regex-parser [_] (re-parser properties (-vmap -regex-parser children)))
          (-regex-unparser [_] (re-unparser properties (-vmap -regex-unparser children)))
          (-regex-transformer [_ transformer method options]
            (re-transformer properties (-vmap #(-regex-transformer % transformer method options) children)))
          (-regex-min-max [_ _] (re-min-max properties children))
          #?@(:cljs [IPrintWithWriter (-pr-writer [this writer opts] (pr-writer-schema this writer opts))]))))
    #?@(:cljs [IPrintWithWriter (-pr-writer [this writer opts] (pr-writer-into-schema this writer opts))])))

(defn -sequence-entry-schema
  [{:keys [type re-validator re-explainer re-parser re-unparser re-transformer re-min-max] {:keys [min max keep]} :child-bounds :as opts}]
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
      (let [entry-parser (-create-entry-parser children opts options)
            form (delay (-create-entry-form parent properties entry-parser options))
            cache (-create-cache options)]
        ^{:type ::schema}
        (reify
          AST
          (-to-ast [this _] (-entry-ast this (-entry-keyset entry-parser)))
          Schema
          (-validator [this] (regex-validator this))
          (-explainer [this path] (regex-explainer this path))
          (-parser [this] (regex-parser this))
          (-unparser [this] (-regex-unparser this))
          (-transformer [this transformer method options] (regex-transformer this transformer method options))
          (-walk [this walker path options] (-walk-entries this walker path options))
          (-properties [_] properties)
          (-options [_] options)
          (-children [_] (-entry-children entry-parser))
          (-parent [_] parent)
          (-form [_] @form)
          Cached
          (-cache [_] cache)
          LensSchema
          (-keep [_] keep)
          (-get [this key default] (-get-entries this key default))
          (-set [this key value] (-set-entries this key value))
          EntrySchema
          (-entries [_] (-entry-entries entry-parser))
          (-entry-parser [_] entry-parser)
          RegexSchema
          (-regex-op? [_] true)
          (-regex-validator [this] (re-validator properties (-vmap (fn [[k _ s]] [k (-regex-validator s)]) (-children this))))
          (-regex-explainer [this path]
            (re-explainer properties (-vmap (fn [[k _ s]] [k (-regex-explainer s (conj path k))]) (-children this))))
          (-regex-parser [this] (re-parser properties (-vmap (fn [[k _ s]] [k (-regex-parser s)]) (-children this))))
          (-regex-unparser [this] (re-unparser properties (-vmap (fn [[k _ s]] [k (-regex-unparser s)]) (-children this))))
          (-regex-transformer [this transformer method options]
            (re-transformer properties (-vmap (fn [[k _ s]] [k (-regex-transformer s transformer method options)]) (-children this))))
          (-regex-min-max [this _] (re-min-max properties (-children this)))
          #?@(:cljs [IPrintWithWriter (-pr-writer [this writer opts] (pr-writer-schema this writer opts))]))))
    #?@(:cljs [IPrintWithWriter (-pr-writer [this writer opts] (pr-writer-into-schema this writer opts))])))

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
   (let [properties' (when properties (when (pos? (count properties)) properties))
         r (when properties' (properties' :registry))
         options (if r (-update options :registry #(mr/composite-registry r (or % (-registry options)))) options)
         properties (if r (assoc properties' :registry (-property-registry r options identity)) properties')]
     (-into-schema (-lookup! type [type properties children] into-schema? false options) properties children options))))

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
     (vector? ?schema) (let [v #?(:clj ^IPersistentVector ?schema, :cljs ?schema)
                             t (-lookup! #?(:clj (.nth v 0), :cljs (nth v 0)) v into-schema? true options)
                             n #?(:bb (count v) :clj (.count v), :cljs (count v))
                             ?p (when (> n 1) #?(:clj (.nth v 1), :cljs (nth v 1)))]
                         (if (or (nil? ?p) (map? ?p))
                           (into-schema t ?p (when (< 2 n) (subvec ?schema 2 n)) options)
                           (into-schema t nil (when (< 1 n) (subvec ?schema 1 n)) options)))
     :else (if-let [?schema' (and (-reference? ?schema) (-lookup ?schema options))]
             (-pointer ?schema (schema ?schema' options) options)
             (-> ?schema (-lookup! ?schema nil false options) (recur options))))))

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
  "Returns an pure validation function of type `x -> boolean` for a given Schema.
   Caches the result for [[Cached]] Schemas with key `:validator`."
  ([?schema]
   (validator ?schema nil))
  ([?schema options]
   (-cached (schema ?schema options) :validator -validator)))

(defn validate
  "Returns true if value is valid according to given schema. Creates the `validator`
   for every call. When performance matters, (re-)use `validator` instead."
  ([?schema value]
   (validate ?schema value nil))
  ([?schema value options]
   ((validator ?schema options) value)))

(defn explainer
  "Returns an pure explainer function of type `x -> explanation` for a given Schema.
   Caches the result for [[Cached]] Schemas with key `:explainer`."
  ([?schema]
   (explainer ?schema nil))
  ([?schema options]
   (let [schema' (schema ?schema options)
         explainer' (-cached schema' :explainer #(-explainer % []))]
     (fn explainer
       ([value]
        (explainer value [] []))
       ([value in acc]
        (when-let [errors (seq (explainer' value in acc))]
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
  "Returns an pure parser function of type `x -> either parsed-x ::invalid` for a given Schema.
   Caches the result for [[Cached]] Schemas with key `:parser`."
  ([?schema]
   (parser ?schema nil))
  ([?schema options]
   (-cached (schema ?schema options) :parser -parser)))

(defn parse
  "parses a value against a given schema. Creates the `parser` for every call.
   When performance matters, (re-)use `parser` instead."
  ([?schema value]
   (parse ?schema value nil))
  ([?schema value options]
   ((parser ?schema options) value)))

(defn unparser
  "Returns an pure unparser function of type `parsed-x -> either x ::invalid` for a given Schema.
   Caches the result for [[Cached]] Schemas with key `:unparser`."
  ([?schema]
   (unparser ?schema nil))
  ([?schema options]
   (-cached (schema ?schema options) :unparser -unparser)))

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

(defn coercer
  "Creates a function to decode and validate a value, throws on validation error."
  ([?schema] (coercer ?schema nil nil))
  ([?schema transformer] (coercer ?schema transformer nil))
  ([?schema transformer options] (coercer ?schema transformer nil nil options))
  ([?schema transformer respond raise] (coercer ?schema transformer respond raise nil))
  ([?schema transformer respond raise options]
   (let [s (schema ?schema options)
         valid? (validator s)
         decode (decoder s transformer)
         explain (explainer s)
         respond (or respond identity)
         raise (or raise #(-fail! ::coercion %))]
     (fn -coercer [x] (let [value (decode x)]
                        (if (valid? value)
                          (respond value)
                          (raise {:value value, :schema s, :explain (explain value)})))))))

(defn coerce
  "Decode and validate a value, throws on validation error."
  ([?schema value] (coerce ?schema value nil nil))
  ([?schema value transformer] (coerce ?schema value transformer nil))
  ([?schema value transformer options] (coerce ?schema value transformer nil nil options))
  ([?schema value transformer respond raise] (coerce ?schema value transformer respond raise nil))
  ([?schema value transformer respond raise options] ((coercer ?schema transformer respond raise options) value)))

(defmacro assert
  "Assert that `value` validates against schema `?schema`, or throws ExceptionInfo.
   The var clojure.core/*assert* determines whether assertion are checked."

  ([?schema value]
   `(assert ~?schema ~value nil))

  ([?schema value options]
   (if *assert*
     `(coerce ~?schema ~value nil ~options)
     value)))

(defn entries
  "Returns `EntrySchema` children as a sequence of `clojure.lang/MapEntry`s
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
   (when-let [schema (schema ?schema options)]
     (when (-entry-schema? schema) (-entries schema)))))

(defn explicit-keys
  "Returns a vector of explicit (not ::m/default) keys from EntrySchema"
  ([?schema] (explicit-keys ?schema nil))
  ([?schema options]
   (let [schema (schema ?schema options)]
     (when (-entry-schema? schema)
       (reduce
        (fn [acc [k :as e]] (cond-> acc (not (-default-entry e)) (conj k)))
        [] (-entries schema))))))

(defn default-schema
  "Returns the default (::m/default) schema from EntrySchema"
  ([?schema] (default-schema ?schema nil))
  ([?schema options]
   (let [schema (schema ?schema options)]
     (when (-entry-schema? schema)
       (-default-entry-schema (-children schema))))))

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

(defn deref-recursive
  "Derefs all schemas at all levels. Does not walk over `:ref`s."
  ([?schema]
   (deref-recursive ?schema nil))
  ([?schema {::keys [ref-key] :as options}]
   (let [schema (schema ?schema options)
         maybe-set-ref (fn [s r] (if (and ref-key r) (-update-properties s assoc ref-key r) s))]
     (-> (walk schema (fn [schema _ children _]
                        (cond (= :ref (type schema)) schema
                              (-ref-schema? schema) (maybe-set-ref (deref (-set-children schema children)) (-ref schema))
                              :else (-set-children schema children)))
               {::walk-schema-refs true})
         (deref-all)))))

(defn from-ast
  "Creates a Schema from AST"
  ([?ast] (from-ast ?ast nil))
  ([?ast options]
   (cond
     (schema? ?ast) ?ast
     (map? ?ast) (if-let [s (-lookup (:type ?ast) options)]
                   (let [r (when-let [r (:registry ?ast)] (-delayed-registry r from-ast))
                         options (cond-> options r (-update :registry #(mr/composite-registry r (or % (-registry options)))))
                         ast (cond-> ?ast r (-update :properties #(assoc % :registry (-property-registry r options identity))))]
                     (cond (and (into-schema? s) (-ast? s)) (-from-ast s ast options)
                           (into-schema? s) (-into-schema s (:properties ast) (-vmap #(from-ast % options) (:children ast)) options)
                           :else s))
                   (-fail! ::invalid-ast {:ast ?ast}))
     :else (-fail! ::invalid-ast {:ast ?ast}))))

(defn ast
  "Returns the Schema AST"
  ([?schema] (ast ?schema nil))
  ([?schema options]
   (let [s (schema ?schema options)]
     (if (-ast? s)
       (-to-ast s options)
       (let [c (-children s)]
         (-ast (cond-> {:type (type s)}
                 c (assoc :children (-vmap #(ast % options) c)))
               (-properties s)
               (-options s)))))))
;;
;; eval
;;

(defn -default-sci-options []
  {:preset :termination-safe
   :aliases {'str 'clojure.string
             'm 'malli.core}
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
  (let [-safe-empty? (fn [x] (and (seqable? x) (empty? x)))]
    (->> [#'any? #'some? #'number? #'integer? #'int? #'pos-int? #'neg-int? #'nat-int? #'pos? #'neg? #'float? #'double?
          #'boolean? #'string? #'ident? #'simple-ident? #'qualified-ident? #'keyword? #'simple-keyword?
          #'qualified-keyword? #'symbol? #'simple-symbol? #'qualified-symbol? #'uuid? #'uri? #'inst? #'seqable?
          #'indexed? #'map? #'vector? #'list? #'seq? #'char? #'set? #'nil? #'false? #'true?
          #'zero? #'coll? [#'empty? -safe-empty?] #'associative? #'sequential? #'ifn? #'fn?
          #?@(:clj [#'rational? #'ratio? #'bytes? #'decimal?])]
         (reduce -register-var {}))))

(defn class-schemas []
  {#?(:clj  Pattern,
      ;; closure will complain if you reference the global RegExp object.
      :cljs (c/type #"")) (-re-schema true)})

(defn comparator-schemas []
  (->> {:> >, :>= >=, :< <, :<= <=, := =, :not= not=}
       (-vmap (fn [[k v]] [k (-simple-schema {:type k :from-ast -from-value-ast :to-ast -to-value-ast :min 1 :max 1
                                              :compile (fn [_ [child] _] {:pred (-safe-pred #(v % child))})})]))
       (into {}) (reduce-kv assoc nil)))

(defn type-schemas []
  {:any (-any-schema)
   :some (-some-schema)
   :nil (-nil-schema)
   :string (-string-schema)
   :int (-int-schema)
   :float (-float-schema)
   :double (-double-schema)
   :boolean (-boolean-schema)
   :keyword (-keyword-schema)
   :symbol (-symbol-schema)
   :qualified-keyword (-qualified-keyword-schema)
   :qualified-symbol (-qualified-symbol-schema)
   :uuid (-uuid-schema)})

(defn sequence-schemas []
  {:+ (-sequence-schema {:type :+, :child-bounds {:min 1, :max 1}, :keep true
                         :re-validator (fn [_ [child]] (re/+-validator child))
                         :re-explainer (fn [_ [child]] (re/+-explainer child))
                         :re-parser (fn [_ [child]] (re/+-parser child))
                         :re-unparser (fn [_ [child]] (re/+-unparser child))
                         :re-transformer (fn [_ [child]] (re/+-transformer child))
                         :re-min-max (fn [_ [child]] {:min (:min (-regex-min-max child true))})})
   :* (-sequence-schema {:type :*, :child-bounds {:min 1, :max 1}, :keep true
                         :re-validator (fn [_ [child]] (re/*-validator child))
                         :re-explainer (fn [_ [child]] (re/*-explainer child))
                         :re-parser (fn [_ [child]] (re/*-parser child))
                         :re-unparser (fn [_ [child]] (re/*-unparser child))
                         :re-transformer (fn [_ [child]] (re/*-transformer child))
                         :re-min-max (fn [_ _] {:min 0})})
   :? (-sequence-schema {:type :?, :child-bounds {:min 1, :max 1}, :keep true
                         :re-validator (fn [_ [child]] (re/?-validator child))
                         :re-explainer (fn [_ [child]] (re/?-explainer child))
                         :re-parser (fn [_ [child]] (re/?-parser child))
                         :re-unparser (fn [_ [child]] (re/?-unparser child))
                         :re-transformer (fn [_ [child]] (re/?-transformer child))
                         :re-min-max (fn [_ [child]] {:min 0, :max (:max (-regex-min-max child true))})})
   :repeat (-sequence-schema {:type :repeat, :child-bounds {:min 1, :max 1}, :keep true
                              :re-validator (fn [{:keys [min max] :or {min 0, max ##Inf}} [child]] (re/repeat-validator min max child))
                              :re-explainer (fn [{:keys [min max] :or {min 0, max ##Inf}} [child]] (re/repeat-explainer min max child))
                              :re-parser (fn [{:keys [min max] :or {min 0, max ##Inf}} [child]] (re/repeat-parser min max child))
                              :re-unparser (fn [{:keys [min max] :or {min 0, max ##Inf}} [child]] (re/repeat-unparser min max child))
                              :re-transformer (fn [{:keys [min max] :or {min 0, max ##Inf}} [child]] (re/repeat-transformer min max child))
                              :re-min-max (fn [props [child]] (-re-min-max * props child))})
   :cat (-sequence-schema {:type :cat, :child-bounds {}, :keep true
                           :re-validator (fn [_ children] (apply re/cat-validator children))
                           :re-explainer (fn [_ children] (apply re/cat-explainer children))
                           :re-parser (fn [_ children] (apply re/cat-parser children))
                           :re-unparser (fn [_ children] (apply re/cat-unparser children))
                           :re-transformer (fn [_ children] (apply re/cat-transformer children))
                           :re-min-max (fn [_ children] (reduce (partial -re-min-max +) {:min 0, :max 0} children))})
   :alt (-sequence-schema {:type :alt, :child-bounds {:min 1}, :keep true
                           :re-validator (fn [_ children] (apply re/alt-validator children))
                           :re-explainer (fn [_ children] (apply re/alt-explainer children))
                           :re-parser (fn [_ children] (apply re/alt-parser children))
                           :re-unparser (fn [_ children] (apply re/alt-unparser children))
                           :re-transformer (fn [_ children] (apply re/alt-transformer children))
                           :re-min-max (fn [_ children] (reduce -re-alt-min-max {:max 0} children))})
   :catn (-sequence-entry-schema {:type :catn, :child-bounds {}, :keep false
                                  :re-validator (fn [_ children] (apply re/cat-validator children))
                                  :re-explainer (fn [_ children] (apply re/cat-explainer children))
                                  :re-parser (fn [_ children] (apply re/catn-parser tags children))
                                  :re-unparser (fn [_ children] (apply re/catn-unparser tags? children))
                                  :re-transformer (fn [_ children] (apply re/cat-transformer children))
                                  :re-min-max (fn [_ children] (reduce (partial -re-min-max +) {:min 0, :max 0} (-vmap last children)))})
   :altn (-sequence-entry-schema {:type :altn, :child-bounds {:min 1}, :keep false
                                  :re-validator (fn [_ children] (apply re/alt-validator children))
                                  :re-explainer (fn [_ children] (apply re/alt-explainer children))
                                  :re-parser (fn [_ children] (apply re/altn-parser tag children))
                                  :re-unparser (fn [_ children] (apply re/altn-unparser tag? children))
                                  :re-transformer (fn [_ children] (apply re/alt-transformer children))
                                  :re-min-max (fn [_ children] (reduce -re-alt-min-max {:max 0} (-vmap last children)))})})

(defn base-schemas []
  {:and (-and-schema)
   :or (-or-schema)
   :orn (-orn-schema)
   :not (-not-schema)
   :map (-map-schema)
   :map-of (-map-of-schema)
   :vector (-collection-schema {:type :vector, :pred vector?, :empty []})
   :sequential (-collection-schema {:type :sequential, :pred sequential?})
   :seqable (-collection-schema {:type :seqable, :pred seqable?})
   :every (-collection-schema {:type :every, :pred seqable?, :bounded true})
   :set (-collection-schema {:type :set, :pred set?, :empty #{}, :in (fn [_ x] x)})
   :enum (-enum-schema)
   :maybe (-maybe-schema)
   :tuple (-tuple-schema)
   :multi (-multi-schema)
   :re (-re-schema false)
   :fn (-fn-schema)
   :ref (-ref-schema)
   :=> (-=>-schema)
   :-> (-->-schema nil)
   :function (-function-schema nil)
   :schema (-schema-schema nil)
   ::schema (-schema-schema {:raw true})})

(defn default-schemas []
  (merge (predicate-schemas) (class-schemas) (comparator-schemas) (type-schemas) (sequence-schemas) (base-schemas)))

(def default-registry
  (let [strict #?(:cljs (identical? mr/mode "strict")
                  :default (= mr/mode "strict"))
        custom #?(:cljs (identical? mr/type "custom")
                  :default (= mr/type "custom"))
        registry (if custom (mr/fast-registry {}) (mr/composite-registry (mr/fast-registry (default-schemas)) (mr/var-registry)))]
    (when-not strict (mr/set-default-registry! registry))
    (mr/registry (if strict registry (mr/custom-default-registry)))))

;;
;; function schemas
;;

(defonce ^:private -function-schemas* (atom {}))
(defn function-schemas ([] (function-schemas :clj)) ([key] (@-function-schemas* key)))

(defn -deregister-function-schemas! [key] (swap! -function-schemas* assoc key {}))

(defn -deregister-metadata-function-schemas!
  [key]
  (swap! -function-schemas* update key
         (fn [fn-schemas-map]
           (reduce-kv (fn [acc ns-sym fn-map]
                        (assoc acc ns-sym
                               (reduce-kv
                                (fn [acc2 fn-sym fn-map]
                                  ;; rm metadata schemas
                                  (if (:metadata-schema? fn-map)
                                    acc2
                                    (assoc acc2 fn-sym fn-map)))
                                {}
                                fn-map)))
                      {}
                      fn-schemas-map))))

(defn function-schema
  ([?schema] (function-schema ?schema nil))
  ([?schema options]
   (let [s (schema ?schema options)]
     (if (-function-schema? s) s (-fail! ::invalid-=>schema {:type (type s), :schema s})))))

;; for cljs we cannot invoke `function-schema` at macroexpansion-time
;; - `?schema` could contain cljs vars that will only resolve at runtime.
(defn -register-function-schema!
  ([ns name ?schema data] (-register-function-schema! ns name ?schema data :clj function-schema))
  ([ns name ?schema data key f]
   (try
     (swap! -function-schemas* assoc-in [key ns name] (merge data {:schema (f ?schema), :ns ns, :name name}))
     (catch #?(:clj Throwable :cljs :default) ex
       (-fail! ::register-function-schema {:ns ns, :name name, :schema ?schema, :data data, :key key, :exception ex})))))

#?(:clj
   (defmacro => [given-sym value]
     (let [cljs-resolve (when (:ns &env) (ns-resolve 'cljs.analyzer.api 'resolve))
           cljs-resolve-symbols (fn [env d]
                                  (walk/postwalk (fn [x] (cond->> x (symbol? x) (or (:name (cljs-resolve env x)))))
                                                 d))
           name-str (name given-sym)
           ns-str (str (or (not-empty (namespace given-sym)) *ns*))
           name' `'~(symbol name-str)
           ns' `'~(symbol ns-str)
           sym `'~(symbol ns-str name-str)
           value' (cond->> value (:ns &env) (cljs-resolve-symbols &env))]
       ;; in cljs we need to register the schema in clojure (the cljs compiler)
       ;; so it is visible in the (function-schemas :cljs) map at macroexpansion time.
       (if (:ns &env)
         (do
           (-register-function-schema! (symbol ns-str) (symbol name-str) value' (meta given-sym) :cljs identity)
           `(do (-register-function-schema! ~ns' ~name' ~value' ~(meta given-sym) :cljs identity) ~sym))
         `(do (-register-function-schema! ~ns' ~name' ~value' ~(meta given-sym)) ~sym)))))

(defn -instrument
  "Takes an instrumentation properties map and a function and returns a wrapped function,
   which will validate function arguments and return values based on the function schema
   definition. The following properties are used:

   | key       | description |
   | ----------|-------------|
   | `:schema` | function schema
   | `:scope`  | optional set of scope definitions, defaults to `#{:input :output :guard}`
   | `:report` | optional side-effecting function of `key data -> any` to report problems, defaults to `m/-fail!`
   | `:gen`    | optional function of `schema -> schema -> value` to be invoked on the args to get the return value"
  ([props]
   (-instrument props nil nil))
  ([props f]
   (-instrument props f nil))
  ([props f options]
   (let [props (-> props
                   (update :scope #(or % #{:input :output :guard}))
                   (update :report #(or % -fail!)))
         s (-> props :schema (schema options))]
     (or (-instrument-f s props f options)
         (-fail! ::instrument-requires-function-schema {:schema s})))))
