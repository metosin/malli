;; See also `malli.generator-ast` for viewing generators as data
(ns malli.generator
  (:require [clojure.set :as set]
            [clojure.spec.gen.alpha :as ga]
            [clojure.string :as str]
            [clojure.test.check :as check]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.random :as random]
            [clojure.test.check.rose-tree :as rose]
            [malli.core :as m]
            [malli.registry :as mr]
            [malli.util :as mu]
            [malli.impl.util :refer [-last -merge]]
            #?(:clj [borkdude.dynaload :as dynaload])))

(declare generator generate -create gen-one-of gen-double)

(defprotocol Generator
  (-generator [this options] "returns generator for schema"))

;;
;; generators
;;


;; # Notes for implementors
;;
;; For the most part, -schema-generator is a pretty direct translation from schemas to generators.
;; However, the naive implementation of recursive ref's (creating a generator for the dereferenced ref
;; and cutting off the generation at a certain depth) tends to create exponentially large test cases.
;;
;; We use a more sophisticated algorithm to achieve linearly sized test cases with recursive refs.
;; The next section describes the strategy implementors should use to participate in this improved behavior.
;; The theory behind this strategy is described in the section below ("Approach for recursive generators").
;;
;; ## Implementation strategy
;;
;; Say you have a composite schema you want to generate values for. You should extend `-schema-generator` and
;; call `generator` recursively on the `m/children`. Now, for every child generator, you need to consider the case
;; that the child generator generates no values, and how this might change the final generator.
;;
;; Use `-unreachable-gen?` to test whether your child generator generates no values (we'll call this an "unreachable" schema/generator).
;; If your parent generator cannot generate values, use `-never-gen` to return an unreachable generator.
;;
;; Here are a few examples---compare them with the logic in their respective -schema-generator methods:
;;   [:maybe M] would generate like :nil if M were unreachable.
;;   [:map [:a M]] would itself be unreachable if M were unreachable.
;;   [:map [:a {:optional true} M]] would generate like [:map] if M were unreachable.
;;   [:vector M] would generate like [:= []] if M were unreachable.
;;   [:vector {:min 1} M] would itself be unreachable if M were unreachable.

(def nil-gen (gen/return nil))

(defn- -child [schema options] (first (m/children schema options)))
(defn- -child-gen [schema options] (generator (-child schema options) options))

(defn -never-gen
  "Return a generator of no values that is compatible with -unreachable-gen?."
  [{::keys [original-generator-schema] :as _options}]
  (with-meta (gen/sized (fn [_]
                          (m/-fail! ::unsatisfiable-schema
                                    (cond-> {}
                                      original-generator-schema (assoc :schema original-generator-schema)))))
             {::never-gen true
              ::original-generator-schema original-generator-schema}))

(defn -unreachable-gen?
  "Returns true iff generator g generators no values."
  [g] (-> (meta g) ::never-gen boolean))

(defn -not-unreachable [g] (when-not (-unreachable-gen? g) g))
(defn -unreachable [g] (when (-unreachable-gen? g) g))

(defn- -random [seed] (if seed (random/make-random seed) (random/make-random)))

(defn -min-max [schema options]
  (let [{:keys [min max] gen-min :gen/min gen-max :gen/max} (m/properties schema options)]
    (when (and min gen-min (< gen-min min))
      (m/-fail! ::invalid-property {:key :gen/min, :value gen-min, :min min}))
    (when (and max gen-max (> gen-max max))
      (m/-fail! ::invalid-property {:key :gen/max, :value gen-min, :max min}))
    {:min (or gen-min min)
     :max (or gen-max max)}))

(defn- inf-nan [schema options]
  (let [{:gen/keys [infinite? NaN?]} (m/properties schema)]
    {:infinite? infinite? :NaN? NaN?}))

(defn- gen-fmap [f gen] (or (-unreachable gen) (gen/fmap f gen)))
(defn- gen-fcat [gen] (gen-fmap #(apply concat %) gen))
(defn- gen-tuple [gens] (or (some -unreachable gens) (apply gen/tuple gens)))
(defn- gen-maybe [g] (if (-unreachable-gen? g) nil-gen (gen/one-of [nil-gen g])))
(def ^:private double-default {:infinite? false, :NaN? false})
(defn- gen-double [opts] (gen/double* (-> (into double-default opts) (update :min #(some-> % double)) (update :max #(some-> % double)))))

(defn- gen-vector [{:keys [min max]} g]
  (cond
    (-unreachable-gen? g) (if (zero? (or min 0)) (gen/return []) g)
    (and min (= min max)) (gen/vector g min)
    (and min max) (gen/vector g min max)
    min (vary-meta (gen/sized #(gen/vector g min (+ min %))) assoc ::generator-ast {:op :vector-min :generator g :min min})
    max (gen/vector g 0 max)
    :else (gen/vector g)))

(defn- gen-vector-distinct-by [schema {:keys [min] :as m} f g]
  (if (-unreachable-gen? g)
    (if (= 0 (or min 0)) (gen/return []) g)
    (gen/vector-distinct-by f g (-> (assoc (if (and min (= min max))
                                             {:num-elements min}
                                             (set/rename-keys m {:min :min-elements :max :max-elements}))
                                           :ex-fn #(m/-exception ::distinct-generator-failure (assoc % :schema schema)))))))

(defn- -string-gen [schema options]
  (gen-fmap str/join (gen-vector (-min-max schema options) gen/char-alphanumeric)))

(defn- -coll-gen
  ([schema options] (-coll-gen schema identity options))
  ([schema f options] (gen-fmap f (gen-vector (-min-max schema options) (-child-gen schema options)))))

(defn- gen-vector-distinct [schema m g] (gen-vector-distinct-by schema m identity g))

(defn- -coll-distinct-gen [schema f options]
  (gen-fmap f (gen-vector-distinct schema (-min-max schema options) (-child-gen schema options))))

(defn- ->such-that-opts [schema] {:max-tries 100 :ex-fn #(m/-exception ::such-that-failure (assoc % :schema schema))})
(defn- gen-such-that [schema pred gen] (or (-unreachable gen) (gen/such-that pred gen (->such-that-opts schema))))

(defn -and-gen [schema options]
  (gen-such-that schema (m/validator schema options) (-child-gen schema options)))

(defn- gen-one-of [options gs]
  (if-some [gs (not-empty (into [] (keep -not-unreachable) gs))]
    (if (= 1 (count gs)) (nth gs 0) (gen/one-of gs))
    (-never-gen options)))

(defn- -seqable-gen [schema options]
  (let [{:keys [min]} (-min-max schema options)
        el (-child schema options)]
    (gen-one-of
     options
     (-> []
         (cond->
           (or (nil? min) (zero? min))
           (conj nil-gen))
         (into (map #(-coll-gen schema % options))
               [identity vec eduction #(into-array #?(:clj Object) %)])
         (conj (-coll-distinct-gen schema set options))
         (cond->
           (and (= :tuple (m/type el))
                (= 2 (count (m/children el))))
           (conj (let [[k v] (m/children el)]
                   (generator [:map-of (or (m/properties schema) {}) k v] options))))))))

(defn -or-gen [schema options]
  (gen-one-of options (map #(generator % options) (m/children schema options))))

(defn- -merge-keyword-dispatch-map-into-entries [schema]
  (let [dispatch (-> schema m/properties :dispatch)]
    (cond-> schema
      (keyword? dispatch)
      (mu/transform-entries
       #(map (fn [[k :as e]]
               (cond-> e
                 (not= ::m/default k)
                 (update 2 mu/merge [:map [dispatch [:= nil k]]]))) %)
       (m/options schema)))))

(defn -multi-gen [schema options]
  (gen-one-of options (map #(generator (last %) options) (m/entries (-merge-keyword-dispatch-map-into-entries schema) options))))

(defn- -build-map [kvs]
  (persistent!
   (reduce
    (fn [acc [k v]]
      (cond (and (= k ::m/default) (map? v)) (reduce-kv assoc! acc v)
            (nil? k) acc
            :else (assoc! acc k v)))
    (transient {}) kvs)))

(defn- -entry-gen [[k s] options]
  (cond->> (gen-fmap #(do [k %]) (generator s options)) (-> s m/properties :optional) gen-maybe))

(defn -map-gen [schema options]
  (->> schema m/entries (map #(-entry-gen % options)) gen-tuple (gen-fmap -build-map)))

(defn -map-of-gen [schema options]
  (->> (gen-tuple (map #(generator % options) (m/children schema options)))
       (gen-vector-distinct-by schema (-min-max schema options) #(nth % 0))
       (gen-fmap #(into {} %))))

#?(:clj
   (defn -re-gen [schema options]
     ;; [com.gfredericks/test.chuck "0.2.10"+]
     (if-let [string-from-regex @(dynaload/dynaload 'com.gfredericks.test.chuck.generators/string-from-regex {:default nil})]
       (let [re (or (first (m/children schema options)) (m/form schema options))]
         (string-from-regex (re-pattern (str/replace (str re) #"^\^?(.*?)(\$?)$" "$1"))))
       (m/-fail! :test-chuck-not-available))))

;; # Approach for recursive generators
;;
;; `-ref-gen` is the only place where recursive generators can be created, and we use `gen/recursive-gen`
;; to handle the recursion. The challenge is that gen/recursive-gen requires _two_ arguments: the base
;; case (scalar gen) and the recursive case (container gen). We need to automatically split the schema argument into
;; these two cases.
;;
;; The main insight we use is that a base case for the schema cannot contain recursive references to itself.
;; A particularly useful base case is simply to "delete" all recursive references. To simulate this, we have the concept of
;; an "unreachable" generator, which represents a "deleted" recursive reference.
;;
;; For infinitely expanding schemas, this will return an unreachable generator--when the base case generator is used,
;; the error message in `-never-gen` will advise users that their schema is infinite.
;;
;;
;; Examples of base cases of some recursive schemas:
;;
;; Schema:    [:schema {:registry {::cons [:maybe [:vector [:tuple pos-int? [:ref ::cons]]]]}} ::cons]
;; Base case: [:schema {:registry {::cons [:nil                                            ]}} ::cons]
;;
;; Schema:    [:schema
;;             {:registry {::ping [:tuple [:= "ping"] [:maybe [:ref ::pong]]]
;;                         ::pong [:tuple [:= "pong"] [:maybe [:ref ::ping]]]}}
;;             ::ping]
;; Base case: [:schema
;;             {:registry {::ping [:tuple [:= "ping"] [:maybe [:ref ::pong]]]
;;                         ::pong [:tuple [:= "pong"] :nil                  ]}}
;;             ::ping]
;;
;; Once we have the base case, we first need determine if the schema is recursive---it's recursive
;; if more than one recursive reference was successfully "deleted" while creating the base case (see below for how we determine recursive references).
;; We can then construct the recursive case by providing `gen/recursive-gen` the base case
;; (this is why this particular base case is so useful) and then propagate the (smaller) generator
;; supplied by `gen/recursive-gen` to convert recursive references.

;; ## Identifying schema recursion
;;
;; Refs are uniquely identified by their paired name and scope. If we see a ref with the
;; same name and scope as another ref we've dereferenced previously, we know that this is a recursion
;; point back to the previously seen ref. The rest of this section explains why.
;;
;; Refs resolve via dynamic scope, which means its dereferenced value is the latest binding found
;; while expanding the schema until the point of finding the ref.
;; This makes the (runtime) scope at the ref's location part of a ref's identity---if the scope
;; is different, then it's (possibly) not the same ref because scope determines how schemas
;; transitively expand.
;;
;; To illustrate why a ref's name is an insufficient identifier, here is a schema that is equivalent to `[:= 42]`:
;;
;;   [:schema {:registry {::a [:schema {:registry {::a [:= 42]}}
;;                             ;; (2)
;;                             [:ref ::a]]}}
;;    ;; (1)
;;    [:ref ::a]]
;;
;; If we identify refs just by name, we would have incorrectly detected (2) to be an (infinitely expanding) recursive
;; reference.
;;
;; In studying the previous example, we might think that since (1) and (2) deref to different schemas, it might sufficient to identify refs just by their derefs.
;; Unfortunately this just pushes the problem elsewhere.
;;
;; For example, here is another schema equivalent to `[:= 42]`:
;;
;;   [:schema {:registry {::a [:ref ::b] ;; (2)
;;                        ::b [:schema {:registry {::a [:ref ::b] ;; (4)
;;                                                 ::b [:= 42]}}
;;                             ;; (3)
;;                             [:ref ::a]]}}
;;    ;; (1)
;;    [:ref ::a]]
;;
;; If we identified ::a by its deref, it would look like (3) deref'ing to (4)
;; is a recursion point after witnessing (1) deref'ing to (2), since (2) == (4). Except this
;; is wrong since it's a different ::b at (2) and (4)! OTOH, if we identified (2) and (4) with their
;; dynamic scopes along with their form, they would be clearly different. Indeed, this
;; is another way to identify refs: pairing their derefs with their deref's scopes.
;; It is slightly more direct to use the ref's direct name and scope, which is why
;; we choose that identifier. The more general insight is that any schema is identified by its form+scope
;; (note: but only after trimming the scope of irrelevant bindings, see next pararaph).
;; That insight may be useful for detecting recursion at places other than refs.
;;
;; Ref identifiers could be made smarter by trimming irrelevant entries in identifying scope.
;; Not all scope differences are relevant, so generators may expand more than strictly necessary
;; in the quest to find the "same" ref schema again. It could skip over refs that generate exactly the
;; same values, but their scopes are uninterestingly different (eg., unused bindings are different).
;;
;; For example, the following schema is recursive "in spirit" between (1) and (2), but since ::b
;; changes, the scope will differ, so the recursion will be detected between (2) and itself instead
;; (where the scope is constant):
;;
;;   [:schema {:registry {::a [:schema {:registry {::b :boolean}}
;;                             ;; (2)
;;                             [:or [:ref ::a] [:ref ::b]]]}}
;;    [:schema {:registry {::b :int}}
;;     ;; (1)
;;     [:or [:ref ::a] [:ref ::b]]]]

(defn- -identify-ref-schema [schema]
  {:scope (-> schema m/-options m/-registry mr/-schemas)
   :name (m/-ref schema)})

(defn -ref-gen [schema options]
  (let [ref-id (-identify-ref-schema schema)]
    (or (force (get-in options [::rec-gen ref-id]))
        (let [scalar-ref-gen (delay (-never-gen options))
              dschema (m/deref schema)]
          (cond->> (generator dschema (assoc-in options [::rec-gen ref-id] scalar-ref-gen))
            (realized? scalar-ref-gen) (gen/recursive-gen
                                        #(generator dschema (assoc-in options [::rec-gen ref-id] %))))))))

(defn -=>-gen [schema options]
  (let [output-generator (generator (:output (m/-function-info schema)) options)]
    (gen/return (m/-instrument {:schema schema} (fn [& _] (generate output-generator options))))))

(defn -function-gen [schema options]
  (gen/return (m/-instrument {:schema schema, :gen #(generate % options)} nil options)))

(defn -regex-generator [schema options]
  (cond-> (generator schema options) (not (m/-regex-op? schema)) (-> vector gen-tuple)))

(defn- -re-entry-gen [e options] (-regex-generator (if (vector? e) (get e 2) e) options))

(defn -cat-gen [schema options]
  (->> (m/children schema options) (map #(-re-entry-gen % options)) gen-tuple gen-fcat))

(defn -alt-gen [schema options]
  (->> (m/children schema options) (map #(-re-entry-gen % options)) (gen-one-of options)))

(defn -?-gen [schema options]
  (let [child (-child schema options)]
    (if-some [g (-not-unreachable (generator child options))]
      (if (m/-regex-op? child)
        (gen/one-of [g (gen/return ())])
        (gen/vector g 0 1))
      (gen/return ()))))

(defn -*-gen [schema options]
  (let [child (-child schema options)]
    (cond->> (gen-vector
              (-min-max schema options)
              (generator child options))
      (m/-regex-op? child) gen-fcat)))

(defn -+-gen [schema options]
  (let [child (-child schema options)]
    (cond->> (gen-vector
              (-> (-min-max schema options)
                  ;; When generating from :+ the base minimum value must be 1
                  ;; to ensure that :+ is always fulfilled
                  (update :min (fnil max 1)))
              (generator child options))
      (m/-regex-op? child) gen-fcat)))

(defn -repeat-gen [schema options]
  (or (some-> (-coll-gen schema options) -not-unreachable (cond-> (m/-regex-op? (-child schema options)) gen-fcat))
      (gen/return ())))

(defn -qualified-ident-gen [schema mk-value-with-ns value-with-ns-gen-size pred gen]
  (if-let [namespace-unparsed (:namespace (m/properties schema))]
    (gen-fmap (fn [k] (mk-value-with-ns (name namespace-unparsed) (name k))) value-with-ns-gen-size)
    (gen-such-that schema pred gen)))

(defn -qualified-keyword-gen [schema]
  (-qualified-ident-gen schema keyword gen/keyword qualified-keyword? gen/keyword-ns))

(defn -qualified-symbol-gen [schema]
  (-qualified-ident-gen schema symbol gen/symbol qualified-symbol? gen/symbol-ns))

(defn- gen-elements [es]
  (if (= 1 (count es))
    (gen/return (first es))
    (gen/elements es)))

(defn- double-gen [schema options]
  (gen/double* (merge (let [props (m/properties schema options)]
                        {:infinite? (get props :gen/infinite? false)
                         :NaN? (get props :gen/NaN? false)})
                      (-> (-min-max schema options)
                          (update :min #(some-> % double))
                          (update :max #(some-> % double))))))

(defmulti -schema-generator (fn [schema options] (m/type schema options)) :default ::default)

(defmethod -schema-generator ::default [schema options] (ga/gen-for-pred (m/validator schema options)))

(defmethod -schema-generator 'empty? [_ _] (ga/gen-for-pred empty?))
(defmethod -schema-generator :> [schema options] (gen-double {:min (inc (-child schema options))}))
(defmethod -schema-generator :>= [schema options] (gen-double {:min (-child schema options)}))
(defmethod -schema-generator :< [schema options] (gen-double {:max (dec (-child schema options))}))
(defmethod -schema-generator :<= [schema options] (gen-double {:max (-child schema options)}))
(defmethod -schema-generator := [schema options] (gen/return (-child schema options)))
(defmethod -schema-generator :not= [schema options] (gen-such-that schema #(not= % (-child schema options)) gen/any-printable))
(defmethod -schema-generator 'pos? [_ options] (gen/one-of [(gen-double {:min 0.00001}) (gen-fmap inc gen/nat)]))
(defmethod -schema-generator 'neg? [_ options] (gen/one-of [(gen-double {:max -0.00001}) (gen-fmap (comp dec -) gen/nat)]))
(defmethod -schema-generator :not [schema options] (gen-such-that schema (m/validator schema options) (ga/gen-for-pred any?)))
(defmethod -schema-generator :and [schema options] (-and-gen schema options))
(defmethod -schema-generator :or [schema options] (-or-gen schema options))
(defmethod -schema-generator :orn [schema options] (-or-gen (m/into-schema :or (m/properties schema) (map last (m/children schema)) (m/options schema)) options))
(defmethod -schema-generator ::m/val [schema options] (-child-gen schema options))
(defmethod -schema-generator :map [schema options] (-map-gen schema options))
(defmethod -schema-generator :map-of [schema options] (-map-of-gen schema options))
(defmethod -schema-generator :multi [schema options] (-multi-gen schema options))
(defmethod -schema-generator :vector [schema options] (-coll-gen schema options))
(defmethod -schema-generator :sequential [schema options] (-coll-gen schema options))
(defmethod -schema-generator :set [schema options] (-coll-distinct-gen schema set options))
(defmethod -schema-generator :enum [schema options] (gen-elements (m/children schema options)))
(defmethod -schema-generator :seqable [schema options] (-seqable-gen schema options))
(defmethod -schema-generator :every [schema options] (-seqable-gen schema options)) ;;infinite seqs?
(defmethod -schema-generator :maybe [schema options] (gen-maybe (-child-gen schema options)))
(defmethod -schema-generator :tuple [schema options] (gen-tuple (map #(generator % options) (m/children schema options))))
#?(:clj (defmethod -schema-generator :re [schema options] (-re-gen schema options)))
(defmethod -schema-generator :any [_ _] (ga/gen-for-pred any?))
(defmethod -schema-generator :some [_ _] gen/any-printable)
(defmethod -schema-generator :nil [_ _] nil-gen)
(defmethod -schema-generator :string [schema options] (-string-gen schema options))
(defmethod -schema-generator :int [schema options] (gen/large-integer* (-min-max schema options)))
(defmethod -schema-generator :double [schema options] (double-gen schema options))
(defmethod -schema-generator :float [schema options] (double-gen schema options))
(defmethod -schema-generator :boolean [_ _] gen/boolean)
(defmethod -schema-generator :keyword [_ _] gen/keyword)
(defmethod -schema-generator :symbol [_ _] gen/symbol)
(defmethod -schema-generator :qualified-keyword [schema _] (-qualified-keyword-gen schema))
(defmethod -schema-generator :qualified-symbol [schema _] (-qualified-symbol-gen schema))
(defmethod -schema-generator :uuid [_ _] gen/uuid)

(defmethod -schema-generator :=> [schema options] (-=>-gen schema options))
(defmethod -schema-generator :-> [schema options] (-=>-gen schema options))
(defmethod -schema-generator :function [schema options] (-function-gen schema options))
(defmethod -schema-generator 'ifn? [_ _] gen/keyword)
(defmethod -schema-generator :ref [schema options] (-ref-gen schema options))
(defmethod -schema-generator :schema [schema options] (generator (m/deref schema) options))
(defmethod -schema-generator ::m/schema [schema options] (generator (m/deref schema) options))

(defmethod -schema-generator :merge [schema options] (generator (m/deref schema) options))
(defmethod -schema-generator :union [schema options] (generator (m/deref schema) options))
(defmethod -schema-generator :select-keys [schema options] (generator (m/deref schema) options))

(defmethod -schema-generator :cat [schema options] (-cat-gen schema options))
(defmethod -schema-generator :catn [schema options] (-cat-gen schema options))
(defmethod -schema-generator :alt [schema options] (-alt-gen schema options))
(defmethod -schema-generator :altn [schema options] (-alt-gen schema options))

(defmethod -schema-generator :? [schema options] (-?-gen schema options))
(defmethod -schema-generator :* [schema options] (-*-gen schema options))
(defmethod -schema-generator :+ [schema options] (-+-gen schema options))
(defmethod -schema-generator :repeat [schema options] (-repeat-gen schema options))

;;
;; Creating a generator by different means, centralized under [[-create]]
;;

(defn- -create-from-return [props]
  (when (contains? props :gen/return)
    (gen/return (:gen/return props))))

(defn- -create-from-elements [props]
  (some-> (:gen/elements props) gen-elements))

(extend-protocol Generator
  #?(:clj Object, :cljs default)
  (-generator [schema options]
    (-schema-generator schema (assoc options ::original-generator-schema schema))))

(defn- -create-from-gen
  [props schema options]
  (or (:gen/gen props)
      (when-not (:gen/elements props)
        (-generator schema options))))

(defn- -create-from-schema [props options]
  (some-> (:gen/schema props) (generator options)))

(defn- -create-from-fmap [gen props schema options]
  (when-some [fmap (:gen/fmap props)]
    (gen/fmap (m/eval fmap (or options (m/options schema)))
              gen)))

(defn- -create [schema options]
  (let [props (-merge (m/type-properties schema)
                      (m/properties schema))
        gen (or (-create-from-return props)
                (-create-from-elements props)
                (-create-from-schema props options)
                (-create-from-gen props schema options)
                (m/-fail! ::no-generator {:options options
                                          :schema schema}))]
    (or (-create-from-fmap gen props schema options)
        gen)))

;;
;; public api
;;

(defn generator
  ([?schema]
   (generator ?schema nil))
  ([?schema options]
   (if (::rec-gen options)
     ;; disable cache while calculating recursive schemas. caches don't distinguish options.
     (-create (m/schema ?schema options) options)
     (m/-cached (m/schema ?schema options) :generator #(-create % options)))))

(defn generate
  ([?gen-or-schema]
   (generate ?gen-or-schema nil))
  ([?gen-or-schema {:keys [seed size] :or {size 30} :as options}]
   (let [gen (if (gen/generator? ?gen-or-schema) ?gen-or-schema (generator ?gen-or-schema options))]
     (rose/root (gen/call-gen gen (-random seed) size)))))

(defn sample
  ([?gen-or-schema]
   (sample ?gen-or-schema nil))
  ([?gen-or-schema {:keys [seed size] :or {size 10} :as options}]
   (let [gen (if (gen/generator? ?gen-or-schema) ?gen-or-schema (generator ?gen-or-schema options))]
     (->> (gen/make-size-range-seq size)
          (map #(rose/root (gen/call-gen gen %1 %2))
               (gen/lazy-random-states (-random seed)))
          (take size)))))

;;
;; functions
;;

(defn function-checker
  ([?schema] (function-checker ?schema nil))
  ([?schema {::keys [=>iterations] :or {=>iterations 100} :as options}]
   (let [schema (m/schema ?schema options)
         -try (fn [f] (try [(f) true] (catch #?(:clj Exception, :cljs js/Error) e [e false])))
         check (fn [schema]
                 (let [{:keys [input output guard]} (m/-function-info schema)
                       input-generator (generator input options)
                       valid-output? (m/validator output options)
                       valid-guard? (if guard (m/validator guard options) (constantly true))
                       validate (fn [f args] (as-> (apply f args) $ (and (valid-output? $) (valid-guard? [args $]))))]
                   (fn [f]
                     (let [{:keys [result shrunk]} (->> (prop/for-all* [input-generator] #(validate f %))
                                                        (check/quick-check =>iterations))
                           smallest (-> shrunk :smallest first)]
                       (when-not (true? result)
                         (let [explain-input (m/explain input smallest)
                               [result success] (when-not explain-input (-try (fn [] (apply f smallest))))
                               explain-output (when (and success (not explain-input)) (m/explain output result))
                               explain-guard (when (and success guard (not explain-output)) (m/explain guard [smallest result]))]
                           (cond-> (assoc shrunk ::m/result result)
                             explain-input (assoc ::m/explain-input explain-input)
                             explain-output (assoc ::m/explain-output explain-output)
                             explain-guard (assoc ::m/explain-guard explain-guard)
                             (ex-message result) (-> (update :result ex-message) (dissoc :result-data)))))))))]
     (if (m/-function-info schema)
       (check schema)
       (if (m/-function-schema? schema)
         (let [checkers (map #(function-checker % options) (m/-function-schema-arities schema))]
           (fn [x] (->> checkers (keep #(% x)) (seq))))
         (m/-fail! ::invalid-function-schema {:type (m/-type schema)}))))))

(defn check
  ([?schema f] (check ?schema f nil))
  ([?schema f options]
   (let [schema (m/schema ?schema options)]
     (m/explain (m/-update-options schema #(assoc % ::m/function-checker function-checker)) f))))
