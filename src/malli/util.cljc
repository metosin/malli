(ns malli.util
  (:refer-clojure :exclude [merge select-keys find get get-in dissoc assoc update assoc-in update-in])
  (:require [clojure.core :as c]
            [malli.core :as m]))

(declare path->in)

(defn ^:no-doc equals
  ([?schema1 ?schema2]
   (equals ?schema1 ?schema2 nil))
  ([?schema1 ?schema2 options]
   (= (m/form ?schema1 options) (m/form ?schema2 options))))

(defn -simplify-map-entry [[k ?p s]]
  (cond
    (not s) [k ?p]
    (and ?p (false? (:optional ?p)) (= 1 (count ?p))) [k s]
    (not (seq ?p)) [k s]
    (false? (:optional ?p)) [k (c/dissoc ?p :optional) s]
    :else [k ?p s]))

(defn -required-map-entry? [[_ ?p]]
  (not (and (map? ?p) (true? (:optional ?p)))))

(defn- -entry [[k ?p1 s1 :as e1] [_ ?p2 s2 :as e2] merge-required merge options]
  (let [required (merge-required (-required-map-entry? e1) (-required-map-entry? e2))
        p (c/merge ?p1 ?p2)]
    (-simplify-map-entry [k (c/assoc p :optional (not required)) (merge s1 s2 options)])))

(defn- -open-map? [schema options]
  (and (= :map (m/type schema options)) (-> schema m/properties :closed false? not)))

;;
;; public api
;;

(defn find-first
  "Prewalks the Schema recursively with a 3-arity fn [schema path options], returns with
  and as soon as the function returns non-null value."
  ([?schema f]
   (find-first ?schema f nil))
  ([?schema f options]
   (let [result (atom nil)]
     (m/-walk
       (m/schema ?schema options)
       (reify m/Walker
         (-accept [_ s path options] (not (or @result (reset! result (f s path options)))))
         (-inner [this s path options] (if-not @result (m/-walk s this path options)))
         (-outer [_ _ _ _ _]))
       [] options)
     @result)))

(defn merge
  "Merges two schemas into one with the following rules:

  * if either schemas is `nil`, the other one is used, regardless of value
  * with two :map schemas, both keys and values are merged
  * with two :map entries, `:merge-entries` fn is used (default last one wins)
  * with any other schemas, `:merge-default` fn is used (default last one wins)

  | key               | description
  | ------------------|-------------
  | `:merge-default`  | `schema1 schema2 options -> schema` fn to merge unknown entries
  | `:merge-required` | `boolean boolean -> boolean` fn to resolve how required keys are merged"
  ([?schema1 ?schema2]
   (merge ?schema1 ?schema2 nil))
  ([?schema1 ?schema2 options]
   (let [[schema1 schema2 :as schemas] [(if ?schema1 (m/deref-all (m/schema ?schema1 options)))
                                        (if ?schema2 (m/deref-all (m/schema ?schema2 options)))]
         {:keys [merge-default merge-required]
          :or {merge-default (fn [_ s2 _] s2)
               merge-required (fn [_ r2] r2)}} options]
     (cond
       (not schema1) schema2
       (not schema2) schema1
       (not= :map (m/type schema1) (m/type schema2)) (merge-default schema1 schema2 options)
       :else (let [p (c/merge (m/properties schema1) (m/properties schema2))]
               (-> [:map]
                   (cond-> p (conj p))
                   (into (:form
                           (reduce
                             (fn [{:keys [keys] :as acc} [k2 :as e2]]
                               (if (keys k2)
                                 (->> (reduce
                                        (fn [acc' [k1 :as e1]]
                                          (conj acc'
                                                (if (= k1 k2)
                                                  (-entry e1 e2 merge-required merge options)
                                                  e1)))
                                        [] (:form acc))
                                      (c/assoc acc :form))
                                 (-> acc
                                     (c/update :form conj e2)
                                     (c/update :keys conj k2))))
                             {:keys #{}, :form []}
                             (mapcat m/children schemas))))
                   (m/schema options)))))))

(defn union
  "Union of two schemas. See [[merge]] for more details."
  ([?schema1 ?schema2]
   (union ?schema1 ?schema2 nil))
  ([?schema1 ?schema2 options]
   (let [merge-default (fn [s1 s2 options] (if (equals s1 s2) s1 (m/schema [:or s1 s2] options)))
         merge-required (fn [r1 r2] (and r1 r2))]
     (merge ?schema1 ?schema2 (-> options
                                  (c/update :merge-default (fnil identity merge-default))
                                  (c/update :merge-required (fnil identity merge-required)))))))

(defn update-properties
  "Returns a Schema instance with updated properties."
  [schema f & args]
  (let [properties (apply f (m/properties schema) args)]
    (m/into-schema
      (m/type schema)
      (if (seq properties) properties)
      (m/children schema)
      (m/options schema))))

(defn closed-schema
  "Closes recursively all :map schemas by adding `{:closed true}`
  property, unless schema explicitely open with `{:closed false}`"
  ([?schema]
   (closed-schema ?schema nil))
  ([?schema options]
   (m/walk
     ?schema
     (m/schema-walker
       (fn [schema]
         (if (-open-map? schema options)
           (update-properties schema c/assoc :closed true)
           schema)))
     options)))

(defn open-schema
  "Closes recursively all :map schemas by removing `:closed`
  property, unless schema explicitely open with `{:closed false}`"
  ([?schema]
   (open-schema ?schema nil))
  ([?schema options]
   (m/walk
     ?schema
     (m/schema-walker
       (fn [schema]
         (if (-open-map? schema options)
           (update-properties schema c/dissoc :closed)
           schema)))
     options)))

(defn subschemas
  "Returns all subschemas for unique paths as a vector of maps with :schema, :path and :in keys.
   Walks over :schema references and top-level :refs. See [[malli.core/-walk]] for all options."
  ([?schema]
   (subschemas ?schema nil))
  ([?schema options]
   (let [schema (m/schema ?schema options)
         options (let [ref (and (= :ref (m/type schema)) (m/-ref schema))]
                   (-> options
                       (clojure.core/update ::m/walk-schema-refs (fnil identity true))
                       (clojure.core/update ::m/walk-refs (fn [f] #(or (= ref %) ((m/-boolean-fn f) %))))))
         state (atom [])]
     (find-first schema (fn [s p _] (swap! state conj {:path p, :in (path->in schema p), :schema s}) nil) options)
     @state)))

(defn distinct-by
  "Returns a sequence of distict (f x) values)"
  [f coll]
  (let [seen (atom #{})]
    (filter (fn [x] (let [v (f x)] (if-not (@seen v) (swap! seen conj v)))) coll)))

(defn path->in
  "Returns a value path for a given Schema and schema path"
  [schema path]
  (loop [i 0, s schema, acc []]
    (or (and (>= i (count path)) acc)
        (recur (inc i) (m/-get s (path i) nil) (cond-> acc (m/-keep s) (conj (path i)))))))

(defn in->paths
  "Returns a vector of schema paths for a given Schema and value path"
  [schema in]
  (let [state (atom [])
        in-equals (fn [[x & xs] [y & ys]] (cond (and x (= x y)) (recur xs ys), (= x y) true, (= ::m/in x) (recur xs ys)))
        parent-exists (fn [v1 v2] (let [i (min (count v1) (count v2))] (= (subvec v1 0 i) (subvec v2 0 i))))]
    (find-first
      schema
      (fn [_ path _]
        (when (and (in-equals (path->in schema path) in) (not (some #(parent-exists path %) @state)))
          (swap! state conj path) nil)))
    @state))

;;
;; MapSchemas
;;

(defn transform-entries
  "Transforms entries with f."
  [?schema f options]
  (let [schema (m/deref-all (m/schema ?schema options))]
    (m/into-schema (m/type schema) (m/properties schema) (f (m/children schema)) options)))

(defn optional-keys
  "Makes map keys optional."
  ([?schema]
   (optional-keys ?schema nil nil))
  ([?schema ?keys]
   (let [[keys options] (if (map? ?keys) [nil ?keys] [?keys nil])]
     (optional-keys ?schema keys options)))
  ([?schema keys options]
   (let [accept (if keys (set keys) (constantly true))
         mapper (fn [[k :as e]] (if (accept k) (c/update e 1 c/assoc :optional true) e))]
     (transform-entries ?schema #(map mapper %) options))))

(defn required-keys
  "Makes map keys required."
  ([?schema]
   (required-keys ?schema nil nil))
  ([?schema ?keys]
   (let [[keys options] (if (map? ?keys) [nil ?keys] [?keys nil])]
     (required-keys ?schema keys options)))
  ([?schema keys options]
   (let [accept (if keys (set keys) (constantly true))
         required (fn [p] (let [p' (c/dissoc p :optional)] (if (seq p') p')))
         mapper (fn [[k :as e]] (if (accept k) (c/update e 1 required) e))]
     (transform-entries ?schema #(map mapper %) options))))

(defn select-keys
  "Like [[clojure.core/select-keys]], but for MapSchemas."
  ([?schema keys]
   (select-keys ?schema keys nil))
  ([?schema keys options]
   (let [key-set (set keys)]
     (transform-entries ?schema #(filter (fn [[k]] (key-set k)) %) options))))

(defn dissoc
  "Like [[clojure.core/dissoc]], but for MapSchemas."
  ([?schema key]
   (dissoc ?schema key nil))
  ([?schema key options]
   (transform-entries ?schema #(remove (fn [[k]] (= key k)) %) options)))

(defn find
  "Like [[clojure.core/find]], but for MapSchemas."
  ([?schema k]
   (find ?schema k nil))
  ([?schema k options]
   (let [schema (m/schema (or ?schema :map) options)]
     (if schema (m/-get schema [::m/find k] nil)))))

;;
;; LensSchemas
;;

(defn get
  "Like [[clojure.core/get]], but for LensSchemas."
  ([?schema k]
   (get ?schema k nil nil))
  ([?schema k default]
   (get ?schema k default nil))
  ([?schema k default options]
   (let [schema (m/schema (or ?schema :map) options)]
     (if schema (m/-get schema k default)))))

(defn assoc
  "Like [[clojure.core/assoc]], but for LensSchemas."
  ([?schema key value]
   (assoc ?schema key value nil))
  ([?schema key value options]
   (m/-set (m/schema ?schema options) key value)))

(defn update
  "Like [[clojure.core/update]], but for LensSchema instances."
  [schema key f & args]
  (m/-set schema key (apply f (get schema key (m/schema :map (m/options schema))) args)))

(defn get-in
  "Like [[clojure.core/get-in]], but for LensSchemas."
  ([?schema ks]
   (get-in ?schema ks nil nil))
  ([?schema ks default]
   (get-in ?schema ks default nil))
  ([?schema [k & ks] default options]
   (let [schema (m/schema (or ?schema :map) options)]
     (if-not k
       schema
       (let [sentinel #?(:clj (Object.), :cljs (js-obj))
             schema (get schema k sentinel)]
         (cond
           (identical? schema sentinel) default
           ks (get-in schema ks default)
           :else schema))))))

(defn assoc-in
  "Like [[clojure.core/assoc-in]], but for LensSchemas."
  ([?schema ks value]
   (assoc-in ?schema ks value nil))
  ([?schema [k & ks] value options]
   (let [schema (m/schema ?schema options)]
     (assoc schema k (if ks (assoc-in (get schema k (m/schema :map (m/options schema))) ks value) value)))))

(defn update-in
  "Like [[clojure.core/update-in]], but for LensSchemas."
  [schema ks f & args]
  (letfn [(up [s [k & ks] f args]
            (assoc s k (if ks (up (get s k (m/schema :map (m/options schema))) ks f args)
                              (apply f (get s k) args))))]
    (up schema ks f args)))

;;
;; map-syntax
;;

(defn -map-syntax-walker [schema _ children _]
  (let [properties (m/properties schema)]
    (cond-> {:type (m/type schema)}
            (seq properties) (clojure.core/assoc :properties properties)
            (seq children) (clojure.core/assoc :children children))))

(defn to-map-syntax
  ([?schema] (to-map-syntax ?schema nil))
  ([?schema options] (m/walk ?schema -map-syntax-walker options)))

(defn from-map-syntax
  ([m] (from-map-syntax m nil))
  ([{:keys [type properties children] :as m} options]
   (if (map? m)
     (let [<-child (if (-> children first vector?) (fn [f] #(clojure.core/update % 2 f)) identity)
           [properties options] (m/-properties-and-options properties options m/-form)]
       (m/into-schema type properties (mapv (<-child #(from-map-syntax % options)) children) options))
     m)))

;;
;; Schemas
;;

(defn -reducing [f]
  (fn [_ [first & rest :as children] options]
    (let [children (mapv #(m/schema % options) children)]
      [children (mapv m/form children) (reduce #(f %1 %2 options) first rest)])))

(defn -applying [f]
  (fn [_ children options]
    [(clojure.core/update children 0 #(m/schema % options))
     (clojure.core/update children 0 #(m/form % options))
     (apply f (conj children options))]))

(defn -util-schema [{:keys [type min max childs type-properties fn]}]
  ^{:type ::m/into-schema}
  (reify m/IntoSchema
    (-into-schema [_ properties children options]
      (m/-check-children! type properties children {:min min, :max max})
      (let [[children forms schema] (fn properties (vec children) options)
            walkable-childs (if childs (subvec children 0 childs) children)
            form (m/-create-form type properties forms)]
        ^{:type ::m/schema}
        (reify
          m/Schema
          (-type [_] type)
          (-type-properties [_] type-properties)
          (-validator [_] (m/-validator schema))
          (-explainer [_ path] (m/-explainer schema path))
          (-transformer [this transformer method options]
            (m/-parent-children-transformer this [schema] transformer method options))
          (-walk [this walker path options]
            (if (m/-accept walker this path options)
              (m/-outer walker this path (m/-inner-indexed walker path walkable-childs options) options)))
          (-properties [_] properties)
          (-options [_] options)
          (-children [_] children)
          (-form [_] form)
          m/LensSchema
          (-keep [_])
          (-get [_ key default] (clojure.core/get children key default))
          (-set [_ key value] (m/into-schema type properties (clojure.core/assoc children key value)))
          m/RefSchema
          (-ref [_])
          (-deref [_] schema))))))

(defn -merge [] (-util-schema {:type :merge, :fn (-reducing merge)}))
(defn -union [] (-util-schema {:type :union, :fn (-reducing union)}))
(defn -select-keys [] (-util-schema {:type :select-keys, :childs 1, :min 2, :max 2, :fn (-applying select-keys)}))

(defn schemas [] {:merge (-merge)
                  :union (-union)
                  :select-keys (-select-keys)})
