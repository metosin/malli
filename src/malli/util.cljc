(ns malli.util
  (:refer-clojure :exclude [merge select-keys find get get-in dissoc assoc update assoc-in update-in keys])
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

(defn- -ok-to-close-or-open? [schema options]
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
        (-inner [this s path options] (when-not @result (m/-walk s this path options)))
        (-outer [_ _ _ _ _]))
      [] options)
     @result)))

(defn merge
  "Merges two schemas into one with the following rules:

  * if either schemas is `nil`, the other one is used, regardless of value
  * with two :map schemas, both keys and values are merged
  * for :and schemas, the first child is used in merge, rest kept as-is
  * with two :map entries, `:merge-entries` fn is used (default last one wins)
  * with any other schemas, `:merge-default` fn is used (default last one wins)

  | key               | description
  | ------------------|-------------
  | `:merge-default`  | `schema1 schema2 options -> schema` fn to merge unknown entries
  | `:merge-required` | `boolean boolean -> boolean` fn to resolve how required keys are merged"
  ([?schema1 ?schema2]
   (merge ?schema1 ?schema2 nil))
  ([?schema1 ?schema2 options]
   (let [s1 (when ?schema1 (m/deref-all (m/schema ?schema1 options)))
         s2 (when ?schema2 (m/deref-all (m/schema ?schema2 options)))
         t1 (when s1 (m/type s1))
         t2 (when s2 (m/type s2))
         {:keys [merge-default merge-required]
          :or {merge-default (fn [_ s2 _] s2)
               merge-required (fn [_ r2] r2)}} options
         bear (fn [p1 p2] (if (and p1 p2) (c/merge p1 p2) (or p1 p2)))
         tear (fn [t s] (if (= :map t) [nil s] (concat [(m/properties s)] (m/children s))))
         join (fn [[p1 c1 & cs1] [p2 c2 & cs2]]
                (m/into-schema :and (bear p1 p2) (concat [(merge c1 c2 options)] cs1 cs2) options))]
     (cond
       (nil? s1) s2
       (nil? s2) s1
       (not (and (-> t1 #{:map :and}) (-> t2 #{:map :and}))) (merge-default s1 s2 options)
       (not (and (-> t1 (= :map)) (-> t2 (= :map)))) (join (tear t1 s1) (tear t2 s2))
       :else (let [p (bear (m/-properties s1) (m/-properties s2))
                   ks (atom #{})
                   children (reduce (fn [form [k2 :as e2]]
                                      (if (@ks k2)
                                        (reduce (fn [acc' [k1 :as e1]]
                                                  (conj acc' (if (= k1 k2)
                                                               (-entry e1 e2 merge-required merge options)
                                                               e1))) [] form)
                                        (do (swap! ks conj k2) (conj form e2))))
                                    [] (into (m/-children s1) (m/-children s2)))]
               (m/into-schema :map p children options))))))

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
  [?schema f & args]
  (let [schema (m/schema ?schema)]
    (apply m/-update-properties schema f args)))

(defn closed-schema
  "Maps are implicitly open by default. They can be explicitly closed or
  open by specifying the `{:closed (true|false)}` property.

  This function converts implicitly open maps to explicitly closed
  maps, recursively. Explicitly open maps are left untouched.

  See [[open-schema]]"
  ([?schema]
   (closed-schema ?schema nil))
  ([?schema options]
   (m/walk
    ?schema
    (m/schema-walker
     (fn [schema]
       (if (-ok-to-close-or-open? schema options)
         (update-properties schema c/assoc :closed true)
         schema)))
    options)))

(defn open-schema
  "Maps are implicitly open by default. They can be explicitly closed or
  open by specifying the `{:closed (true|false)}` property.

  This function converts explicitly closed maps to implicitly open
  maps, recursively. Explicitly open maps are left untouched.

  See [[closed-schema]]"
  ([?schema]
   (open-schema ?schema nil))
  ([?schema options]
   (m/walk
    ?schema
    (m/schema-walker
     (fn [schema]
       (if (-ok-to-close-or-open? schema options)
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
  "Returns a sequence of distinct (f x) values)"
  [f coll]
  (let [seen (atom #{})]
    (filter (fn [x] (let [v (f x)] (when-not (@seen v) (swap! seen conj v)))) coll)))

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

(defn data-explainer
  "Like `m/explainer` but output is pure clojure data. Schema objects have been replaced with their m/form.
   Useful when you need to serialise errrors."
  ([?schema]
   (data-explainer ?schema nil))
  ([?schema options]
   (let [explainer' (m/explainer ?schema options)]
     (fn data-explainer
       ([value]
        (data-explainer value [] []))
       ([value in acc]
        (some-> (explainer' value in acc)
                (c/update :schema m/form)
                (c/update :errors (partial mapv #(c/update % :schema m/form)))))))))

(defn explain-data
  "Explains a value against a given schema. Like `m/explain` but output is pure clojure data.
  Schema objects have been replaced with their `m/form`. Useful when you need to serialise errrors.

  Creates the `mu/data-explainer` for every call. When performance matters, (re-)use `mu/data-explainer` instead."
  ([?schema value]
   (explain-data ?schema value nil))
  ([?schema value options]
   ((data-explainer ?schema options) value [] [])))

;;
;; EntrySchemas
;;

(defn transform-entries
  "Transforms entries with f."
  ([?schema f]
   (transform-entries ?schema f nil))
  ([?schema f options]
   (let [schema (m/deref-all (m/schema ?schema options))]
     (m/into-schema (m/-parent schema) (m/-properties schema) (f (m/-children schema)) (or (m/options schema) options)))))

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
         required (fn [p] (let [p' (c/dissoc p :optional)] (when (seq p') p')))
         mapper (fn [[k :as e]] (if (accept k) (c/update e 1 required) e))]
     (transform-entries ?schema #(map mapper %) options))))

(defn select-keys
  "Like [[clojure.core/select-keys]], but for EntrySchemas."
  ([?schema keys]
   (select-keys ?schema keys nil))
  ([?schema keys options]
   (let [key-set (set keys)]
     (transform-entries ?schema #(filter (fn [[k]] (key-set k)) %) options))))

(defn rename-keys
  "Like [[clojure.set/rename-keys]], but for EntrySchemas. Collisions are resolved in favor of the renamed key, like `assoc`-ing."
  ([?schema kmap]
   (rename-keys ?schema kmap nil))
  ([?schema kmap options]
   (transform-entries
    ?schema
    (fn [entries]
      (let [source-keys (set (c/keys kmap))
            target-keys (set (vals kmap))
            remove-conflicts (fn [[k]] (or (source-keys k) (not (target-keys k))))
            alter-keys (fn [[k m v]] [(c/get kmap k k) m v])]
        (->> entries (filter remove-conflicts) (map alter-keys))))
    options)))

(defn dissoc
  "Like [[clojure.core/dissoc]], but for EntrySchemas."
  ([?schema key]
   (dissoc ?schema key nil))
  ([?schema key options]
   (transform-entries ?schema #(remove (fn [[k]] (= key k)) %) options)))

(defn find
  "Like [[clojure.core/find]], but for EntrySchemas."
  ([?schema k]
   (find ?schema k nil))
  ([?schema k options]
   (let [schema (m/schema (or ?schema :map) options)]
     (when schema (m/-get schema [::m/find k] nil)))))

(defn keys
  "Like [[clojure.core/keys]], but for EntrySchemas."
  [?schema]
  (when-let [ents (m/entries ?schema)]
    (for [[k _] ents]
      k)))

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
     (when schema (m/-get schema k default)))))

(defn assoc
  "Like [[clojure.core/assoc]], but for LensSchemas."
  ([?schema key value]
   (assoc ?schema key value nil))
  ([?schema key value options]
   (m/-set (m/schema ?schema options) key value)))

(defn update
  "Like [[clojure.core/update]], but for LensSchema instances."
  [schema key f & args]
  (m/-set (m/schema schema) key (apply f (get schema key) args)))

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
    (-type [_] type)
    (-type-properties [_] type-properties)
    (-properties-schema [_ _])
    (-children-schema [_ _])
    (-into-schema [parent properties children options]
      (m/-check-children! type properties children min max)
      (let [[children forms schema] (fn properties (vec children) options)
            form (delay (m/-create-form type properties forms options))
            cache (m/-create-cache options)]
        ^{:type ::m/schema}
        (reify
          m/Schema
          (-validator [_] (m/-validator schema))
          (-explainer [_ path] (m/-explainer schema path))
          (-parser [_] (m/-parser schema))
          (-unparser [_] (m/-unparser schema))
          (-transformer [this transformer method options]
            (m/-parent-children-transformer this [schema] transformer method options))
          (-walk [this walker path options]
            (let [children (if childs (subvec children 0 childs) children)]
              (when (m/-accept walker this path options)
                (m/-outer walker this path (m/-inner-indexed walker path children options) options))))
          (-properties [_] properties)
          (-options [_] options)
          (-children [_] children)
          (-parent [_] parent)
          (-form [_] @form)
          m/Cached
          (-cache [_] cache)
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
