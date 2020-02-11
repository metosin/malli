(ns malli.util
  (:refer-clojure :exclude [merge select-keys get get-in dissoc assoc update assoc-in update-in])
  (:require [clojure.core :as c]
            [malli.core :as m]))

(defn ^:no-doc equals
  ([?schema1 ?schema2]
   (equals ?schema1 ?schema2 nil))
  ([?schema1 ?schema2 options]
   (= (m/form ?schema1 options) (m/form ?schema2 options))))

(defn ^:no-doc simplify-map-entry [[k ?p s]]
  (cond
    (not s) [k ?p]
    (and ?p (false? (:optional ?p)) (= 1 (count ?p))) [k s]
    (not (seq ?p)) [k s]
    (false? (:optional ?p)) [k (c/dissoc ?p :optional) s]
    :else [k ?p s]))

(defn- -entry [[k ?p1 s1 :as e1] [_ ?p2 s2 :as e2] merge-required merge options]
  (let [required (merge-required (m/required-map-entry? e1) (m/required-map-entry? e2))
        p (c/merge ?p1 ?p2)]
    (simplify-map-entry [k (c/assoc p :optional (not required)) (merge s1 s2 options)])))

(defn- -open-map? [schema options]
  (and (= :map (m/name schema options)) (-> schema m/properties :closed false? not)))

;;
;; public api
;;

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
   (let [[schema1 schema2 :as schemas] [(if ?schema1 (m/schema ?schema1 options))
                                        (if ?schema2 (m/schema ?schema2 options))]
         {:keys [merge-default merge-required]
          :or {merge-default (fn [_ s2 _] s2)
               merge-required (fn [_ r2] r2)}} options]
     (cond
       (not schema1) schema2
       (not schema2) schema1
       (not= :map (m/name schema1) (m/name schema2)) (merge-default schema1 schema2 options)
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
                             (mapcat m/map-entries schemas))))
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
  (let [schema (m/schema schema)
        properties (apply f (m/properties schema) args)]
    (m/into-schema
      (m/name schema)
      (if (seq properties) properties)
      (m/children schema)
      (m/options schema))))

(defn closed-schema
  "Closes recursively all :map schemas by adding `{:closed true}`
  property, unless schema explicitely open with `{:closed false}`"
  ([schema]
   (closed-schema schema nil))
  ([schema options]
   (m/accept
     schema
     (m/schema-visitor
       (fn [schema]
         (if (-open-map? schema options)
           (update-properties schema c/assoc :closed true)
           schema))))))

(defn open-schema
  "Closes recursively all :map schemas by removing `:closed`
  property, unless schema explicitely open with `{:closed false}`"
  ([schema]
   (open-schema schema nil))
  ([schema options]
   (m/accept
     schema
     (m/schema-visitor
       (fn [schema]
         (if (-open-map? schema options)
           (update-properties schema c/dissoc :closed)
           schema))))))

;;
;; Entries
;;

(defn transform-entries
  "Transforms map-entries with f."
  [schema f options]
  (m/into-schema (m/name schema) (m/properties schema) (f (m/map-entries schema options))))

;;
;; MapSchemas
;;

(defn select-keys
  "Like [[clojure.core/select-keys]], but for MapSchemas."
  ([?schema keys]
   (select-keys ?schema keys nil))
  ([?schema keys options]
   (let [schema (m/schema ?schema options)
         key-set (set keys)]
     (transform-entries schema (partial filter (fn [[k]] (key-set k))) options))))

(defn dissoc
  "Like [[clojure.core/dissoc]], but for MapSchemas."
  ([?schema key]
   (dissoc ?schema key nil))
  ([?schema key options]
   (let [schema (m/schema ?schema options)]
     (transform-entries schema (partial remove (fn [[k]] (= key k))) options))))

;;
;; LensSchemas
;;

(defn get
  "Like [[clojure.core/get]], but for LensSchemas."
  ([?schema k]
   (get ?schema k nil))
  ([?schema k options]
   (let [schema (m/schema (or ?schema :map) options)]
     (m/-get schema k options))))

(defn assoc
  "Like [[clojure.core/assoc]], but for LensSchemas."
  ([?schema key value]
   (assoc ?schema key value nil))
  ([?schema key value options]
   (let [schema (m/schema (or ?schema :map) options)]
     (m/-set schema key value))))

(defn update
  "Like [[clojure.core/update]], but for LensSchemas.
   Works only on Schema instances, not on Schema AST."
  [schema key f & args]
  (let [schema (m/schema schema)]
    (m/-set schema key (apply f (m/-get schema key nil) args))))

(defn get-in
  "Like [[clojure.core/get-in]], but for LensSchemas."
  ([?schema ks]
   (get-in ?schema ks nil))
  ([?schema [k & ks] options]
   (let [schema (get (m/schema (or ?schema :map) options) k)]
     (if ks (get-in schema ks) schema))))

(defn assoc-in
  "Like [[clojure.core/assoc-in]], but for LensSchemas."
  ([?schema ks value]
   (assoc-in ?schema ks value nil))
  ([?schema [k & ks] value options]
   (let [schema (m/schema (or ?schema :map) options)]
     (assoc schema k (if ks (assoc-in (get schema k) ks value) value)))))

(defn update-in
  "Like [[clojure.core/update-in]], but for LensSchemas.
   Works only on Schema instances, not on Schema AST."
  [schema ks f & args]
  (letfn [(up [s [k & ks] f args]
            (assoc s k (if ks (up (get s k) ks f args)
                              (apply f (get s k) args))))]
    (up schema ks f args)))

(defn optional-keys
  "Makes map keys optional."
  ([?schema]
   (optional-keys ?schema nil nil))
  ([?schema ?keys]
   (let [[keys options] (if (map? ?keys) [nil ?keys] [?keys nil])]
     (optional-keys ?schema keys options)))
  ([?schema keys options]
   (let [schema (m/schema ?schema options)
         accept (if keys (set keys) (constantly true))
         mapper (fn [[k :as e]] (if (accept k) (c/update e 1 c/assoc :optional true) e))]
     (transform-entries schema (partial map mapper) options))))

(defn required-keys
  "Makes map keys required."
  ([?schema]
   (required-keys ?schema nil nil))
  ([?schema ?keys]
   (let [[keys options] (if (map? ?keys) [nil ?keys] [?keys nil])]
     (required-keys ?schema keys options)))
  ([?schema keys options]
   (let [schema (m/schema ?schema options)
         accept (if keys (set keys) (constantly true))
         required (fn [p] (let [p' (c/dissoc p :optional)] (if (seq p') p')))
         mapper (fn [[k :as e]] (if (accept k) (c/update e 1 required) e))]
     (transform-entries schema (partial map mapper) options))))
