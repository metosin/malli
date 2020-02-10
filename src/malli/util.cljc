(ns malli.util
  (:refer-clojure :exclude [merge select-keys get-in])
  (:require [malli.core :as m]))

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
    (false? (:optional ?p)) [k (dissoc ?p :optional) s]
    :else [k ?p s]))

(defn- -entry [[k ?p1 s1 :as e1] [_ ?p2 s2 :as e2] merge-required merge options]
  (let [required (merge-required (m/required-map-entry? e1) (m/required-map-entry? e2))
        p (clojure.core/merge ?p1 ?p2)]
    (simplify-map-entry [k (assoc p :optional (not required)) (merge s1 s2 options)])))

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
       :else (let [p (clojure.core/merge (m/properties schema1) (m/properties schema2))]
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
                                      (assoc acc :form))
                                 (-> acc
                                     (update :form conj e2)
                                     (update :keys conj k2))))
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
                                  (update :merge-default (fnil identity merge-default))
                                  (update :merge-required (fnil identity merge-required)))))))

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
         (if (and (= :map (m/name schema options))
                  (-> schema m/properties :closed false? not))
           (update-properties schema assoc :closed true)
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
         (if (and (= :map (m/name schema options))
                  (-> schema m/properties :closed false? not))
           (update-properties schema dissoc :closed)
           schema))))))

(defn select-keys
  "Like [[clojure.core/select-keys]], but for MapSchemas."
  ([?schema keys]
   (select-keys ?schema keys nil))
  ([?schema keys options]
   (let [schema (m/schema ?schema options)
         name (m/name schema)
         key-set (set keys)
         entries (->> (m/map-entries schema options)
                      (filter (fn [[k]] (key-set k))))]
     (m/into-schema name (m/properties schema) entries))))

(defn get-in
  "Like [[clojure.core/get-in]], but for MapSchemas."
  ([?schema ks]
   (get-in ?schema ks nil))
  ([?schema ks options]
   (let [schema (m/schema ?schema options)]
     (loop [sentinel #?(:clj (Object.) :cljs (js/Object.))
            schema schema
            ks (seq ks)]
       (if ks
         (let [v (m/-get schema (first ks) sentinel)]
           (if-not (identical? sentinel v)
             (recur sentinel v (next ks))))
         schema)))))
