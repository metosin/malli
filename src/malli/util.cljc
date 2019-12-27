(ns malli.util
  (:refer-clojure :exclude [merge])
  (:require [malli.core :as m]))

(defn ^:no-doc simplify-map-entry [[k ?p s]]
  (cond
    (not s) [k ?p]
    (and ?p (false? (:optional ?p)) (= 1 (count ?p))) [k s]
    (not (seq ?p)) [k s]
    (false? (:optional ?p)) [k (dissoc ?p :optional) s]
    :else [k ?p s]))

(defn- -entry [[k ?p1 s1 :as e1] [_ ?p2 s2 :as e2] merge-required merge opts]
  (let [required (merge-required (m/required-map-entry? e1) (m/required-map-entry? e2))
        p (clojure.core/merge ?p1 ?p2)]
    (simplify-map-entry [k (assoc p :optional (not required)) (merge s1 s2 opts)])))

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
  | `:merge-default`  | `schema1 schema2 opts -> schema` fn to merge unknown entries
  | `:merge-required` | `boolean boolean -> boolean` fn to resolve how required keys are merged"
  ([?schema1 ?schema2]
   (merge ?schema1 ?schema2 nil))
  ([?schema1 ?schema2 opts]
   (let [[schema1 schema2 :as schemas] [(if ?schema1 (m/schema ?schema1 opts))
                                        (if ?schema2 (m/schema ?schema2 opts))]
         {:keys [merge-default merge-required]
          :or {merge-default (fn [_ s2 _] s2)
               merge-required (fn [_ r2] r2)}} opts]
     (cond
       (not schema1) schema2
       (not schema2) schema1
       (not= :map (m/name schema1) (m/name schema2)) (merge-default schema1 schema2 opts)
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
                                                  (-entry e1 e2 merge-required merge opts)
                                                  e1)))
                                        [] (:form acc))
                                      (assoc acc :form))
                                 (-> acc
                                     (update :form conj e2)
                                     (update :keys conj k2))))
                             {:keys #{}, :form []}
                             (mapcat m/map-entries schemas))))
                   (m/schema opts)))))))

(defn union
  "Union of two schemas. See [[merge]] for more details."
  ([?schema1 ?schema2]
   (union ?schema1 ?schema2 nil))
  ([?schema1 ?schema2 opts]
   (let [merge-default (fn [s1 s2 opts] (if (m/equals s1 s2) s1 (m/schema [:or s1 s2] opts)))
         merge-required (fn [r1 r2] (and r1 r2))]
     (merge ?schema1 ?schema2 (-> opts
                                  (update :merge-default (fnil identity merge-default))
                                  (update :merge-required (fnil identity merge-required)))))))
