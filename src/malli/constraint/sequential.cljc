(ns malli.constraint.sequential
  (:require [malli.constraint.util :refer [composite-constraint-types
                                           -add-gen-key
                                           -generator-types]]))

(defn- sequential-constraints []
  (let [constraint-types (into {} (map (juxt identity identity))
                               (concat composite-constraint-types #{:max :min :distinct :sorted}))
        generator-constraint-types (-generator-types (keys constraint-types))
        validator-constraint-types (-> constraint-types
                                       ;; :gen/foo :=> :any
                                       (into (map (fn [c] [c :any])) (keys generator-constraint-types))
                                       (assoc :max :max-count
                                              :min :min-count
                                              :distinct :distinct-seqable
                                              :sorted :sorted-sequential))]
    {:flat-property-keys (into #{} (mapcat -add-gen-key)
                               #{:max :min :distinct :sorted})
     :generator-constraint-types (into validator-constraint-types
                                       generator-constraint-types)
     :validator-constraint-types validator-constraint-types}))

(defn schema-constraints []
  (zipmap [:vector :sequential] (repeat (sequential-constraints))))
