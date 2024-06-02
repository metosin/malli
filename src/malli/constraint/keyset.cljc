(ns malli.constraint.keyset
  (:require [malli.constraint.util :refer [composite-constraint-types
                                           -add-gen-key
                                           -generator-types]]))
            

(defn keyset-constraints []
  (let [constraint-types (into {} (map (juxt identity identity))
                               (concat composite-constraint-types
                                       ;;TODO :sorted
                                       [:disjoint :max :min :contains :dispatch]))
        generator-constraint-types (-generator-types (keys constraint-types))
        validator-constraint-types (-> constraint-types
                                       ;; :gen/foo :=> :any
                                       (into (map (fn [c] [c :any])) (keys generator-constraint-types))
                                       ;;TODO :disjoint :disjoint-keys
                                       (assoc :max :max-count
                                              :min :min-count))]
    {:keyword-sugar :contains
     :flat-property-keys (into #{} (mapcat -add-gen-key)
                               #{:not :contains})
     :nested-property-keys (into #{} (mapcat -add-gen-key)
                                 (-> composite-constraint-types (disj :not) (conj :disjoint :dispatch)))
     :validator-constraint-types validator-constraint-types
     :generator-constraint-types (into validator-constraint-types
                                       generator-constraint-types)}))

(defn schema-constraints []
  (zipmap [:map :set :map-of] (repeat (keyset-constraints))))
