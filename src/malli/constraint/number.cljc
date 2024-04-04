(ns malli.constraint.number
  (:require [malli.constraint.util :refer [composite-constraint-types
                                           -add-gen-key
                                           -generator-types]]))

(defn- number-constraints []
  (let [constraint-types (into {} (map (juxt identity identity))
                               (concat composite-constraint-types #{:max :min :< :> :<= :>=}))
        generator-constraint-types (-generator-types (keys constraint-types))
        validator-constraint-types (-> constraint-types
                                       ;; :gen/foo :=> :any
                                       (into (map (fn [c] [c :any])) (keys generator-constraint-types))
                                       (assoc :max :<=
                                              :min :>=))]
    {:flat-property-keys (into #{} (mapcat -add-gen-key)
                               #{:max :min :< :> :<= :>= :not
                                 ;;TODO
                                 ;:even? :odd? :pos? :neg? :multiple
                                 })
     :nested-property-keys (into #{} (mapcat -add-gen-key)
                                 (-> composite-constraint-types (disj :not)))
     :validator-constraint-types validator-constraint-types
     :generator-constraint-types (into validator-constraint-types
                                       generator-constraint-types)}))

(defn schema-constraints []
  {:int (number-constraints)
   :double (-> (number-constraints)
               ;:TODO
               #_(update :flat-property-keys into #{:gen/infinite? :gen/NaN?}))})
