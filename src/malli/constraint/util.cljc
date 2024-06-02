(ns malli.constraint.util)

(def composite-constraint-types
  #{:and :or :implies :xor :iff :not
    ;;not composite
    :is :is-not})

(defn -add-gen-key [k]
  [k (keyword "gen" (name k))])

(defn -generator-types [constraint-types]
  (into {} (map (juxt #(keyword "gen" (name %))
                      identity))
        constraint-types))
