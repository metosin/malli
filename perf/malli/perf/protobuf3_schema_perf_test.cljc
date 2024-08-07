(ns malli.perf.protobuf3-schema-perf-test
  (:require [malli.protobuf3-schema :as protobuf]
            [malli.perf.core :as p]))

(defn transform-perf []
  ;; 28.656211 µs
  (p/bench (protobuf/transform [:map
                                [:id string?]
                                [:metadata [:map
                                            [:created_at inst?]
                                            [:tags [:vector string?]]]]
                                [:data [:vector [:map
                                                 [:name string?]
                                                 [:details [:map
                                                            [:type [:enum :type-a :type-b :type-c]]
                                                            [:properties [:vector [:map
                                                                                   [:key string?]
                                                                                   [:value [:or string? int? boolean?]]
                                                                                   [:nested [:vector [:map
                                                                                                      [:sub_key string?]
                                                                                                      [:sub_value any?]]]]]]]]]]]]])))

(comment
  (transform-perf))
