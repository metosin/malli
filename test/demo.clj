(ns demo)

(require '[malli.dev.pretty :as pretty])

(def Adult
  [:map
   [:age [:int {:min 18}]]
   [:home [:map
           [:city :string]
           [:zip :int]]]])

(comment
 (pretty/explain
  Adult
  {:name "Endy"
   :age 17
   :home {:zip 33100}}))

(comment
 (pretty/explain
  [:map
   [:id :int]
   [:tags [:set :keyword]]
   [:address [:map
              [:street :string]
              [:city :string]
              [:zip :int]
              [:lonlat [:tuple :double :double]]]]]
  {:id "123"
   :EXTRA "KEY"
   :tags #{:artesan "coffee" :garden}
   :address {:street "Ahlmanintie 29"
             :city "Tampere"
             :zip 33100
             :lonlat [61.4858322, 23.7832851]}}))

