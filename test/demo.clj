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
