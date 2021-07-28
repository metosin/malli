(ns malli.plantuml-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.string :as str]
            [malli.plantuml :as plantuml]))

(defn trimmed= [s1 s2]
  (letfn [(trim [x] (str/trim (str/replace x #"\s+" " ")))]
    (= (trim s1) (trim s2))))

(deftest transform-test

  (is (trimmed=
       "@startuml
        entity :malli.dot/schema {
         [:enum {:title \"enum\"} \"S\" \"M\" \"L\"]
       }
       @enduml"
        (plantuml/transform
          [:enum {:title "enum"} "S" "M" "L"])))

  (is (trimmed=
       "@startuml
        entity :malli.dot/schema {
         :x :string
        }
        @enduml"
        (plantuml/transform
          [:map {:x 1}
           [:x {:x 1} :string]])))

  (is (trimmed=
       "@startuml
       entity :malli.dot/schema {
        [:and int? [:< 100]]
       }
       @enduml"
       (plantuml/transform [:and int? [:< 100]]))))
