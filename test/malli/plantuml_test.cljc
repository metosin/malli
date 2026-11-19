(ns malli.plantuml-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [malli.core :as m]
            [malli.plantuml :as plantuml]
            [malli.registry :as mr]))

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

(deftest options-registry-test
  (let [registry (mr/composite-registry
                  m/default-registry
                  (mr/registry {"Curve" [:enum :curve25519 :p256]
                                "Key" [:map [:curve "Curve"] [:value [:map [:x int?]]]]}))]

    (testing "external schemas referenced from the options registry are shown as entities"
      (is (trimmed=
           "@startuml
            entity :malli.dot/schema {
             \"Curve\"
            }
            entity Curve {
             [:enum :curve25519 :p256]
            }
            :malli.dot/schema o-- Curve
            @enduml"
           (plantuml/transform "Curve" {:registry registry}))))

    (testing "external schemas are normalized and their references followed"
      (is (trimmed=
           "@startuml
            entity :malli.dot/schema {
             :key \"Key\"
            }
            entity Curve {
             [:enum :curve25519 :p256]
            }
            entity Key {
             :curve \"Curve\"
             :value \"Key$Value\"
            }
            abstract Key$Value {
             :x int?
            }
            :malli.dot/schema o-- Key
            Key o-- Curve
            Key *-- Key$Value
            @enduml"
           (plantuml/transform [:map [:key "Key"]] {:registry registry}))))))
