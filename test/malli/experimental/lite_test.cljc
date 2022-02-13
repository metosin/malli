(ns malli.experimental.lite-test
  (:require [clojure.test :refer [deftest is testing]]
            [malli.experimental.lite :as l]
            [malli.core :as m]))

(deftest schema-test
  (let [lschema (l/schema
                 {:int int?
                  :opt (l/optional {:a int?})
                  :maybe (l/maybe {:a int?})
                  :set (l/set {:a int?})
                  :vector (l/vector {:a int?})
                  :nested {:int int?
                           :map-of (l/map-of int? {:a int?})
                           :tuple (l/tuple int? {:a int?})
                           :and (l/and {:a int?} :map)
                           :or (l/or {:a int?} {:b int?})}})
        mschema (m/schema
                 [:map
                  [:int int?]
                  [:opt {:optional true} [:map [:a int?]]]
                  [:maybe [:maybe [:map [:a int?]]]]
                  [:set [:set [:map [:a int?]]]]
                  [:vector [:vector [:map [:a int?]]]]
                  [:nested [:map
                            [:int int?]
                            [:map-of [:map-of int? [:map [:a int?]]]]
                            [:tuple [:tuple int? [:map [:a int?]]]]
                            [:and [:and [:map [:a int?]] :map]]
                            [:or [:or [:map [:a int?]] [:map [:b int?]]]]]]])]
    (is (= (m/form lschema) (m/form mschema)))

    (testing "with options"
      (let [options {:registry (assoc (m/default-schemas) ::id :int)}]
        (is (= (m/form (binding [l/*options* options] (l/schema {:id ::id, :nested {:id ::id}})))
               (m/form [:map [:id ::id] [:nested [:map [:id ::id]]]] options)))))))
