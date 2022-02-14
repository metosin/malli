(ns malli.dev.pretty-test
  (:require [clojure.test :refer [deftest is]]
            [malli.core-test :as mct]
            [malli.dev.pretty :as pretty]))

(deftest explain-test
  (is (nil? (pretty/explain :string "1")))
  (is (re-find
       #"Validation Error"
       (with-out-str
         (is (mct/results= {:schema :string
                            :value 1
                            :errors [{:path []
                                      :in []
                                      :schema :string
                                      :value 1
                                      :message "should be a string"}]}
                           (pretty/explain :string 1)))))))
