(ns malli.experimental.time.json-schema-test
  (:require [malli.experimental.time-test :refer [r]]
            [malli.experimental.time.json-schema]
            [malli.json-schema :as json]
            [clojure.test :as t]))

(t/deftest time-formats
  (t/is
   (= {:type "object",
       :properties {:date {:$ref "#/definitions/time.local-date"},
                    :time {:$ref "#/definitions/time.offset-time"},
                    :date-time {:$ref "#/definitions/time.offset-date-time"},
                    :duration {:$ref "#/definitions/time.duration"}},
       :required [:date :time :date-time :duration],
       :definitions {"time.local-date" {:type "string", :format "date"},
                     "time.offset-time" {:type "string", :format "time"},
                     "time.offset-date-time" {:type "string", :format "date-time"},
                     "time.duration" {:type "string", :format "duration"}}}
      (json/transform
       [:map
        [:date :time/local-date]
        [:time :time/offset-time]
        [:date-time :time/offset-date-time]
        [:duration :time/duration]]
       {:registry r}))))
