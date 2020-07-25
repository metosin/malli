;; this file is used to view the generated bundle sizes
;; - npx shadow-cljs run shadow.cljs.build-report app2 /tmp/report.html
;; - npx shadow-cljs release app2 --pseudo-names
(ns malli.app2
  (:require [malli.core :as m]
            [malli.options :as mo]))

;; - cljs: :closure-defines {malli.options/type "custom"}
;; -  clj: :jvm-opts ["-Dmalli.options/type=custom"]

;; just what is needed (1.2kb gzipped)
(def options
  {:registry {:string (m/-string-schema)
              :maybe (m/-maybe-schema)
              :map (m/-map-schema)}})

(m/validate
  [:map [:maybe [:maybe :string]]]
  {:maybe "sheep"}
  options)
; => true

(mo/reset-custom-default-options! options)

(m/validate
  [:map [:maybe [:maybe :string]]]
  {:maybe "sheep"})
; => true
