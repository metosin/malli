;; this file is used to view the generated bundle sizes
;; - npx shadow-cljs run shadow.cljs.build-report app3 /tmp/report.html
;; - npx shadow-cljs release app3 --pseudo-names
(ns malli.app3
  (:require [malli.core :as m]))

;; - cljs: :closure-defines {malli.options/type "empty"}
;; -  clj: :jvm-opts ["-Dmalli.options/type=empty"]

;; just what is needed (1.2kb gzipped)
(m/validate
  [:string {:min 1}]
  "sheep"
  {:registry {:string (m/-string-schema)}})
; => true
