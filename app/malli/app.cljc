;; this file is used to view the generated bundle sizes
;; - npx shadow-cljs run shadow.cljs.build-report app /tmp/report.html
;; - npx shadow-cljs release app --pseudo-names
(ns malli.app
  (:require [malli.core :as m]))

(m/validate
 [:map [:maybe [:maybe :string]]]
 {:maybe "sheep"})
; => true
