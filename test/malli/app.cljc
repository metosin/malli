(ns malli.app
  (:require [malli.core :as m]))

;; this file is used to view the generated bundle sizes
;; npx shadow-cljs run shadow.cljs.build-report malli /tmp/report.html
(m/validate :map {})
