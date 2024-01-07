;; this file is used to view the generated bundle sizes
;; - npx shadow-cljs run shadow.cljs.build-report app2 /tmp/report.html
;; - npx shadow-cljs release app2 --pseudo-names
(ns malli.app2
  (:require [malli.core :as m]
            [malli.registry :as mr]))

;; - cljs: :closure-defines {malli.registry/type "custom"}
;; -  clj: :jvm-opts ["-Dmalli.registry/type=custom"]

;; just what is needed (1.2kb gzipped)
(def registry
  {:string (m/-string-schema)
   :maybe (m/-maybe-schema)
   :map (m/-map-schema)})

(m/validate
 [:map [:maybe [:maybe :string]]]
 {:maybe "sheep"}
 {:registry registry})
; => true

(mr/set-default-registry! registry)

(m/validate
 [:map [:maybe [:maybe :string]]]
 {:maybe "sheep"})
; => true
