# Tips

## Removing Schemas based on a property

```clj
(require '[malli.core :as m])

(def Schema
  [:map
   [:user map?]
   [:profile map?]
   [:tags [:vector [int? {:deleteMe true}]]]
   [:nested [:map [:x [:tuple {:deleteMe true} string? string?]]]]
   [:token [string? {:deleteMe true}]]])

(m/accept
  Schema
  (fn [schema children _ options]
    (when-not (:deleteMe (m/properties schema))
      (let [children (if (m/map-entries schema) (filterv last children) children)]
        (try (m/into-schema (m/name schema) (m/properties schema) children options)
             (catch #?(:clj Exception, :cljs js/Error) _))))))
;[:map
; [:user map?] 
; [:profile map?] 
; [:nested :map]]
```
