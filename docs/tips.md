# Tips

## Removing Schemas based on a property

Schemas can be walked over recursively using `m/accept`:

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
    ;; return nil if Schema has the property 
    (when-not (:deleteMe (m/properties schema))
      ;; there are two syntaxes: normal and the map-entry, handle separatly
      (let [children (if (m/map-entries schema) (filterv last children) children)]
        ;; create a new Schema with the updated children, or return nil
        (try (m/into-schema (m/name schema) (m/properties schema) children options)
             (catch #?(:clj Exception, :cljs js/Error) _))))))
;[:map
; [:user map?] 
; [:profile map?] 
; [:nested :map]]
```

In the example, `:tags` key was removed as it's contents would have been an empty `:vector`, which is not legal Schema syntax. Empty `:map` is ok.

## Trimming strings

Example how to trim all `:string` values using a custom transformer:

```clj
(require '[malli.transform :as mt])
(require '[malli.core :as m])
(require '[clojure.string :as str])

;; a decoding transformer, only mounting to :string schemas with truthy :string/trim property
(defn string-trimmer []
  (mt/transformer
    {:decoders
     {:string
      {:compile (fn [schema _]
                  (let [{:string/keys [trim]} (m/properties schema)]
                    (when trim #(cond-> % (string? %) str/trim))))}}}))

;; no trimming
(m/decode [:string {:min 1}] "    " string-trimmer)
; => "    "

;; trim me please
(m/decode [:string {:string/trim true, :min 1}] "    " string-trimmer)
; => ""

;; without :string/trim is a no-op
(m/decoder :string string-trimmer)
```
