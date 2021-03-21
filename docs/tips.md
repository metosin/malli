# Tips

## Removing Schemas based on a property

Schemas can be walked over recursively using `m/walk`:

```clj
(require '[malli.core :as m])

(def Schema
  [:map
   [:user map?]
   [:profile map?]
   [:tags [:vector [int? {:deleteMe true}]]]
   [:nested [:map [:x [:tuple {:deleteMe true} string? string?]]]]
   [:token [string? {:deleteMe true}]]])

(m/walk
  Schema
  (fn [schema _ children options]
    ;; return nil if Schema has the property 
    (when-not (:deleteMe (m/properties schema))
      ;; there are two syntaxes: normal and the entry, handle separately
      (let [children (if (m/entries schema) (filterv last children) children)]
        ;; create a new Schema with the updated children, or return nil
        (try (m/into-schema (m/type schema) (m/properties schema) children options)
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

;; trim me please
(m/decode [:string {:string/trim true, :min 1}] " kikka  " string-trimmer)
; => "kikka"

;; no trimming
(m/decode [:string {:min 1}] "    " string-trimmer)
; => "    "

;; without :string/trim, decoding is a no-op
(m/decoder :string string-trimmer)
; => #object[clojure.core$identity]
```

## Decoding collections

Transforming a comma-separated string into a vector of ints:

```clj
(require '[malli.core :as m])
(require '[malli.transform :as mt])
(require '[clojure.string :as str])

(m/decode 
  [:vector {:decode/string #(str/split % #",")} int?] 
  "1,2,3,4" 
  (mt/string-transformer))
; => [1 2 3 4]
```

## Normalizing properties

Returning a Schema form with `nil` in place of empty properties:

```clj
(require '[malli.core :as m])

(defn normalize-properties [?schema]
  (m/walk
    ?schema
    (fn [schema _ children _]
      (if (vector? (m/form schema))
        (into [(m/type schema) (m/properties schema)] children)
        (m/form schema)))))

(normalize-properties
  [:map
   [:x int?]
   [:y [:tuple int? int?]]
   [:z [:set [:map [:x [:enum 1 2 3]]]]]])
;[:map nil
; [:x nil int?]
; [:y nil [:tuple nil int? int?]]
; [:z nil [:set nil
;          [:map nil
;           [:x nil [:enum nil 1 2 3]]]]]]
```

## Walking Schema and Entry Properties

1. walk entries on the way in
2. unwalk entries on the way out

```clj
(defn walk-properties [schema f]
  (m/walk
    schema
    (fn [s _ c _]
      (m/into-schema
        (m/-parent s)
        (f (m/-properties s))
        (cond->> c (m/entries s) (map (fn [[k p s]] [k (f p) (first (m/children s))])))
        (m/options s)))
    {::m/walk-entry-vals true}))
```

Stripping all swagger-keys:

```clj
(defn remove-swagger-keys [p]
  (not-empty
    (reduce-kv
      (fn [acc k _]
        (cond-> acc (some #{:swagger} [k (-> k namespace keyword)]) (dissoc k)))
      p p)))

(walk-properties
  [:map {:title "Organisation name"}
   [:ref {:swagger/description "Reference to the organisation"
          :swagger/example "Acme floor polish, Houston TX"} :string]
   [:kikka [:string {:swagger {:title "kukka"}}]]]
  remove-swagger-keys)
;[:map {:title "Organisation name"}
; [:ref :string]
; [:kikka :string]]
```

## Allowing invalid values on optional keys

e.g. don't fail if the optional keys hava invalid values.

1. create a helper function that transforms the schema swapping the actual schema with `:any`
2. done.

```clj
(defn allow-invalid-optional-values [schema]
  (m/walk
    schema
    (m/schema-walker
      (fn [s]
        (cond-> s
                (m/entries s)
                (mu/transform-entries
                  (partial map (fn [[k {:keys [optional] :as p} s]] [k p (if optional :any s)]))))))))

(allow-invalid-optional-values
  [:map
   [:a string?]
   [:b {:optional true} int?]
   [:c [:maybe
        [:map
         [:d string?]
         [:e {:optional true} int?]]]]])
;[:map
; [:a string?]
; [:b {:optional true} :any]
; [:c [:maybe [:map
;              [:d string?]
;              [:e {:optional true} :any]]]]]

(m/validate
  [:map
   [:a string?]
   [:b {:optional true} int?]]
  {:a "Hey" :b "Nope"})
; => false

(m/validate
  (allow-invalid-optional-values
    [:map
     [:a string?]
     [:b {:optional true} int?]])
  {:a "Hey" :b "Nope"})
; => true
```
