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

Using a custom transformer:

```clj
(defn query-decoder [schema]
  (m/decoder
    schema
    (mt/transformer
      (mt/transformer
        {:name "vectorize strings"
         :decoders
         {:vector
          {:compile (fn [schema _]
                      (let [separator (-> schema m/properties :query/separator (or ","))]
                        (fn [x]
                          (cond
                            (not (string? x)) x
                            (str/includes? x separator) (into [] (.split ^String x separator))
                            :else [x]))))}}})
      (mt/string-transformer))))

(def decode
  (query-decoder
    [:map
     [:a [:vector {:query/separator ";"} :int]]
     [:b [:vector :int]]]))

(decode {:a "1", :b "1"})
; => {:a [1], :b [1]}

(decode {:a "1;2", :b "1,2"})
; => {:a [1 2], :b [1 2]}
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
## Collecting inlined reference definitions from schemas

By default, one can inline schema reference definitions with `:map`, like:

```clj
(def User
  [:map
   [::id :int]
   [:name :string]
   [::country {:optional true} :string]])
```

It would be nice to be able to simplify the schemas into:

```clj
[:map
 ::id
 [:name :string]
 [::country {:optional true}]]
```

Use cases:
* Simplify large schemas
* Finding differences in semantics
* Refactoring multiple schemas to use a shared registry

Naive implementation (doesn't look up the local registries):

```clj
(defn collect-references [schema]
  (let [acc* (atom {})
        ->registry (fn [registry]
                     (->> (for [[k d] registry]
                            (if (seq (rest d))
                              (m/-fail! ::ambiguous-references {:data d})
                              [k (first (keys d))]))
                          (into {})))
        schema (m/walk
                 schema
                 (fn [schema path children _]
                   (let [children (if (= :map (m/type schema)) ;; just maps
                                    (->> children
                                         (mapv (fn [[k p s]]
                                                 ;; we found inlined references
                                                 (if (and (m/-reference? k) (not (m/-reference? s)))
                                                   (do (swap! acc* update-in [k (m/form s)] (fnil conj #{}) (conj path k))
                                                       (if (seq p) [k p] k))
                                                   [k p s]))))
                                    children)
                         ;; accumulated registry, fail on ambiguous refs
                         registry (->registry @acc*)]
                     ;; return simplified schema
                     (m/into-schema
                       (m/-parent schema)
                       (m/-properties schema)
                       children
                       {:registry (mr/composite-registry (m/-registry (m/options schema)) registry)}))))]
    {:registry (->registry @acc*)
     :schema schema}))
```

In action:

```clj
(collect-references User)
;{:registry {:user/id :int,
;            :user/country :string}
; :schema [:map
;          :user/id
;          [:name :string]
;          [:user/country {:optional true}]]}
```

```clj
(collect-references
  [:map
   [:user/id :int]
   [:child [:map
            [:user/id :string]]]])
; =throws=> :user/ambiguous-references {:data {:string #{[:child :user/id]}, :int #{[:user/id]}}}
```
