# Tips

## Accessing both schema and value in transformation

```clojure
(require '[malli.core :as m])
(require '[malli.transform :as mt])

(def Address
  [:map
   [:id :string]
   [:tags [:set :keyword]]
   [:address [:map
              [:street :string]
              [:city :string]]]])

(def lillan
  {:id "Lillan"
   :tags #{:artesan :coffee :hotel}
   :address {:street "Ahlmanintie 29"
             :city "Tampere"}})

(m/decode
 Address
 lillan
 (mt/transformer
  {:default-decoder
   {:compile (fn [schema _]
               (fn [value]
                 (prn [value (m/form schema)])
                 value))}}))
;[{:id "Lillan", :tags #{:coffee :artesan :hotel}, :address {:street "Ahlmanintie 29", :city "Tampere"}} [:map [:id :string] [:tags [:set :keyword]] [:address [:map [:street :string] [:city :string]]]]]
;["Lillan" [:malli.core/val :string]]
;["Lillan" :string]
;[#{:coffee :artesan :hotel} [:malli.core/val [:set :keyword]]]
;[#{:coffee :artesan :hotel} [:set :keyword]]
;[:coffee :keyword]
;[:artesan :keyword]
;[:hotel :keyword]
;[{:street "Ahlmanintie 29", :city "Tampere"} [:malli.core/val [:map [:street :string] [:city :string]]]]
;[{:street "Ahlmanintie 29", :city "Tampere"} [:map [:street :string] [:city :string]]]
;["Ahlmanintie 29" [:malli.core/val :string]]
;["Ahlmanintie 29" :string]
;["Tampere" [:malli.core/val :string]]
;["Tampere" :string]
;; => {:id "Lillan", :tags #{:coffee :artesan :hotel}, :address {:street "Ahlmanintie 29", :city "Tampere"}}
```

## Removing Schemas based on a property

Schemas can be walked over recursively using `m/walk`:

```clojure
(require '[malli.core :as m])

(def Schema
  [:map
   [:user :map]
   [:profile :map]
   [:tags [:vector [:int {:deleteMe true}]]]
   [:nested [:map [:x [:tuple {:deleteMe true} :string :string]]]]
   [:token [:string {:deleteMe true}]]])

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
; [:user :map]
; [:profile :map]
; [:nested :map]]
```

In the example, `:tags` key was removed as it's contents would have been an empty `:vector`, which is not legal Schema syntax. Empty `:map` is ok.

## Trimming strings

Example how to trim all `:string` values using a custom transformer:

```clojure
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
;; => "kikka"

;; no trimming
(m/decode [:string {:min 1}] "    " string-trimmer)
;; => "    "

;; without :string/trim, decoding is a no-op
(m/decoder :string string-trimmer)
; => #object[clojure.core$identity]
```

## Decoding collections

Transforming a comma-separated string into a vector of ints:

```clojure
(require '[malli.core :as m])
(require '[malli.transform :as mt])
(require '[clojure.string :as str])

(m/decode
  [:vector {:decode/string #(str/split % #",")} int?]
  "1,2,3,4"
  (mt/string-transformer))
;; => [1 2 3 4]
```

Using a custom transformer:

```clojure
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
;; => {:a [1], :b [1]}

(decode {:a "1;2", :b "1,2"})
;; => {:a [1 2], :b [1 2]}
```

## Normalizing properties

Returning a Schema form with `nil` in place of empty properties:

```clojure
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
   [:x :int]
   [:y [:tuple :int :int]]
   [:z [:set [:map [:x [:enum 1 2 3]]]]]])
;; => [:map nil
;;     [:x nil :int]
;;     [:y nil [:tuple nil :int :int]]
;;     [:z nil [:set nil
;;              [:map nil
;;               [:x nil [:enum nil 1 2 3]]]]]]
```

## Default value from a function

The `mt/default-value-transformer` can fill default values if the `:default` property is given. It
is possible though to calculate a default value with a given function providing custom transformer
derived from `mt/default-value-transformer`:

```clojure
(defn default-fn-value-transformer
  ([]
   (default-fn-value-transformer nil))
  ([{:keys [key] :or {key :default-fn}}]
   (let [add-defaults
         {:compile
          (fn [schema _]
            (let [->k-default (fn [[k {default key :keys [optional]} v]]
                                (when-not optional
                                  (when-some [default (or default (some-> v m/properties key))]
                                    [k default])))
                  defaults    (into {} (keep ->k-default) (m/children schema))
                  exercise    (fn [x defaults]
                                (reduce-kv (fn [acc k v]
                                             ; the key difference compare to default-value-transformer
                                             ; we evaluate v instead of just passing it
                                             (if-not (contains? x k)
                                               (-> (assoc acc k ((m/eval v) x))
                                                   (try (catch Exception _ acc)))
                                               acc))
                                           x defaults))]
              (when (seq defaults)
                (fn [x] (if (map? x) (exercise x defaults) x)))))}]
     (mt/transformer
      {:decoders {:map add-defaults}
       :encoders {:map add-defaults}}))))
```

Example 1: if `:secondary` is missing, same its value to value of `:primary`
```clojure
(m/decode
 [:map
  [:primary :string]
  [:secondary {:default-fn '#(:primary %)} :string]]
 {:primary "blue"}
 (default-fn-value-transformer))
```

Example 2: if `:cost` is missing, try to calculate it from `:price` and `:qty`:
```clojure
(def Purchase
  [:map
   [:qty {:default 1} number?]
   [:price {:optional true} number?]
   [:cost {:default-fn '(fn [m] (* (:qty m) (:price m)))} number?]])

(def decode-autonomous-vals
  (m/decoder Purchase (mt/transformer (mt/string-transformer) (mt/default-value-transformer))))
(def decode-interconnected-vals
  (m/decoder Purchase (default-fn-value-transformer)))

(-> {:qty "100" :price "1.2"} decode-autonomous-vals decode-interconnected-vals)
;; => {:price 1.2, :qty 100.0, :cost 120.0}
(-> {:price "1.2"} decode-autonomous-vals decode-interconnected-vals)
;; => {:qty 1, :price 1.2, :cost 1.2}
(-> {:prie "1.2"} decode-autonomous-vals decode-interconnected-vals)
;; => {:prie "1.2", :qty 1}
```

## Walking Schema and Entry Properties

1. walk entries on the way in
2. unwalk entries on the way out

```clojure
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

```clojure
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

```clojure
(require '[malli.util :as mu])

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
   [:a :string]
   [:b {:optional true} :int]
   [:c [:maybe
        [:map
         [:d :string]
         [:e {:optional true} :int]]]]])
;[:map
; [:a :string]
; [:b {:optional true} :any]
; [:c [:maybe [:map
;              [:d :string]
;              [:e {:optional true} :any]]]]]

(m/validate
  [:map
   [:a :string]
   [:b {:optional true} :int]]
  {:a "Hey" :b "Nope"})
;; => false

(m/validate
  (allow-invalid-optional-values
    [:map
     [:a :string]
     [:b {:optional true} :int]])
  {:a "Hey" :b "Nope"})
;; => true
```
## Collecting inlined reference definitions from schemas

By default, one can inline schema reference definitions with `:map`, like:

```clojure
(def User
  [:map
   [::id :int]
   [:name :string]
   [::country {:optional true} :string]])
```

It would be nice to be able to simplify the schemas into:

```clojure
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

```clojure
(require '[malli.registry :as mr])

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

```clojure
(collect-references User)
;{:registry {:user/id :int,
;            :user/country :string}
; :schema [:map
;          :user/id
;          [:name :string]
;          [:user/country {:optional true}]]}
```
<!-- :test-doc-blocks/skip -->
```clojure
(collect-references
  [:map
   [:user/id :int]
   [:child [:map
            [:user/id :string]]]])
; =throws=> :user/ambiguous-references {:data {:string #{[:child :user/id]}, :int #{[:user/id]}}}
```

## Getting error-values into humanized result

```clojure
(require '[malli.error :as me])
(-> [:map
     [:x :int]
     [:y [:set :keyword]]
     [:z [:map
          [:a [:tuple :int :int]]]]]
    (m/explain {:x "1"
                :y #{:a "b" :c}
                :z {:a [1 "2"]}})
    (me/humanize {:wrap #(select-keys % [:value :message])}))
;; => {:x [{:value "1"
;      :message "should be an integer"}],
; :y #{[{:value "b"
;        :message "should be a keyword"}]},
; :z {:a [nil [{:value "2"
;               :message "should be an integer"}]]}}
```

## Dependent String Schemas

A schema for a string made of two components `a` and `b` separated by a `/` where the schema of `b`
depends on the value of `a`. The valid values of a are known in advance.

For instance:
* When `a` is "ip" , `b` should be a valid ip
* When `a` is "domain", `b` should be a valid domain

Here are a few examples of valid and invalid data:
* `"ip/127.0.0.1"` is valid
* `"ip/111"` is not valid
* `"domain/cnn.com"` is valid
* `"domain/aa"` is not valid
* `"kika/aaa"` is not valid

```clojure
(def domain #"[a-zA-Z0-9][-a-zA-Z0-9]{0,62}(\.[a-zA-Z0-9][-a-zA-Z0-9]{0,62})+")

(def ipv4 #"^(?:[0-9]{1,3}\.){3}[0-9]{1,3}$")

;; a multi schema describing the values as a tuple
;; includes transformation guide to and from a string domain
(def schema [:multi {:dispatch first
                     :decode/string #(str/split % #"/")
                     :encode/string #(str/join "/" %)}
             ["domain" [:tuple [:= "domain"] domain]]
             ["ip" [:tuple [:= "ip"] ipv4]]])

;; define workers
(def validate (m/validator schema))
(def decode (m/decoder schema mt/string-transformer))
(def encode (m/encoder schema mt/string-transformer))

(decode "ip/127.0.0.1")
;; => ["ip" "127.0.0.1"]

(-> "ip/127.0.0.1" (decode) (encode))
;; => "ip/127.0.0.1"

(map (comp validate decode)
     ["ip/127.0.0.1"
      "ip/111"
      "domain/cnn.com"
      "domain/aa"
      "kika/aaa"])
;; => (true false true false false)
```

It is also possible to use a custom transformer instead of `string-transformer` (for example, in order to avoid `string-transformer` to perform additional encoding and decoding):

```clojure
(def schema [:multi {:dispatch first
                     :decode/my-custom #(str/split % #"/")
                     :encode/my-custom #(str/join "/" %)}
             ["domain" [:tuple [:= "domain"] domain]]
             ["ip" [:tuple [:= "ip"] ipv4]]])

(def decode (m/decoder schema (mt/transformer {:name :my-custom})))

(decode "ip/127.0.0.1")
;; => ["ip" "127.0.0.1"]
```

## Converting Schemas

Example utility to convert schemas recursively:

```clojure
(defn schema-mapper [m]
  (fn [s] ((or (get m (m/type s)) ;; type mapping
               (get m ::default)  ;; default mapping
               (constantly s))    ;; nop
           s)))

(m/walk
 [:map
  [:id :keyword]
  [:size :int]
  [:tags [:set :keyword]]
  [:sub
   [:map
    [:kw :keyword]
    [:data [:tuple :keyword :int :keyword]]]]]
 (m/schema-walker
  (schema-mapper
    {:keyword (constantly :string)                            ;; :keyword -> :string
     :int #(m/-set-properties % {:gen/elements [1 2]})        ;; custom :int generator
     ::default #(m/-set-properties % {::type (m/type %)})}))) ;; for others
;[:map {::type :map}
; [:id :string]
; [:size [:int {:gen/elements [1 2 3]}]]
; [:tags [:set {::type :set} :string]]
; [:sub [:map {::type :map}
;        [:kw :string]
;        [:data [:tuple {::type :tuple}
;                :string
;                [:int {:gen/elements [1 2 3]}]
;                :string]]]]]
```
