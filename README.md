# malli [![Build Status](https://img.shields.io/circleci/project/github/metosin/malli.svg)](https://circleci.com/gh/metosin/malli) [![Slack](https://img.shields.io/badge/clojurians-malli-blue.svg?logo=slack)](https://clojurians.slack.com/messages/malli/)

Plain data Schemas for Clojure/Script.

**STATUS**: *Pre-alpha*, in design and prototyping phase.

<img src="https://raw.githubusercontent.com/metosin/malli/master/docs/img/malli.png" width=130 align="right"/>

- Schemas as data
- Schema-driven Validation
- Schema-driven Transformation
- Schema-driven Value Generation
- Infer Schemas from sample values
- Tools for programming with Schemas
- No global state, explicit everything
- First class error messages
- Fast

Try the [online demo](https://malli.io). Libraries using malli:

- [Aave](https://github.com/teknql/aave), a code checking tool for Clojure.

## Examples

Definining and validating Schemas:

```clj
(require '[malli.core :as m])

(m/validate int? "1")
; => false

(m/validate int? 1)
; => true

(m/validate [:and int? [:> 6]] 7)
; => true

(def valid?
  (m/validator
    [:map
     [:x boolean?]
     [:y {:optional true} int?]
     [:z string?]]))

(valid? {:x true, :z "kikka"})
; => true
```

Schemas can have properties:

```clj
(def Age
  [:and
   {:title "Age"
    :description "It's an age"
    :json-schema/example 20}
   int? [:> 18]])
   
(m/properties Age)
; => {:title "Age"
;     :description "It's an age"
;     :json-schema/example 20}   
```

Serializable function schemas using [sci](https://github.com/borkdude/sci):

```clj
(def my-schema
  [:and
   [:map
    [:x int?]
    [:y int?]]
   [:fn '(fn [{:keys [x y]}] (> x y))]])

(m/validate my-schema {:x 1, :y 0})
; => true

(m/validate my-schema {:x 1, :y 2})
; => false
```

## Detailed Errors

Detailed errors with `m/explain`:

```clj
(def Address
  [:map
   [:id string?]
   [:tags [:set keyword?]]
   [:address
    [:map
     [:street string?]
     [:city string?]
     [:zip int?]
     [:lonlat [:tuple double? double?]]]]])

(m/explain
  Address
  {:id "Lillan"
   :tags #{:artesan :coffee :hotel}
   :address {:street "Ahlmanintie 29"
             :city "Tampere"
             :zip 33100
             :lonlat [61.4858322, 23.7854658]}})
; => nil

(m/explain
  Address
  {:id "Lillan"
   :tags #{:artesan "coffee" :garden}
   :address {:street "Ahlmanintie 29"
             :zip 33100
             :lonlat [61.4858322, nil]}})
;{:schema [:map
;          [:id string?]
;          [:tags [:set keyword?]]
;          [:address [:map
;                     [:street string?]
;                     [:city string?]
;                     [:zip int?]
;                     [:lonlat [:tuple double? double?]]]]],
; :value {:id "Lillan",
;         :tags #{:artesan :garden "coffee"},
;         :address {:street "Ahlmanintie 29"
;                   :zip 33100
;                   :lonlat [61.4858322 nil]}},
; :errors (#Error{:path [2 1 1], :in [:tags 0], :schema keyword?, :value "coffee"}
;          #Error{:path [3 1],
;                 :in [:address],
;                 :schema [:map
;                          [:street string?]
;                          [:city string?]
;                          [:zip int?]
;                          [:lonlat [:tuple double? double?]]],
;                 :type :malli.core/missing-key,
;                 :malli.core/key :city}
;          #Error{:path [3 1 4 1 2], :in [:address :lonlat 1], :schema double?, :value nil})}
```

## Custom Error Messages

Explain results can be humanized with `malli.error/humanize`:

```clj
(require '[malli.error :as me])

(-> Address
    (m/explain
      {:id "Lillan"
       :tags #{:artesan "coffee" :garden}
       :address {:street "Ahlmanintie 29"
                 :zip 33100
                 :lonlat [61.4858322, nil]}})
    (me/humanize
      {:wrap :message}))
;{:tags #{"should be keyword"}
; :address {:city "missing required key"
;           :lonlat [nil "should be double"]}}
```

Error messages can be customized with `:error/message` and `:error/fn` properties:

```clj
(-> [:map
     [:id int?]
     [:size [:enum {:error/message "should be: S|M|L"} 
             "S" "M" "L"]]
     [:age [:fn {:error/fn '(fn [{:keys [value]} _] (str value ", should be > 18"))}
            '(fn [x] (and (int? x) (> x 18)))]]]
    (m/explain {:size "XL", :age 10})
    (me/humanize
      {:wrap :message
       :errors (-> me/default-errors
                   (assoc ::m/missing-key {:error/fn (fn [{:keys [in]} _] (str "missing key " (last in)))}))}))
;{:id "missing key :id"
; :size "should be: S|M|L"
; :age "10, should be > 18"}
```

Messages can be localized:

```clj
(-> [:map
     [:id int?]
     [:size [:enum {:error/message {:en "should be: S|M|L"
                                    :fi "pitäisi olla: S|M|L"}}
             "S" "M" "L"]]
     [:age [:fn {:error/fn {:en '(fn [{:keys [value]} _] (str value ", should be > 18"))
                            :fi '(fn [{:keys [value]} _] (str value ", pitäisi olla > 18"))}}
            '(fn [x] (and (int? x) (> x 18)))]]]
    (m/explain {:size "XL", :age 10})
    (me/humanize
      {:locale :fi
       :wrap :message
       :errors (-> me/default-errors
                   (assoc-in ['int? :error-message :fi] "pitäisi olla numero")
                   (assoc ::m/missing-key {:error/fn {:en '(fn [{:keys [in]} _] (str "missing key " (last in)))
                                                      :fi '(fn [{:keys [in]} _] (str "puuttuu avain " (last in)))}}))}))
;{:id "puuttuu avain :id"
; :size "pitäisi olla: S|M|L"
; :age "10, pitäisi olla > 18"}
```

Top-level humanized map-errors are under `:malli/error`:

```clj
(-> [:and [:map
           [:password string?]
           [:password2 string?]]
     [:fn {:error/message "passwords don't match"}
      '(fn [{:keys [password password2]}]
         (= password password2))]]
    (m/explain {:password "secret"
                :password2 "faarao"})
    (me/humanize {:wrap :message}))
; {:malli/error "passwords don't match"}
```

Errors can be targetted using `:error/path` property:

```clj
(-> [:and [:map
           [:password string?]
           [:password2 string?]]
     [:fn {:error/message "passwords don't match"
           :error/path [:password2]}
      '(fn [{:keys [password password2]}]
         (= password password2))]]
    (m/explain {:password "secret"
                :password2 "faarao"})
    (me/humanize {:wrap :message}))
; {:password2 "passwords don't match"}
```

## Value Transformation

```clj
(require '[malli.transform :as mt])
```

Two-way schema-driven value transformations with `m/decode` and `m/encode` using a `m/Transformer`. Default Transformers include: `string-transformer`, `json-transformer`, `strip-extra-keys-transformer`, `default-value-transformer` and `key-transformer`.

```clj
(m/decode int? "42" mt/string-transformer)
; 42

(m/encode int? 42 mt/string-transformer)
; "42"
```

Transformations are recursive:

```clj
(m/decode
  Address
  {:id "Lillan",
   :tags ["coffee" "artesan" "garden"],
   :address {:street "Ahlmanintie 29"
             :city "Tampere"
             :zip 33100
             :lonlat [61.4858322 23.7854658]}}
  mt/json-transformer)
;{:id "Lillan",
; :tags #{:coffee :artesan :garden},
; :address {:street "Ahlmanintie 29"
;           :city "Tampere"
;           :zip 33100
;           :lonlat [61.4858322 23.7854658]}}
```

Transform map keys:

```clj
(m/decode
  Address
  {:id "Lillan",
   :tags ["coffee" "artesan" "garden"],
   :address {:street "Ahlmanintie 29"
             :city "Tampere"
             :zip 33100
             :lonlat [61.4858322 23.7854658]}}
  (mt/key-transformer name))
;{"id" "Lillan",
; "tags" #{:coffee :artesan :garden},
; "address" {"street" "Ahlmanintie 29"
;            "city" "Tampere"
;            "zip" 33100
;            "lonlat" [61.4858322 23.7854658]}}
```

Transformers can be composed with `mt/transformer`:

```clj
(def strict-json-transformer
  (mt/transformer
    mt/strip-extra-keys-transformer
    mt/json-transformer)

(m/decode
  Address
  {:id "Lillan",
   :EVIL "LYN"
   :tags ["coffee" "artesan" "garden"],
   :address {:street "Ahlmanintie 29"
             :DARK "ORKO"
             :city "Tampere"
             :zip 33100
             :lonlat [61.4858322 23.7854658]}}
  strict-json-transformer)
;{:id "Lillan",
; :tags #{:coffee :artesan :garden},
; :address {:street "Ahlmanintie 29"
;           :city "Tampere"
;           :zip 33100
;           :lonlat [61.4858322 23.7854658]}}
```

Schema properties can be used to override default transformations:

```clj
(m/decode
  [string? {:decode/string 'str/upper-case}]
  "kerran" mt/string-transformer)
; => "KERRAN"
```

Decoders and encoders as interceptors (with `:enter` and `:leave` stages):

```clj
(m/decode
  [string? {:decode/string {:enter 'str/upper-case}}]
  "kerran" mt/string-transformer)
; => "KERRAN"
```

```clj
(m/decode
  [string? {:decode/string {:enter '#(str "olipa_" %)
                            :leave '#(str % "_avaruus")}}]
  "kerran" mt/string-transformer)
; => "olipa_kerran_avaruus"
```

To access Schema (and options) use `:compile`:

```clj
(m/decode
  [int? {:math/multiplier 10
         :decode/math {:compile '(fn [schema _]
                                  (let [multiplier (:math/multiplier (m/properties schema))]
                                    (fn [x] (* x multiplier))))}}]
  12
  (mt/transformer {:name :math}))
; => 120
```

Going crazy:

```clj
(m/decode
  [:map
   {:decode/math {:enter '#(update % :x inc)
                  :leave '#(update % :x (partial * 2))}}
   [:x [int? {:decode/math {:enter '(partial + 2)
                            :leave '(partial * 3)}}]]]
  {:x 1} 
  (mt/transformer {:name :math}))
; => {:x 42}
```

Applying default values:

```clj
(m/encode
  [:map {:default {}}
   [:a [int? {:default 1}]]
   [:b [:vector {:default [1 2 3]} int?]]
   [:c [:map {:default {}}
        [:x [int? {:default 42}]]
        [:y int?]]]
   [:d [:map
        [:x [int? {:default 42}]]
        [:y int?]]]
   [:e int?]]
  nil
  (mt/transformer
    mt/default-value-transformer
    mt/string-transformer))
;{:a "1"
; :b ["1" "2" "3"]
; :c {:x "42"}}
```

## Merging Schemas

Schemas can be deep-merged with `m/merge`:

```clj
(m/merge
  Address
  [:map
   [:description string?]
   [:address
    [:map
     [:country string?]]]])
;[:map
; [:id string?]
; [:tags [:set keyword?]]
; [:address 
;  [:map 
;   [:street string?] 
;   [:city string?] 
;   [:zip int?] 
;   [:lonlat [:tuple double? double?]] 
;   [:country string?]]]
; [:description string?]]
```

## Persisting Schemas 

Writing and Reading schemas as [EDN](https://github.com/edn-format/edn), no `eval` needed.

```clj
(require '[malli.edn :as edn])

(-> [:and
     [:map
      [:x int?]
      [:y int?]]
     [:fn '(fn [{:keys [x y]}] (> x y))]]
    (edn/write-string)
    (doto prn) ; => "[:and [:map [:x int?] [:y int?]] [:fn (fn [{:keys [x y]}] (> x y))]]"
    (edn/read-string)
    (doto (-> (m/validate {:x 0, :y 1}) prn)) ; => false
    (doto (-> (m/validate {:x 2, :y 1}) prn))) ; => true
;[:and 
; [:map 
;  [:x int?] 
;  [:y int?]] 
; [:fn (fn [{:keys [x y]}] (> x y))]]
```

## Multi Schemas

Closed dispatch with `:multi` schema and `:dispatch` property:

```clj
(m/validate
  [:multi {:dispatch :type}
   [:sized [:map [:type keyword?] [:size int?]]]
   [:human [:map [:type keyword?] [:name string?] [:address [:map [:country keyword?]]]]]]
  {:type :sized, :size 10})
; true
```

Any (serializable) function can be used for `:dispatch`:

```clj
(m/validate
  [:multi {:dispatch 'first}
   [:sized [:tuple keyword? [:map [:size int?]]]]
   [:human [:tuple keyword? [:map [:name string?] [:address [:map [:country keyword?]]]]]]]
  [:human {:name "seppo", :address {:country :sweden}}])
; true
```

`:dispatch` values should be decoded before actual values:

```clj
(m/decode
  [:multi {:dispatch :type
           :decode/string '#(update % :type keyword)}
   [:sized [:map [:type [:= :sized] [:size int?]]]
   [:human [:map [:type [:= :human]] [:name string?] [:address [:map [:country keyword?]]]]]]
  {:type "human"
   :name "Tiina"
   :age "98"
   :address {:country "finland"
             :street "this is an extra key"}}
  (mt/transformer mt/strip-extra-keys-transformer mt/string-transformer))
;{:type :human
; :name "Tiina"
; :address {:country :finland}}
```

## Value Generation

Schemas can be used to generate values:

```clj
(require '[malli.generator :as mg])

;; random
(mg/generate keyword?)
; => :?

;; using seed
(mg/generate [:enum "a" "b" "c"] {:seed 42})
;; => "a"

;; using seed and size
(mg/generate pos-int? {:seed 10, :size 100})
;; => 55740

;; regexs work too
(mg/generate 
  [:re #"^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,63}$"] 
  {:seed 42, :size 10})
; => "CaR@MavCk70OHiX.yZ"

;; gen/elements (note, are not validated)
(mg/generate
  [:and {:gen/elements ["kikka" "kukka" "kakka"]} string?]
  {:seed 10})
; => "kikka"

;; portable gen/fmap
(mg/generate
  [:and {:gen/fmap '(partial str "kikka_")} string?]
  {:seed 10, :size 10})
;; => "kikka_WT3K0yax2"

(require '[clojure.test.check.generators :as gen])

;; gen/gen (note, not serializable)
(mg/generate
  [:sequential {:gen/gen (gen/list gen/neg-int)} int?]
  {:size 42, :seed 42})
; => (-37 -13 -13 -24 -20 -11 -34 -40 -22 0 -10)
```

Generated values are valid:

```clj
(mg/generate Address {:seed 123, :size 4})
;{:id "H7",
; :tags #{:v?.w.t6!.QJYk-/-?s*4
;         :_7U
;         :QdG/Xi8J
;         :*Q-.p*8*/n-J9u}
; :address {:street "V9s"
;           :city ""
;           :zip 3
;           :lonlat [-2.75 -0.625]}}

(m/validate Address (mg/generate Address))
; => true
```

Sampling values:

```clj
;; sampling
(mg/sample [:and int? [:> 10] [:< 100]] {:seed 123})
; => (25 39 51 13 53 43 57 15 26 27)
```

## Inferring Schemas from sample data

Inspired by [F# Type providers](https://docs.microsoft.com/en-us/dotnet/fsharp/tutorials/type-providers/):


```clj
(require '[malli.provider :as mp])

(def samples
  [{:id "Lillan"
    :tags #{:artesan :coffee :hotel}
    :address {:street "Ahlmanintie 29"
              :city "Tampere"
              :zip 33100
              :lonlat [61.4858322, 23.7854658]}}
   {:id "Huber",
    :description "Beefy place"
    :tags #{:beef :wine :beer}
    :address {:street "Aleksis Kiven katu 13"
              :city "Tampere"
              :zip 33200
              :lonlat [61.4963599 23.7604916]}}])

(mp/provide samples)
;[:map
; [:id string?]
; [:tags [:set keyword?]]
; [:address
;  [:map
;   [:street string?]
;   [:city string?]
;   [:zip number?]
;   [:lonlat [:vector double?]]]]
; [:description {:optional true} string?]]
```

All samples are valid against the inferred schema:

```clj
(every? (partial m/validate (mp/provide samples)) samples)
; => true
```

## Schema Transformation

Schemas can be transformed using the [Visitor Pattern](https://en.wikipedia.org/wiki/Visitor_pattern):

```clj
(defn visitor [schema children _]
  {:name (m/name schema)
   :properties (or (m/properties schema) {})
   :children children})

(m/accept Address visitor)
;{:name :map,
; :properties {},
; :children [{:name string?
;             :properties {}
;             :children []}
;            {:name :set
;             :properties {}
;             :children [{:name keyword?, :properties {}, :children []}]}
;            {:name :map,
;             :properties {},
;             :children [{:name string?, :properties {}, :children []}
;                        {:name string?, :properties {}, :children []}
;                        {:name int?, :properties {}, :children []}
;                        {:name :tuple,
;                         :properties {},
;                         :children [{:name double?, :properties {}, :children []}
;                                    {:name double?, :properties {}, :children []}]}]}]}
```

### JSON Schema

Transforming Schemas into [JSON Schema](https://json-schema.org/):

```clj
(require '[malli.json-schema :as json-schema])

(json-schema/transform Address)
;{:type "object",
; :properties {:id {:type "string"},
;              :tags {:type "array"
;                     :items {:type "string"}
;                     :uniqueItems true},
;              :address {:type "object",
;                        :properties {:street {:type "string"},
;                                     :city {:type "string"},
;                                     :zip {:type "integer", :format "int64"},
;                                     :lonlat {:type "array",
;                                              :items [{:type "number"} {:type "number"}],
;                                              :additionalItems false}},
;                        :required [:street :city :zip :lonlat]}},
; :required [:id :tags :address]}
```

Custom transformation via properties:

```clj
(json-schema/transform
  [:enum
   {:title "Fish"
    :description "It's a fish"
    :json-schema/type "string"
    :json-schema/default "perch"}
   "perch" "pike"])
;{:title "Fish"
; :description "It's a fish"
; :type "string"
; :default "perch"
; :enum ["perch" "pike"]}
```

### Swagger2 Schema

Transforming Schemas into [Swagger2 Schema](https://github.com/OAI/OpenAPI-Specification/blob/master/versions/2.0.md):

```clj
(require '[malli.swagger :as swagger])

(swagger/transform Address)
;{:type "object",
; :properties {:id {:type "string"},
;              :tags {:type "array"
;                     :items {:type "string"}
;                     :uniqueItems true},
;              :address {:type "object",
;                        :properties {:street {:type "string"},
;                                     :city {:type "string"},
;                                     :zip {:type "integer", :format "int64"},
;                                     :lonlat {:type "array",
;                                              :items {},
;                                              :x-items [{:type "number", :format "double"}
;                                                        {:type "number", :format "double"}]}},
;                        :required [:street :city :zip :lonlat]}},
; :required [:id :tags :address]}
```

## Performance

Validation:

```clj
(require '[clojure.spec.alpha :as s])
(require '[criterium.core :as cc])

;; 40ns
(let [spec (s/and int? (s/or :pos-int pos-int? :neg-int neg-int?))
      valid? (partial s/valid? spec)]
  (cc/quick-bench
    (valid? spec 0)))

;; 5ns
(let [valid? (m/validator [:and int? [:or pos-int? neg-int?]])]
  (cc/quick-bench
    (valid? 0)))
```

Coercion:

```clj
(require '[spec-tools.core :as st])

(s/def ::id int?)
(s/def ::name string?)

;; 14µs
(let [spec (s/keys :req-un [::id ::name])
      transform #(st/coerce spec % st/string-transformer)]
  (cc/quick-bench
    (transform {:id "1", :name "kikka"})))

;; 140ns
(let [schema [:map [:id int?] [:name string?]]
      transform (m/decoder schema transform/string-transformer)]
  (cc/quick-bench
    (transform {:id "1", :name "kikka"})))
```

## Registry

All public functions take optional options map with optional `:registry` key. It is an map of `name->IntoSchema`.  It defaults to `malli.core/default-registry` which is an merge of the following subregistries:

#### `malli.core/predicate-registry`

Contains both function values and unqualified symbol representations for all relevant core predicates. Having both representations enables reading forms from both code (function values) and EDN-files (symbols): `any?`, `some?`, `number?`, `integer?`, `int?`, `pos-int?`, `neg-int?`, `nat-int?`, `float?`, `double?`, `boolean?`, `string?`, `ident?`, `simple-ident?`, `qualified-ident?`, `keyword?`, `simple-keyword?`, `qualified-keyword?`, `symbol?`, `simple-symbol?`, `qualified-symbol?`, `uuid?`, `uri?`, `decimal?`, `inst?`, `seqable?`, `indexed?`, `map?`, `vector?`, `list?`, `seq?`, `char?`, `set?`, `nil?`, `false?`, `true?`, `zero?`, `rational?`, `coll?`, `empty?`, `associative?`, `sequential?`, `ratio?` and `bytes?`.

#### `malli.core/class-registry`

Class-based schemas, contains `java.util.regex.Pattern` & `js/RegExp`.

#### `malli.core/comparator-registry`

Comparator functions as keywords: `:>`, `:>=`, `:<`, `:<=`, `:=` and `:not=`.

#### `malli.core/base-registry`

Contains `:and`, `:or`, `:map`, `:map-of`, `:vector`, `:list`, `:sequential`, `:set`, `:tuple`, `:enum`, `:maybe`, `:multi`, `:re` and `:fn`.

### Custom registry

Example to create a custom registry without the default core predicates and with `:string` and `:int` Schemas:

```clj
(def registry
  (merge
    m/class-registry
    m/comparator-registry
    m/base-registry
    {:int (m/fn-schema :int int?)
     :string (m/fn-schema :string string?)}))

(m/validate [:or :int :string] 'kikka {:registry registry})
; => false

(m/validate [:or :int :string] 123 {:registry registry})
; => true
```

Predicate Schemas don't work anymore:

```clj
(m/validate int? 123 {:registry registry})
; Syntax error (ExceptionInfo) compiling
; :malli.core/invalid-schema
```

### Mutable registry

[clojure.spec](https://clojure.org/guides/spec) introduces a mutable global registry for specs. There is no such thing in `malli`, but you can create it yourself.

Using a custom registry atom:

```clj
(defonce my-registry
  (atom m/default-registry))

(defn register! [k schema]
  (swap! my-registry assoc k (m/schema schema))
  k)

(register! ::id int?)
;; => :user/id

(register! ::name string?)
;; => :user/name

(m/validate 
  [:tuple ::id ::name] 
  [18 "and life"] 
  {:registry @my-registry})
; => true
```

Mutating the default registry (not recommended):

```clj
(defn evil-register! [k ?schema]
  (alter-var-root
    #'m/default-registry
    (constantly
      (assoc m/default-registry k (m/schema ?schema))))
  k)

(evil-register! ::int int?)
; :user/int

(m/validate ::int 1)
; => true
```

## Entities and Values

Schemas as just data, so they can be either inlined (values) or referenced (entities) in other schemas. For validation, they work the same way, but for model documentation, they are kept as separate.

### Value Schemas

Schemas can be represented as abstract schema syntax and referenced as values:

```clj
(def Age
  [:and int? [:> 18]])

(def User
  [:map
   [:name string?]
   [:age Age]])

(m/validate 
  User 
  {:name "Mirjami", :age 62})
; => true
```

**NOTE**: Schema format validation only occurs when a `m/schema` is called, so here `Age` and `User` could contain syntax errors. 

### Entity Schemas

Wrapping schemas into `m/schema` makes them first class entities. Here `User` is an entity, while `Age` is a (embedded) value.

```clj
(def Age
  [:and int? [:> 18]])

(def User
  (m/schema
    [:map
     [:name string?]
     [:age Age]]))

(m/validate 
  User 
  {:name "Mirjami", :age 62})
; => true
```

## Motivation

We are building dynamic multi-tenant systems where data-models should be first-class: they should drive the runtime value transformations, forms and processes. We should be able to edit the models at runtime, persist them and load them back from database and over the wire, for both Clojure and ClojureScript. Think of [JSON Schema](https://json-schema.org/), but for Clojure/Script.

Hasn't the problem been solved (many times) already?

There is [Schema](https://github.com/plumatic/schema), which is awesome, proven and collaborative open source project, and we absolutely love it. We still use it in most of our projects. Sad part: serializing & de-serializing schemas is non-trivial and there is no back-tracking on branching.

[Spec](https://clojure.org/guides/spec) is the de facto data specification library for Clojure. It has many great ideas, but it is based on macros, it has a global registry and it doesn't support runtime transformations. [Spec-tools](https://github.com/metosin/spec-tools) was created to "fix" some of the things, but after [three years](https://github.com/metosin/spec-tools/commit/18aeb78db7886c985b2881fd87fde6039128b3fb) of developing it, it's still kinda hack and not fun to maintain.

So, we decided to spin out our own library, which would do all the things we feel is important for dynamic system development. It's based on the best parts of the existing libraries and several project-specific tools we have done over the years.

> If you have expectations (of others) that aren't being met, those expectations are your own responsibility. You are responsible for your own needs. If you want things, make them.

- Rich Hickey, [Open Source is Not About You](https://gist.github.com/richhickey/1563cddea1002958f96e7ba9519972d9)

## Links (and thanks)

- Schema https://github.com/plumatic/schema
- Clojure.spec https://clojure.org/guides/spec
- Core.typed https://github.com/clojure/core.typed
- TypeScript https://www.typescriptlang.org/
- Struct https://funcool.github.io/struct/latest/
- JOI https://github.com/hapijs/joi
- JSON Schema https://json-schema.org/understanding-json-schema/index.html
- Spec-provider: https://github.com/stathissideris/spec-provider
- F# Type Providers: https://docs.microsoft.com/en-us/dotnet/fsharp/tutorials/type-providers/

## Running tests

We use Kaocha as a test runner. Before running the tests, you need to install NPM dependencies.

```bash
npm install
bin/kaocha
```
