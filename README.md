# malli [![Build Status](https://img.shields.io/circleci/project/github/metosin/malli.svg)](https://circleci.com/gh/metosin/malli)

Plain data Schemas for Clojure/Script.

**STATUS**: *Pre-alpha*, in design and prototyping phase.

<img src="https://raw.githubusercontent.com/metosin/malli/master/docs/img/malli.png" width=130 align="right"/>

- Schemas as data
- Schema-driven Runtime Validation
- Schema-driven Transformations
- Tools for programming with Schemas
- No global state, explicit everything
- First class error messages
- Fast

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

Optional [Schema-like](https://github.com/plumatic/schema) syntax for `:map` keys:

```clj
(def valid?
  (m/validator
    [:map
     [:x boolean?]
     [[:opt y] int?]
     [[:req :z] string?]]))

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
; :problems ({:path [2 1 1], :in [:tags 0], :schema keyword?, :value "coffee"}
;            {:path [3 1],
;             :in [:address],
;             :schema [:map
;                      [:street string?]
;                      [:city string?]
;                      [:zip int?]
;                      [:lonlat [:tuple double? double?]]],
;             :type :malli.core/missing-key,
;             :malli.core/key :city}
;            {:path [3 1 4 1 2], :in [:address :lonlat 1], :schema double?, :value nil})}
```

Transforming Schemas:

```clj
(require '[malli.transform :as mt])

(m/transform
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

Serializing & Deserializing schemas, no `eval` needed.

```clj
(require '[clojure.edn :as edn])

(-> [:map
     [:id int?]
     [:name string?]
     [:lonlat [:tuple double? double?]]]
    (m/schema)
    (pr-str)
    (edn/read-string)
    (m/schema)
    (m/validate
      {:id 42
       :name "Tampere"
       :lonlat [61.49911 23.78712]}))
; => true
```

Performance:

```clj
(require '[clojure.spec.alpha :as s])
(require '[criterium.core :as cc])

;; 40ns
(let [spec (s/and int? (s/or :pos-int pos-int? :neg-int neg-int?))
      valid? (partial s/valid? spec)]
  (assert (= [true false true] (map valid? [-1 0 1])))
  (cc/quick-bench
    (valid? spec 0)))

;; 5ns
(let [valid? (m/validator [:and int? [:or pos-int? neg-int?]])]
  (assert (= [true false true] (map valid? [-1 0 1])))
  (cc/quick-bench
    (valid? 0)))
```

## Registry

All public functions take optional options map with optional `:registry` key. It is an map of `name->IntoSchema`.  It defaults to `malli.core/default-registry` which is an merge of the following subregistries:

#### `malli.core/predicate-registry`

Contains both function values and unqualified symbol representations for all `clojure.core`/`cljs.core` functions that end with a questionmark, e.g. `int?`, `'int?`, `string?`, `'string?`. Having both enables reading forms from both code (function values) and EDN-files (symbols) 

#### `malli.core/comparator-registry`

Comparator functions as keywords: `:>`, `:>=`, `:<`, `:<=`, `:=` and `:not=`.

#### `malli.core/base-registry`

Contains `:and`, `:or`, `:map`, `:vector`, `:list`, `:set`, `:tuple`, `:enum` and `:maybe`.

### Custom registry

Example to create a custom registry without the default core predicates and with `:string` and `:int` Schemas:

```clj
(def registry
  (merge
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

[clojure.spec](https://clojure.org/guides/spec) introduces a mutable global registry for specs. There is no such thing in `malli`, but you can create it yourself:

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

- Clojure.spec https://clojure.org/guides/spec
- Core.typed https://github.com/clojure/core.typed
- TypeScript https://www.typescriptlang.org/
- Schema https://github.com/plumatic/schema
- Struct https://funcool.github.io/struct/latest/
- JOI https://github.com/hapijs/joi
- JSON Schema https://json-schema.org/understanding-json-schema/index.html

## Running tests

We use Kaocha as a test runner. Before running the tests, you need to install NPM dependencies.

```bash
npm install
bin/kaocha
```
