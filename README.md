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
     [[:opt :y] int?]
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

Serializing & Deserializing schemas:

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

Contains `:and`, `:or`, `:map`, `:vector`, `:list`, `:set` and `:tuple`.

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

Wrapping schemas into `m/schema` makes them first class. Here `User` is an entity, while `Age` is a (embedded) value.

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

## More

Should have:
- Context dependent pluggable validators
- Context dependent pluggable coercers
- Close compatibility to Json Schema 7

Could have:
- Clojure spec generation
- Database schema generations

## Links

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
