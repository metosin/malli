# Malli CHANGELOG

We use [Break Versioning][breakver]. The version numbers follow a `<major>.<minor>.<patch>` scheme with the following intent:

| Bump    | Intent                                                     |
| ------- | ---------------------------------------------------------- |
| `major` | Major breaking changes -- check the changelog for details. |
| `minor` | Minor breaking changes -- check the changelog for details. |
| `patch` | No breaking changes, ever!!                                |

`-SNAPSHOT` versions are preview versions for upcoming releases.

[breakver]: https://github.com/ptaoussanis/encore/blob/master/BREAK-VERSIONING.md

Malli is in [alpha](README.md#alpha).

## UNRELEASED

* `:map-of` provider understands `:maybe` and optional keys

## 0.7.2 (2021-12-12)

* FIX Function with Sequential return value cannot define as function schema [#585](https://github.com/metosin/malli/issues/585)

## 0.7.1 (2021-12-11)

* FIX `decimal?` predicate schema was removed in 0.7.0, [#590](https://github.com/metosin/malli/issues/590)

## 0.7.0 (2021-12-07)

### Performance

* big improvements to schema creation, transformation and inferring perfromance, see [#531](https://github.com/metosin/malli/issues/513) and [#550](https://github.com/metosin/malli/pull/550).

#### Schema Creation

```clj
(def ?schema
  [:map
   [:x boolean?]
   [:y {:optional true} int?]
   [:z [:map
        [:x boolean?]
        [:y {:optional true} int?]]]])

(def schema (m/schema ?schema))

;; 44µs -> 2.5µs (18x)
(bench (m/schema ?schema))

;; 44µs -> 240ns (180x, not realized)
(p/bench (m/schema ?schema {::m/lazy-entries true}))
```

#### Schema Transformation

```clj
;; 26µs -> 1.2µs (21x)
(bench (m/walk schema (m/schema-walker identity)))

;; 4.2µs -> 0.54µs (7x)
(bench (mu/assoc schema :w :string))

;; 51µs -> 3.4µs (15x)
(bench (mu/closed-schema schema))

;; 5µs -> 28ns (180x)
(p/bench (m/deref-all ref-schema))

;; 134µs -> 9µs (15x)
(p/bench (mu/merge schema schema))
```

#### Schema Workers

```clj
(def schema (m/schema ?schema))

;; 1.6µs -> 64ns (25x)
(p/bench (m/validate schema {:x true, :z {:x true}}))

;; 1.6µs -> 450ns (3x)
(p/bench (m/explain schema {:x true, :z {:x true}}))
```

#### Schema Inferring

```clj
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

;; 126ms -> 2.5ms (50x)
(p/bench (mp/provide samples))

;; 380µs (330x)
(let [provide (mp/provider)]
  (p/bench (provide samples)))
```

### Schema AST

New optimized map-syntax to super-fast schema creation, see [README](README.md#map-syntax).

```clj
(def ast (m/ast ?schema))
;{:type :map,
; :keys {:x {:order 0, :value {:type boolean?}},
;        :y {:order 1, :value {:type int?}
;            :properties {:optional true}},
;        :z {:order 2,
;            :value {:type :map,
;                    :keys {:x {:order 0
;                               :value {:type boolean?}},
;                           :y {:order 1 
;                               :value {:type int?}
;                               :properties {:optional true}}}}}}}

;; 150ns (16x)
(p/bench (m/from-ast ast))

(-> ?schema
    (m/schema)
    (m/ast)
    (m/from-ast)
    (m/form)
    (= ?schema))
; => true
```

Currently in alpha, will fully replace the old map-syntax at some point.

### Swappable default registry

No need to play with Compiler options or JVM properties to swap the default registry (only if you want to get DCE on CLJS with small set of schemas). Can be disabled with new `malli.registry/mode=strict` option.

```clj
(require '[malli.core :as m]
         '[malli.util :as mu]
         '[malli.registry :as mr]
         '[malli.generator :as mg])

;; look ma, just works
(mr/set-default-registry!
  (mr/composite-registry
    (m/default-schemas)
    (mu/schemas)))

(mg/generate
  [:merge
   [:map [:x :int]]
   [:map [:y :int]]])
; => {:x 0, :y 92}
```

### Public API

* **BREAKING**: `m/explain` `:errors` are plain maps, not `Error` records.
* **BREAKING**: `malli.provider/schema` is moved into extender API: `malli.provider/-schema`
* **BREAKING**: strings generate alphanumeric chars by default
* `malli.provider` supports inferring of `:maybe` and `:map-of`
* configure default registry in less invasive manner, [#488](https://github.com/metosin/malli/issues/488)
* `nil` is a valid default with `mt/default-value-transformer` [#576](https://github.com/metosin/malli/issues/576)
* fixed `:schema` explain path, [#573](https://github.com/metosin/malli/issues/573)
* fixed `:enum` explain path, [#553](https://github.com/metosin/malli/issues/553)
* fixed pretty printing of function values, [#509](https://github.com/metosin/malli/pull/509)
* fixed `:function` lenses
* fixed arity error in `m/function-schema`
* add localized error messages for all type-schemas
* support for Lazy EntrySchema parsing
* `empty?` Schema does not throw exceptions

### Extender API

* **BREAKING**: `m/EntrySchema` replaces `m/MapSchema` with new `-entry-parser` method
* **BREAKING**: (eager) `m/-parse-entries` is removed, use `m/-entry-parser` instead
* **BREAKING**: `m/-create-form` supports 2 & 4 arities (was: 3)
* `m/EntryParser` protocol
* `m/-entry-forms` helper
* `m/walk-leaf`, `m/-walk-entries` & `m/-walk-indexed` helpers
* `m/Cached` protocol and `m/-create-cache` for memoization of `-validator`, `-explainer`, `-parser` and `-unparser` when using `m/validator`, `m/explain`, `m/parser` and `m/unparser`.

## 0.6.1 (2021-08-08)

* add missing optional dependency to `mvxcvi/arrangement` to make pretty printing work

## 0.6.0 (2021-08-08)

* Much faster validators on CLJ (loop unrolling & programming against interfaces) with `:or`, `:and`, `:orn` and `:map`, thanks to [Ben Sless](https://github.com/bsless):

```clj
;; 164ns -> 28ns
(let [valid? (m/validator [:and [:> 0] [:> 1] [:> 2] [:> 3] [:> 4]])]
  (cc/quick-bench (valid? 5)))
  
;; 150ns -> 30ns
(let [valid? (m/validator [:map [:a :any] [:b :any] [:c :any] [:d :any] [:e :any]])
      value {:a 1, :b 2, :c 3, :d 4, :e 5}]
  (cc/quick-bench (valid? value)))
```

* Much faster collection transformers on CLJ (loop unrolling & programming against interfaces):

```clj
(let [decode (m/decoder
               [:map
                [:id :string]
                [:type :keyword]
                [:address
                 [:map
                  [:street :string]
                  [:lonlat [:tuple :double :double]]]]]
               (mt/json-transformer))
      json {:id "pulla"
            :type "food"
            :address {:street "hämeenkatu 14"
                      :lonlat [61 23.7644223]}}]
                      
  ;; 920ns => 160ns
  (cc/quick-bench
    (decode json)))
```

### Public API

* **BREAKING**: `malli.json-schema/unlift-keys` is removed in favor of `malli.core/-unlift-keys`
* **BREAKING**: `malli.json-schema/unlift` is removed in favor of `get`
* **BREAKING**: `malli.provider/stats` is removed (was already deprecated)
* **BREAKING**: `malli.util/update` doesn't the properties of the key it updates, fixes [#412](https://github.com/metosin/malli/issues/412)
* **BREAKING**: New rules for humanized errors, see [#502](https://github.com/metosin/malli/pull/502), fixes [#80](https://github.com/metosin/malli/issues/80), [#428](https://github.com/metosin/malli/issues/428) and [#499](https://github.com/metosin/malli/issues/499).

* new `malli.instrument` and `malli.dev` for instrumenting function Vars (e.g. `defn`s), see [the guide](docs/function-schemas.md).
* new `malli.plantuml` namespace for [PlantUML generation](README.md#plantuml)
* new `malli.generator/check` for generative testing of functions and `defn`s.
* new `malli.core/parent`
* `:map-of` supports `:min` and `:max` properties
* Collection Schemas emit correct JSON Schema min & max declarations
* humanized errors for `:boolean` & `:malli.core/tuple-limit`
* predicate schema for `fn?`
* `malli.util/transform-entries` passes in options [#340]/(https://github.com/metosin/malli/pull/340)  
* BETA: humanized errors can be read from parent schemas (also from map entries), fixes [#86](https://github.com/metosin/malli/issues/86):

```clj
(-> [:map
     [:foo {:error/message "entry-failure"} :int]]
    (m/explain {:foo "1"})
    (me/humanize {:resolve me/-resolve-root-error}))
; => {:foo ["entry-failure"]}
```

* New experimental pretty printer for schema errors, using [fipp](https://github.com/brandonbloom/fipp).

<img src="https://github.com/metosin/malli/blob/master/docs/img/defn-schema.png">

### Extender API

* `malli.util.impl/-fail!` is now `malli.core/-fail!`
* `malli.core/-unlift-keys`
* `malli.core/-instrument`
* **BREAKING**: `malli.core/-register-function-schema!` is now 4-arity, new argument is data map
* **BREAKING**: `malli.core/-fail!` has only arity 1 & 2 versions

## 0.5.1 (2021-05-02)

### Public API

* Fix [#435](https://github.com/metosin/malli/issues/435): :re ignores :gen/xxx properties in absence of :gen/gen
* More customization on -collection-schema [#433](https://github.com/metosin/malli/pull/433)

## 0.5.0 (2021-04-28)

### Public API

* Add `ifn?` predicate, [#416](https://github.com/metosin/malli/pull/416)
* Accumulate errors correctly with `m/-explain` with `:function` and `:=>` Schemas
* New `m/properties-schema` and `m/children-schema` to resolve Malli Schemas for `IntoSchema`s. Empty implementations.
* New `:gen/schema` property for declarative generation, e.g. `[:string {:gen/schema :int, :gen/fmap str}]`
* Support double generator options via schema properties
* Fix [#419](https://github.com/metosin/malli/issues/419): Parsing bug in :map schema
* Fix [#418](https://github.com/metosin/malli/issues/418): Better error messages / docs for registry references
* Fix [#415](https://github.com/metosin/malli/issues/415): Default branch in multi schema are not transformed
* Fix [#427](https://github.com/metosin/malli/issues/427): Generated sets of :ref are always empty

### Extender API

* **BREAKING**: `-type` is moved from `Schema` to `IntoSchema`.
* **BREAKING**: `-type-properties` is moved from `Schema` to `IntoSchema`.
* new Protocol methods in `IntoSchema` Protocol
  * `(-properties-schema [this options] "maybe returns :map schema describing schema properties")`
  * `(-children-schema [this options] "maybe returns sequence schema describing schema children")`

## 0.4.0 (2021-03-31)

### Public API

* `:nil` schema, [#401](https://github.com/metosin/malli/pull/401)
* **BREAKING/FIX**: parsing `:multi` returns branch information, [#403](https://github.com/metosin/malli/pull/403)
* `:and` merges using first child, [#405](https://github.com/metosin/malli/pull/405)

## 0.3.1 (2021-03-21)

### Public API

* Add `:orn` json-schema & generator, [#400](https://github.com/metosin/malli/pull/400)
* Ignore optional properties in `mt/default-value-transformer`, [#397](https://github.com/metosin/malli/pull/397)
* Support `nil` keys in maps, [#392](https://github.com/metosin/malli/pull/392)
* `:m/default` for `:multi`, [#391](https://github.com/metosin/malli/pull/391)
* Fix inconsistent park-ing in alt(n)-parser, [#390](https://github.com/metosin/malli/pull/390)
* Fix json schema generation when all attributes of a map are optional, [#385](https://github.com/metosin/malli/pull/385)
* Note about transformers best-effort behavior, [#384](https://github.com/metosin/malli/pull/384)
* Humanized regex/sequence errors, [#383](https://github.com/metosin/malli/pull/383)
* Humanized error for `:double`, [#382](https://github.com/metosin/malli/pull/382)

## 0.3.0 (2021-03-02)

### Public API

* support for sequence schemas: `:cat`, `catn`, `alt`, `altn`, `:?`, `:*`, `:+` and `repeat`, see [Sequence Schemas](./README.md#sequence-schemas).
* support for parsing and unparsing schemas: `m/parse`, `m/parser`, `m/unparse`, `m/unparser`, see [Parsing values](./README.md#parsing-values).
* support for function schmas: `:=>` and `:function`, see [Function Schemas](./README.md#function-schemas).
* support for [clj-kondo](https://github.com/clj-kondo/clj-kondo), see [Clj-kondo](./README.md#clj-kondo).
* new schemas: `:any` (e.g. `any?`), `:not` (complement) and `:orn` (or with named branches)
* `:qualified-keyword` support `:namespace` property

* FIX: Schema vizualization is not working for `[:< ...]` like schemas, [#370](https://github.com/metosin/malli/issues/370)
* Ensure we use size 30 for generator (for more variety), [#364](https://github.com/metosin/malli/pull/364)
* Set JSON Schema types and formats for numbers properly [#354](https://github.com/metosin/malli/pull/354)
* -memoize actually memoized. easily 100x faster now [#350](https://github.com/metosin/malli/pull/350)  
* Fix interceptor composition, [#347](https://github.com/metosin/malli/pull/350)
* `malli.util`: add a rename-keys utility, similar to clojure.set [#338](https://github.com/metosin/malli/pull/338)
* Let `mu/update` accept plain data schemas, [#329](https://github.com/metosin/malli/pull/329)
* `mu/find`, [#322](https://github.com/metosin/malli/pull/322)

### Extender API

* **BREAKING**: `m/Schema` has new methods: `-parent`, `-parser` and `-unparser`
* **BREAKING**: `m/-coder` and `m/-chain` are replaced wih `m/-intercepting`
* **BREAKING**: `m/-fail!` is now `miu/-fail!`
* **BREAKING**: `m/-error` is now `miu/-error`
  
## 0.2.1 (2020-10-22)

* fix `:sequential` decoding with empty sequence under `mt/json-transformer`, fixes [#288](https://github.com/metosin/malli/issues/288)
  * removed broken `mt/-sequential->seq`

## 0.2.0 (2020-10-18)

* **BREAKING (MINOR)**: `m/deref` returns original schema, does not throw, fixes [#284](https://github.com/metosin/malli/issues/284).
* **BREAKING (MINOR)**: the following utilities in `malli.util` deref top-level refs recursively: `merge`, `union`, `transform-entries`, `optional-keys`, `required-keys`, `select-keys` and `dissoc`.
* `m/deref-all` derefs all top-level references recursively, e.g.

```clj
(m/deref-all [:schema [:schema int?]])
; => int?
```

* `:ref`, `:schema`, `::m/schema` have now generators, JSON Schema and Swagger support
* `mu/subschemas` walks over top-level `:ref` and all `:schema`s.
* `m/walk` can walk over `:ref` and `:schema` reference schemas. Walking can be enabled using options `:malli.core/walk-refs` and `:malli.core/walk-schema-refs`.
* Welcome declarative schema transformations!

There are also declarative versions of schema transforming utilities in `malli.util/schemas`. These include `:merge`, `:union` and `:select-keys`:

```clj
(def registry (merge (m/default-schemas) (mu/schemas)))

(def Merged
  (m/schema
    [:merge
     [:map [:x :string]]
     [:map [:y :int]]]
    {:registry registry}))

Merged
;[:merge
; [:map [:x :string]]
; [:map [:y :int]]]

(m/deref Merged)
;[:map 
; [:x :string] 
; [:y :int]]

(m/validate Merged {:x "kikka", :y 6})
; => true
```

* New options for SCI:
  * `:malli.core/disable-sci` for explicitly disabling `sci`, fixes [#276](https://github.com/metosin/malli/issues/276)
  * `:malli.core/sci-options` for configuring `sci`

* `malli.transform/default-value-transformer` accepts options `:key` and `:defaults`:

```clj
(m/decode
  [:map
   [:user [:map
           [:name :string]
           [:description {:ui/default "-"} :string]]]]
  nil
  (mt/default-value-transformer
    {:key :ui/default
     :defaults {:map (constantly {})
                :string (constantly "")}}))
; => {:user {:name "", :description "-"}}
```
* Support microsecond precision when parsing datetime strings. [#280](https://github.com/metosin/malli/pull/280)

## 0.1.0 (2020-10-08)

First stable release.

## Breaking Changes in pre-alpha:

* 8.10.2020
  * removed `:list` schema
  * removed `malli.error/SchemaError` protocol in favor of using `m/type-properties` for custom errors
* 20.9.2020
  * removed `m/-predicate-schema`, `m/-partial-predicate-schema` and `m/-leaf-schema`
* 19.9.2020
  * new mandatory Protocol method in `m/Schema`: `-type-properties`
* 1.9.2020
  * `m/children` returns 3-tuple (key, properties, schema) for `MapSchema`s
  * `m/map-entries` is removed, `m/entries` returns a `MapEntry` of key & `m/-val-schema`  
* 4.8.2020
  * `:path` in explain is re-implemented: map keys by value, others by child index
  * `m/-walk` and `m/Walker` uses `:path`, not `:in`
  * `m/-outer` has new parameter order: `walker schema path children options`
  * `malli.util/path-schemas` replaced with `malli.util/subschemas` & `malli.util/distict-by`
  * `LensSchema` has a new `-key` method
  * renamed some non-user apis in `malli.core` & `malli.util`
  * moved map-syntax helpers from `malli.core` to `malli.util`
  * dynaload `com.gfredericks/test.chuck`
* 23.7.2020
  * `sci` is not a default dependency. Enabling sci-support:
    * **Clojure**: add a dependency to `borkdude/sci`
    * **ClojureScript**: also require `sci.core` (directly or via `:preloads`)
* 18.7.2020
  * big cleanup of `malli.transform` internals.
* 12.7.2020
  * `malli.mermaid` is removed (in favor of `malli.dot`)  
* 10.7.2020
  * `[metosin/malli "0.0.1-20200710.075225-19"]`
  * Visitor is implemented using a Walker.
    * `m/accept` -> `m/walk`
    * `m/schema-visitor` -> `m/schema-walker`
    * `m/map-syntax-visitor` -> `m/map-syntax-walker`
* 31.6.2020
  * new `-children` method in `Schema`, to return child schemas as instances (instead of just AST)
* 17.6.2020
  * change all `malli.core/*-registry` defs into `malli.core/*-schemas` defns to enable DCE for clojurescript
* 9.6.2020 
  * `malli.core/name` & `malli.core/-name` renamed to `malli.core/type` & `malli.core/-type`
  * `malli.generator/-generator` is renamed to `malli.generator/-schema-generator`
