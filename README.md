# malli 

[![Build Status](https://img.shields.io/github/workflow/status/metosin/malli/Run%20tests.svg)](https://github.com/metosin/malli/actions)
[![cljdoc badge](https://cljdoc.org/badge/metosin/malli)](https://cljdoc.org/d/metosin/malli/)
[![Clojars Project](https://img.shields.io/clojars/v/metosin/malli.svg)](https://clojars.org/metosin/malli)
[![Slack](https://img.shields.io/badge/clojurians-malli-blue.svg?logo=slack)](https://clojurians.slack.com/messages/malli/)

Data-driven Schemas for Clojure/Script.

**STATUS**: [*alpha*](#alpha)

<img src="https://raw.githubusercontent.com/metosin/malli/master/docs/img/malli.png" width=130 align="right"/>

- Schema definitions as data
- [Validation](#examples) and [Value Transformation](#value-transformation)
- First class [Error Messages](#error-messages) with [Spell Checking](#spell-checking)
- [Serializable function schemas](#serializable-functions)
- [Generating values](#value-generation) from Schemas
- [Inferring Schemas](#inferring-schemas) from sample values
- Tools for [Programming with Schemas](#programming-with-schemas)
- [Parsing](#parsing-values), [Unparsing](#unparsing-values) and [Sequence Schemas](#sequence-schemas)
- [Persisting schemas](#persisting-schemas) and the alternative [Map-syntax](#map-syntax)
- Immutable, Mutable, Dynamic, Lazy and Local [Schema Registries](#schema-registry)
- [Schema Transformations](#schema-Transformation) to [JSON Schema](#json-schema) and [Swagger2](#swagger2)
- [Multi-schemas](#multi-schemas), [Recursive Schemas](#recursive-schemas) and [Default values](#default-values)
- [Function Schemas](#function-schemas) with [clj-kondo](#clj-kondo) support
- [Visualizing Schemas](#visualizing-schemas) with DOT
- [Fast](#performance)

Presentations:

- [ClojureScript Podcast: Malli wtih Tommi Reiman](https://soundcloud.com/user-959992602/s4-e30-malli-wtih-tommi-reiman)
- [Structure and Interpretation of Malli Regex Schemas](https://www.metosin.fi/blog/malli-regex-schemas/)
- LNDCLJ 9.12.2020: [Designing with Malli](https://youtu.be/bQDkuF6-py4), slides [here](https://www.slideshare.net/mobile/metosin/designing-with-malli)
- [Malli, Data-Driven Schemas for Clojure/Script](https://www.metosin.fi/blog/malli/)
- CEST 2.6.2020: [Data-driven Rapid Application Development with Malli](https://www.youtube.com/watch?v=ww9yR_rbgQs)
- ClojureD 2020: [Malli: Inside Data-driven Schemas](https://www.youtube.com/watch?v=MR83MhWQ61E), slides [here](https://www.slideshare.net/metosin/malli-inside-datadriven-schemas)

Try the [online demo](https://malli.io). Libraries using or supporting malli:

- [Aave](https://github.com/teknql/aave), a code checking tool for Clojure.
- [Gungnir](https://github.com/kwrooijen/gungnir), a high level, data driven database library for Clojure data mapping.
- [Regal](https://github.com/lambdaisland/regal), Royally reified regular expressions
- [Reitit](https://github.com/metosin/reitit), a fast data-driven router for Clojure/Script.
- [wasm.cljc](https://github.com/helins/wasm.cljc) - Spec compliant WebAssembly compiler and decompiler
- [malli-instrument](https://github.com/setzer22/malli-instrument) - Instrumentation for malli mimicking the clojure.spec.alpha API
- [Snoop](https://github.com/CrypticButter/snoop) - Function instrumentation using Malli schemas.
 
## Examples

Defining and validating Schemas:

```clj
(require '[malli.core :as m])

(m/validate int? "1")
; => false

(m/validate int? 1)
; => true

(m/validate [:and int? [:> 6]] 7)
; => true

(m/validate [:qualified-keyword {:namespace :aaa}] :aaa/bbb)
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

Maps are open by default:

```clj
(m/validate
  [:map [:x int?]]
  {:x 1, :extra "key"})
; => true
```

Maps can be closed with `:closed` property:

```clj
(m/validate
  [:map {:closed true} [:x int?]]
  {:x 1, :extra "key"})
; => false
``` 

Maps keys are not limited to keywords:

```clj
(m/validate
  [:map
   ["status" [:enum "ok"]]
   [1 any?]
   [nil any?]
   [::a string?]]
  {"status" "ok"
   1 'number
   nil :yay
   ::a "properly awesome"})
; => true
```

## Qualified keys in a map

You can also use [decomplected maps keys and values](https://clojure.org/about/spec#_decomplect_mapskeysvalues) using registry references. References must be either qualified keywords of strings.

```clj
(m/validate
  [:map {:registry {::id int?
                    ::country string?}}
   ::id
   [:name string?]
   [::country {:optional true}]]
  {::id 1
   :name "kikka"})
; => true
```

## Homogeneous Maps

Other times, we use a map as a homogeneous index. In this case, all our key-value
pairs have the same type. For this use case, we can use the `:map-of` schema.

```clj
(m/validate 
  [:map-of :string [:map [:lat number?] [:long number?]]]
  {"oslo" {:lat 60 :long 11}
   "helsinki" {:lat 60 :long 24}})
;; => true
```

## Sequence Schemas

You can use `:sequential` for any homogeneous Clojure sequence, `:vector` for vectors and `:set` for sets.

```clj
(m/validate [:sequential any?] (list "this" 'is :number 42))
;; => true

(m/validate [:vector int?] [1 2 3])
;; => true

(m/validate [:vector int?] (list 1 2 3))
;; => false
```

Malli also supports sequence regexes like [Seqexp](https://github.com/cgrand/seqexp) and Spec.
The supported operators are `:cat` & `:catn` for concatenation / sequencing

```clj
(m/validate [:cat string? int?] ["foo" 0]) ; => true

(m/validate [:catn [:s string?] [:n int?]] ["foo" 0]) ; => true
```

`:alt` & `:altn` for alternatives

```clj
(m/validate [:alt keyword? string?] ["foo"]) ; => true

(m/validate [:altn [:kw keyword?] [:s string?]] ["foo"]) ; => true
```

and `:?`, `:*`, `:+` & `:repeat` for repetition:

```clj
(m/validate [:? int?] []) ; => true
(m/validate [:? int?] [1]) ; => true
(m/validate [:? int?] [1 2]) ; => false

(m/validate [:* int?] []) ; => true
(m/validate [:* int?] [1 2 3]) ; => true

(m/validate [:+ int?] []) ; => false
(m/validate [:+ int?] [1]) ; => true
(m/validate [:+ int?] [1 2 3]) ; => true

(m/validate [:repeat {:min 2, :max 4} int?] [1]) ; => false
(m/validate [:repeat {:min 2, :max 4} int?] [1 2]) ; => true
(m/validate [:repeat {:min 2, :max 4} int?] [1 2 3 4]) ; => true (:max is inclusive, as elsewhere in Malli)
(m/validate [:repeat {:min 2, :max 4} int?] [1 2 3 4 5]) ; => false
```

`:catn` and `:altn` allow naming the subsequences / alternatives

```clj
(m/explain
  [:* [:catn [:prop string?] [:val [:altn [:s string?] [:b boolean?]]]]]
  ["-server" "foo" "-verbose" 11 "-user" "joe"])
;; => {:schema [:* [:map [:prop string?] [:val [:map [:s string?] [:b boolean?]]]]],
;;     :value ["-server" "foo" "-verbose" 11 "-user" "joe"],
;;     :errors (#Error{:path [0 :val :s], :in [3], :schema string?, :value 11}
;;              #Error{:path [0 :val :b], :in [3], :schema boolean?, :value 11})}
```

while `:cat` and `:alt` just use numeric indices for paths:

```clj
(m/explain 
  [:* [:cat string? [:alt string? boolean?]]]
  ["-server" "foo" "-verbose" 11 "-user" "joe"])
;; => {:schema [:* [:cat string? [:alt string? boolean?]]],
;;     :value ["-server" "foo" "-verbose" 11 "-user" "joe"],
;;     :errors (#Error{:path [0 1 0], :in [3], :schema string?, :value 11}
;;              #Error{:path [0 1 1], :in [3], :schema boolean?, :value 11})}
```

As all these examples show, the "seqex" operators take any non-seqex child schema to
mean a sequence of one element that matches that schema. To force that behaviour for
a seqex child `:schema` can be used:

```clj
(m/validate
  [:cat [:= :names] [:schema [:* string?]] [:= :nums] [:schema [:* number?]]]
  [:names ["a" "b"] :nums [1 2 3]])
; => true

;; whereas
(m/validate
  [:cat [:= :names] [:* string?] [:= :nums] [:* number?]]
  [:names "a" "b" :nums 1 2 3]) 
; => true
```

Although a lot of effort has gone into making the seqex implementation fast

```clj
(require '[clojure.spec.alpha :as s])
(require '[criterium.core :as cc])

(let [valid? (partial s/valid? (s/* int?))]
  (cc/quick-bench (valid? (range 10)))) ; Execution time mean : 27µs

(let [valid? (m/validator [:* int?])]
  (cc/quick-bench (valid? (range 10)))) ; Execution time mean : 2.7µs
```

it is always better to use less general tools whenever possible:

```clj
(let [valid? (partial s/valid? (s/coll-of int?))]
  (cc/quick-bench (valid? (range 10)))) ; Execution time mean : 1.8µs

(let [valid? (m/validator [:sequential int?])]
  (cc/quick-bench (valid? (range 10)))) ; Execution time mean : 0.12µs
```

## String schemas

Using a predicate:

```clj
(m/validate string? "kikka")
```

Using `:string` Schema:

```clj
(m/validate :string "kikka")
;; => true

(m/validate [:string {:min 1, :max 4}] "")
;; => false
```

Using regular expressions:

```clj

(m/validate #"a+b+c+" "abbccc")
;; => true

;; :re with string
(m/validate [:re ".{3,5}"] "abc")
;; => true

;; :re with regex
(m/validate [:re #".{3,5}"] "abc")
;; => true

;; NB: re-find semantics
(m/validate [:re #"\d{4}"] "1234567")
;; => true

;; anchor with ^...$ if you want to strictly match the whole string
(m/validate [:re #"^\d{4}$"] "1234567")
;; => false

```

## Fn schemas

`:fn` allows any predicate function to be used:
 
```clj
(def my-schema
  [:and
   [:map
    [:x int?]
    [:y int?]]
   [:fn (fn [{:keys [x y]}] (> x y))]])
   
(m/validate my-schema {:x 1, :y 0})
; => true

(m/validate my-schema {:x 1, :y 2})
; => false
```

`ifn?` accepts any value that implements Clojure(Script)'s IFn:

```clj
(m/validate ifn? :keyword)
; => true

(m/validate ifn? [])
; => true

(m/validate ifn? {})
; => true

(s/validate ifn? 123)
; => false
```

## Serializable Functions

Enabling serializable function schemas requires [sci](https://github.com/borkdude/sci) as external dependency. If
it is not present, the malli function evaluator throws `:sci-not-available` exception.

For ClojureScript, you also need to require `sci.core` manually, either directly or via [`:preloads`](https://clojurescript.org/reference/compiler-options#preloads).

For GraalVM, you need to require `sci.core` manually, before requiring any malli namespaces.

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

**NOTE**: [sci is not termination safe](https://github.com/borkdude/sci/issues/348) so be carefull `sci` functions from untrusted sources. You can explictely disable with option `::m/disable-sci` and set the default options with `::m/sci-options`.

```clj
(m/validate [:fn 'int?] 1 {::m/disable-sci true})
; Execution error
; :malli.core/sci-not-available {:code int?}
```

## Error Messages

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
; :errors (#Error{:path [:tags 0]
;                 :in [:tags 0]
;                 :schema keyword?
;                 :value "coffee"}
;          #Error{:path [:address :city],
;                 :in [:address :city],
;                 :schema [:map
;                          [:street string?]
;                          [:city string?]
;                          [:zip int?]
;                          [:lonlat [:tuple double? double?]]],
;                 :type :malli.core/missing-key}
;          #Error{:path [:address :lonlat 1]
;                 :in [:address :lonlat 1]
;                 :schema double?
;                 :value nil})}
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
    (me/humanize))
;{:tags #{["should be a keyword"]}
; :address {:city ["missing required key"]
;           :lonlat [nil ["should be a double"]]}}
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
      {:errors (-> me/default-errors
                   (assoc ::m/missing-key {:error/fn (fn [{:keys [in]} _] (str "missing key " (last in)))}))}))
;{:id ["missing key :id"]
; :size ["should be: S|M|L"]
; :age ["10, should be > 18"]}
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
       :errors (-> me/default-errors
                   (assoc-in ['int? :error-message :fi] "pitäisi olla numero")
                   (assoc ::m/missing-key {:error/fn {:en '(fn [{:keys [in]} _] (str "missing key " (last in)))
                                                      :fi '(fn [{:keys [in]} _] (str "puuttuu avain " (last in)))}}))}))
;{:id ["puuttuu avain :id"]
; :size ["pitäisi olla: S|M|L"]
; :age ["10, pitäisi olla > 18"]}
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
    (me/humanize))
; {:malli/error ["passwords don't match"]}
```

Errors can be targeted using `:error/path` property:

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
    (me/humanize))
; {:password2 ["passwords don't match"]}
```

By default, only direct erronous schema properties are used:

```clj
(-> [:map
     [:foo {:error/message "entry-failure"} :int]] ;; here, :int fails, no error props
    (m/explain {:foo "1"})
    (me/humanize))
; => {:foo ["should be an integer"]}
```

Looking up humanized errors from parent schemas with custom `:resolve`:

```clj
(-> [:map
     [:foo {:error/message "entry-failure"} :int]]
    (m/explain {:foo "1"})
    (me/humanize {:resolve me/resolve-root-error}))
; => {:foo ["entry-failure"]}
```

## Spell Checking

For closed schemas, key spelling can be checked with:

```clj
(-> [:map [:address [:map [:street string?]]]]
    (mu/closed-schema)
    (m/explain
      {:name "Lie-mi"
       :address {:streetz "Hämeenkatu 14"}})
    (me/with-spell-checking)
    (me/humanize))
;{:address {:street ["missing required key"]
;           :streetz ["should be spelled :street"]}
; :name ["disallowed key"]}
```

## Value Transformation

```clj
(require '[malli.transform :as mt])
```

Two-way schema-driven value transformations with `m/decode` and `m/encode` using a `m/Transformer`. 

Default Transformers include: `string-transformer`, `json-transformer`, `strip-extra-keys-transformer`, `default-value-transformer` and `key-transformer`.

**NOTE**: the included transformers are best-effort, i.e. they won't throw on bad input, they will just pass the input value through unchanged. You should make sure your schema validation catches these non-transformed values. Custom transformers should follow the same idiom.

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
(m/encode
  Address
  {:id "Lillan",
   :tags ["coffee" "artesan" "garden"],
   :address {:street "Ahlmanintie 29"
             :city "Tampere"
             :zip 33100
             :lonlat [61.4858322 23.7854658]}}
  (mt/key-transformer {:encode name}))
;{"id" "Lillan",
; "tags" ["coffee" "artesan" "garden"],
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
    mt/json-transformer))

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
; => {:x 24}
```

## Default values

Applying default values:

```clj
(m/decode [:and {:default 42} int?] nil mt/default-value-transformer)
; => 42
```

With custom key and type defaults:

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

Single sweep of defaults & string encoding:

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

## Programming with Schemas

```clj
(require '[malli.util :as mu])
```

Updating Schema properties:

```clj
(mu/update-properties [:vector int?] assoc :min 1)
; => [:vector {:min 1} int?]
```

Lifted `clojure.core` function to work with schemas: `select-keys`, `dissoc`, `get`, `assoc`, `update`, `get-in`, `assoc-in`, `update-in`

```clj
(mu/get-in Address [:address :lonlat])
; => [:tuple double? double?]

(mu/update-in Address [:address] mu/assoc :country [:enum "fi" "po"])
;[:map
; [:id string?]
; [:tags [:set keyword?]]
; [:address
;  [:map [:street string?]
;   [:city string?]
;   [:zip int?]
;   [:lonlat [:tuple double? double?]]
;   [:country [:enum "fi" "po"]]]]]

(-> Address
    (mu/dissoc :address)
    (mu/update-properties assoc :title "Address"))
;[:map {:title "Address"} 
; [:id string?] 
; [:tags [:set keyword?]]]
```

Making keys optional or required:

```clj
(mu/optional-keys [:map [:x int?] [:y int?]])
;[:map 
; [:x {:optional true} int?] 
; [:y {:optional true} int?]]

(mu/required-keys [:map [:x {:optional true} int?] [:y int?]])
;[:map 
; [:x int?] 
; [:y int?]]
```

Closing and opening all `:map` schemas recursively:

```clj
(def abcd
  [:map {:title "abcd"}
   [:a int?]
   [:b {:optional true} int?]
   [:c [:map
        [:d int?]]]])

(mu/closed-schema abcd)
;[:map {:title "abcd", :closed true}
; [:a int?]
; [:b {:optional true} int?]
; [:c [:map {:closed true}
;      [:d int?]]]]

(-> abcd 
    mu/closed-schema
    mu/open-schema)
;[:map {:title "abcd"}
; [:a int?]
; [:b {:optional true} int?]
; [:c [:map
;      [:d int?]]]]
```

Merging Schemas (last value wins):

```clj
(mu/merge
  [:map
   [:name string?]
   [:description string?]
   [:address
    [:map
     [:street string?]
     [:country [:enum "finland" "poland"]]]]]
  [:map
   [:description {:optional true} string?]
   [:address
    [:map
     [:country string?]]]])
;[:map
; [:name string?]
; [:description {:optional true} string?]
; [:address [:map
;            [:street string?]
;            [:country string?]]]]
```

With `:and`, first child is used in merge:

```clj
(mu/merge
  [:and {:type "entity"}
   [:map {:title "user"} 
    [:name :string]]
   map?]
  [:map {:description "aged"} [:age :int]])
;[:and {:type "entity"} 
; [:map {:title "user", :description "aged"} 
;  [:name :string] 
;  [:age :int]] 
; map?]
```

Schema unions (merged values of both schemas are valid for union schema):

```clj
(mu/union
  [:map
   [:name string?]
   [:description string?]
   [:address
    [:map
     [:street string?]
     [:country [:enum "finland" "poland"]]]]]
  [:map
   [:description {:optional true} string?]
   [:address
    [:map
     [:country string?]]]])
;[:map
; [:name string?]
; [:description {:optional true} string?]
; [:address [:map
;            [:street string?]
;            [:country [:or [:enum "finland" "poland"] string?]]]]]
```

Adding generated example values to Schemas:

```clj
(m/walk
  [:map
   [:name string?]
   [:description string?]
   [:address
    [:map
     [:street string?]
     [:country [:enum "finland" "poland"]]]]]
  (m/schema-walker
    (fn [schema]
      (mu/update-properties schema assoc :examples (mg/sample schema {:size 2, :seed 20})))))
;[:map
; {:examples ({:name "", :description "", :address {:street "", :country "poland"}}
;             {:name "W", :description "x", :address {:street "8", :country "finland"}})}
; [:name [string? {:examples ("" "")}]]
; [:description [string? {:examples ("" "")}]]
; [:address
;  [:map
;   {:examples ({:street "", :country "finland"} {:street "W", :country "poland"})}
;   [:street [string? {:examples ("" "")}]]
;   [:country [:enum {:examples ("finland" "poland")} "finland" "poland"]]]]]
```

Finding first value (prewalk):

```clj
(mu/find-first
  [:map
   [:x int?]
   [:y [:vector [:tuple
                 [:or [:and {:salaisuus "turvassa"} boolean?] int?]
                 [:schema {:salaisuus "vaarassa"} false?]]]]
   [:z [:string {:salaisuus "piilossa"}]]]
  (fn [schema _ _]
    (-> schema m/properties :salaisuus)))
; => "turvassa"
```

Finding all subschemas with paths, retaining order:

```clj
(def Schema
  (m/schema
    [:maybe
     [:map
      [:id string?]
      [:tags [:set keyword?]]
      [:address
       [:and
        [:map
         [:street {:optional true} string?]
         [:lonlat {:optional true} [:tuple double? double?]]]
        [:fn '(fn [{:keys [street lonlat]}] (or street lonlat))]]]]]))

(mu/subschemas Schema)
;[{:path [], :in [], :schema [:maybe
;                             [:map
;                              [:id string?]
;                              [:tags [:set keyword?]]
;                              [:address
;                               [:and
;                                [:map
;                                 [:street {:optional true} string?]
;                                 [:lonlat {:optional true} [:tuple double? double?]]]
;                                [:fn (fn [{:keys [street lonlat]}] (or street lonlat))]]]]]}
; {:path [0], :in [], :schema [:map
;                              [:id string?]
;                              [:tags [:set keyword?]]
;                              [:address
;                               [:and
;                                [:map
;                                 [:street {:optional true} string?]
;                                 [:lonlat {:optional true} [:tuple double? double?]]]
;                                [:fn (fn [{:keys [street lonlat]}] (or street lonlat))]]]]}
; {:path [0 :id], :in [:id], :schema string?}
; {:path [0 :tags], :in [:tags], :schema [:set keyword?]}
; {:path [0 :tags :malli.core/in], :in [:tags :malli.core/in], :schema keyword?}
; {:path [0 :address], :in [:address], :schema [:and
;                                               [:map
;                                                [:street {:optional true} string?]
;                                                [:lonlat {:optional true} [:tuple double? double?]]]
;                                               [:fn (fn [{:keys [street lonlat]}] (or street lonlat))]]}
; {:path [0 :address 0], :in [:address], :schema [:map
;                                                 [:street {:optional true} string?]
;                                                 [:lonlat {:optional true} [:tuple double? double?]]]}
; {:path [0 :address 0 :street], :in [:address :street], :schema string?}
; {:path [0 :address 0 :lonlat], :in [:address :lonlat], :schema [:tuple double? double?]}
; {:path [0 :address 0 :lonlat 0], :in [:address :lonlat 0], :schema double?}
; {:path [0 :address 0 :lonlat 1], :in [:address :lonlat 1], :schema double?}
; {:path [0 :address 1], :in [:address], :schema [:fn (fn [{:keys [street lonlat]}] (or street lonlat))]}]
```

Collecting unique value paths and their schema paths:

```clj
(->> Schema
     (mu/subschemas)
     (mu/distinct-by :id)
     (mapv (juxt :in :path)))
;[[[] []]
; [[] [0]]
; [[:id] [0 :id]]
; [[:tags] [0 :tags]]
; [[:tags :malli.core/in] [0 :tags :malli.core/in]]
; [[:address] [0 :address]]
; [[:address] [0 :address 0]]
; [[:address :street] [0 :address 0 :street]]
; [[:address :lonlat] [0 :address 0 :lonlat]]
; [[:address :lonlat 0] [0 :address 0 :lonlat 0]]
; [[:address :lonlat 1] [0 :address 0 :lonlat 1]]
; [[:address] [0 :address 1]]]
```

Schema paths can be converted into value paths:

```clj
(mu/get-in Schema [0 :address 0 :lonlat])
; => [:tuple double? double?]

(mu/path->in Schema [0 :address 0 :lonlat])
; => [:address :lonlat]
```

and back, returning all paths:

```clj
(mu/in->paths Schema [:address :lonlat])
; => [[0 :address 0 :lonlat]]
```

## Declarative Schema Transformation

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

Default branch with `::m/default`:

```clj
(def valid?
  (m/validator
    [:multi {:dispatch :type}
     ["object" [:map-of :keyword :string]]
     [::m/default :string]]))

(valid? {:type "object", :key "1", :value "100"})
; => true

(valid? "SUCCESS!")
; => true

(valid? :failure)
; => false
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
   [:sized [:map [:type [:= :sized]] [:size int?]]]
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

## Recursive Schemas

[Local Registry](#local-registry) allows an easy way to create recursive schemas. To be recursive, the schema refers to another schema of the local registry using two useful keywords `:ref` and `:schema`. First, the schema is defined in the local registry, one of the value refer to another schema with the `:ref` keyword. The top-level schema is defined in the second argument of the `:schema` definition:

```clj
(m/validate
  [:schema {:registry {::cons [:maybe [:tuple pos-int? [:ref ::cons]]]}}
   ::cons]
  [16 [64 [26 [1 [13 nil]]]]])
; => true
```

Without the `:ref` keyword, malli would have tried to expand an infinite schema and would have fallen in a stack overflow exception:

```clj
(m/validate
  [:schema {:registry {::cons [:maybe [:tuple pos-int? ::cons]]}}
   ::cons]
  [16 [64 [26 [1 [13 nil]]]]])
; StackOverflowError 
```

Mutual recursion works too. Thanks to the `:schema` construct, many schemas could be defined in the local registry, the top-level one being promoted by the `:schema` second parameter:

```clj
(m/validate
  [:schema {:registry {::ping [:maybe [:tuple [:= "ping"] [:ref ::pong]]]
                       ::pong [:maybe [:tuple [:= "pong"] [:ref ::ping]]]}}
   ::ping]
  ["ping" ["pong" ["ping" ["pong" ["ping" nil]]]]])
; => true
```

Nested registries, last definition wins:

```clj
(m/validate
  [:schema {:registry {::ping [:maybe [:tuple [:= "ping"] [:ref ::pong]]]
                       ::pong any?}} ;; effectively unreachable
   [:schema {:registry {::pong [:maybe [:tuple [:= "pong"] [:ref ::ping]]]}}
    ::ping]]
  ["ping" ["pong" ["ping" ["pong" ["ping" nil]]]]])
; => true
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

;; regexs work too (only clj and if [com.gfredericks/test.chuck "0.2.10"+] available)
(mg/generate 
  [:re #"^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,63}$"] 
  {:seed 42, :size 10})
; => "CaR@MavCk70OHiX.yZ"

;; :gen/elements (note, are not validated)
(mg/generate
  [:and {:gen/elements ["kikka" "kukka" "kakka"]} string?]
  {:seed 10})
; => "kikka"

;; portable :gen/fmap
(mg/generate
  [:and {:gen/fmap '(partial str "kikka_")} string?]
  {:seed 10, :size 10})
;; => "kikka_WT3K0yax2"

;; :gen/schema
(mg/generate
  [:any {:gen/schema [:int {:min 10, :max 20}]}]
  {:seed 10})
; => 19

;; :gen/min & :gen/max for numbers and collections
(mg/generate 
  [:vector {:gen/min 4, :gen/max 4} :int] '
  {:seed 1})
; => [-8522515 -1433 -1 1]

;; :gen/infinite? & :gen/NaN? for :double
(mg/generate 
  [:double {:gen/infinite? true, :gen/NaN? true}] 
  {:seed 1})
; => ##Inf

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

Integration with test.check:

```clj
(require '[clojure.test.check.generators :as gen])
(gen/sample (mg/generator pos-int?))
; => (2 1 2 2 2 2 8 1 55 83)
```

## Inferring Schemas

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

## Parsing values

Schemas can be used to parse values using `m/parse` and `m/parser`:

`m/parse` for one-time things:

```clj
(m/parse
  [:* [:catn
       [:prop string?]
       [:val [:altn
              [:s string?]
              [:b boolean?]]]]]
  ["-server" "foo" "-verbose" true "-user" "joe"])
;[{:prop "-server", :val [:s "foo"]}
; {:prop "-verbose", :val [:b true]}
; {:prop "-user", :val [:s "joe"]}]
```

`m/parser` to create an optimized parser: 

```clj
(def Hiccup
  [:schema {:registry {"hiccup" [:orn
                                 [:node [:catn
                                         [:name keyword?]
                                         [:props [:? [:map-of keyword? any?]]]
                                         [:children [:* [:schema [:ref "hiccup"]]]]]]
                                 [:primitive [:orn
                                              [:nil nil?]
                                              [:boolean boolean?]
                                              [:number number?]
                                              [:text string?]]]]}}
   "hiccup"])

(def parse-hiccup (m/parser Hiccup))

(parse-hiccup
  [:div {:class [:foo :bar]}
   [:p "Hello, world of data"]])
;[:node
; {:name :div
;  :props {:class [:foo :bar]}
;  :children [[:node
;              {:name :p
;               :props nil
;               :children [[:primitive [:text "Hello, world of data"]]]}]]}]
```

Parsing returns tagged values for `:orn`, `:catn`, `:altn` and `:multi`.

```clj
(def Multi
  [:multi {:dispatch :type}
   [:user [:map [:size :int]]]
   [::m/default :any]])

(m/parse Multi {:type :user, :size 1})
; => [:user {:type :user, :size 1}]

(m/parse Multi {:type "sized", :size 1})
; => [:malli.core/default {:type "sized", :size 1}]
```

## Unparsing values

The inverse of parsing, using `m/unparse` and `m/unparser`:

```clj
(->> [:div {:class [:foo :bar]}
      [:p "Hello, world of data"]]
     (m/parse Hiccup)
     (m/unparse Hiccup))
;[:div {:class [:foo :bar]}
; [:p "Hello, world of data"]]
```

## Map-syntax

Schemas can converted into map-syntax (with keys `:type` and optionally `:properties` and `:children`):

```clj
(def Schema
  [:map
   [:id string?]
   [:tags [:set keyword?]]
   [:address
    [:map
     [:street string?]
     [:lonlat [:tuple double? double?]]]]])

(mu/to-map-syntax Schema)
;{:type :map,
; :children [[:id nil {:type string?}]
;            [:tags nil {:type :set
;                        :children [{:type keyword?}]}]
;            [:address nil {:type :map,
;                           :children [[:street nil {:type string?}]
;                                      [:lonlat nil {:type :tuple
;                                                    :children [{:type double?} {:type double?}]}]]}]]}
```

... and back:

```clj
(-> Schema (mu/to-map-syntax) (mu/from-map-syntax) (mu/equals Schema))
; => true
```

## Schema Transformation

Schemas can be transformed using post-walking, e.g. the [Visitor Pattern](https://en.wikipedia.org/wiki/visitor_pattern).

The identity walker:

```clj
(m/walk 
  Address 
  (m/schema-walker identity))
;[:map
; [:id string?]
; [:tags [:set keyword?]]
; [:address
;  [:map
;   [:street string?]
;   [:city string?]
;   [:zip int?]
;   [:lonlat [:tuple double? double?]]]]]
```

Adding `:title` property to schemas:

```clj
(m/walk
  Address
  (m/schema-walker #(mu/update-properties % assoc :title (name (m/type %)))))
;[:map {:title "map"}
; [:id [string? {:title "string?"}]]
; [:tags [:set {:title "set"} [keyword? {:title "keyword?"}]]]
; [:address
;  [:map {:title "map"}
;   [:street [string? {:title "string?"}]]
;   [:city [string? {:title "string?"}]]
;   [:zip [int? {:title "int?"}]]
;   [:lonlat [:tuple {:title "tuple"} [double? {:title "double?"}] [double? {:title "double?"}]]]]]]
```

Transforming schemas into maps:

```clj
(m/walk
  Address
  (fn [schema _ children _]
    (-> (m/properties schema)
        (assoc :malli/type (m/type schema))
        (cond-> (seq children) (assoc :malli/children children)))))
;{:malli/type :map,
; :malli/children [[:id nil {:malli/type string?}]
;                  [:tags nil {:malli/type :set
;                              :malli/children [{:malli/type keyword?}]}]
;                  [:address nil {:malli/type :map,
;                                 :malli/children [[:street nil {:malli/type string?}]
;                                                  [:city nil {:malli/type string?}]
;                                                  [:zip nil {:malli/type int?}]
;                                                  [:lonlat nil {:malli/type :tuple
;                                                                :malli/children [{:malli/type double?}
;                                                                                 {:malli/type double?}]}]]}]]}
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

Custom transformation via `:json-schema` namespaced properties:

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

Full override with `:json-schema` property:

```clj
(json-schema/transform 
  [:map {:json-schema {:type "file"}} 
   [:file any?]])
; {:type "file"}
```

### Swagger2

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

Custom transformation via `:swagger` and `:json-schema` namespaced properties:

```clj
(swagger/transform
  [:enum
   {:title "Fish"
    :description "It's a fish"
    :swagger/type "string"
    :json-schema/default "perch"}
   "perch" "pike"])
;{:title "Fish"
; :description "It's a fish"
; :type "string"
; :default "perch"
; :enum ["perch" "pike"]}
```

Full override with `:swagger` property:

```clj
(swagger/transform 
  [:map {:swagger {:type "file"}} 
   [:file any?]])
; {:type "file"}
```

## Custom Schema Types

Schema Types are described using `m/IntoSchema` protocol, which has a factory method
`(-into-schema [this properties children options])` to create the actual Schema instances. 
See `malli.core` for example implementations.

### Simple Schema

For simple cases, there is `m/-simple-schema`:

```clj
(require '[clojure.test.check.generators :as gen])

(def Over6
  (m/-simple-schema
    {:type :user/over6
     :pred #(and (int? %) (> % 6))
     :type-properties {:error/message "should be over 6"
                       :decode/string mt/-string->long
                       :json-schema/type "integer"
                       :json-schema/format "int64"
                       :json-schema/minimum 6
                       :gen/gen (gen/large-integer* {:min 7})}}))

(m/into-schema? Over6)
; => true
```

`m/IntoSchema` can be both used as Schema (creating a Schema instance with `nil` properties
and children) and as Schema type to create new Schema instances without needing to
register the types:

```clj
(m/schema? (m/schema Over6))
; => true

(m/schema? (m/schema [Over6 {:title "over 6"}]))
; => true
```

`:pred` is used for validation:

```clj
(m/validate Over6 2)
; => false

(m/validate Over6 7)
; => true
```

`:type-properties` are shared for all schema instances and are used just like Schema
(instance) properties by many Schema applications, including [error messages](#custom-error-messages),
[value generation](#value-generation) and [json-schema](#json-schema) transformations.

```clj
(json-schema/transform Over6)
; => {:type "integer", :format "int64", :minimum 6}

(json-schema/transform [Over6 {:json-schema/example 42}])
; => {:type "integer", :format "int64", :minimum 6, :example 42}
```

### Content Dependent Simple Schema

You can also build content-dependent schemas by using a callback function of `properties children -> opts` instead of static `opts`:

```clj
(def Over
  (m/-simple-schema
    (fn [{:keys [value]} _]
      (assert (int? value))
      {:type :user/over
       :pred #(and (int? %) (> % value))
       :type-properties {:error/fn (fn [error _] (str "should be over " value ", was " (:value error)))
                         :decode/string mt/-string->long
                         :json-schema/type "integer"
                         :json-schema/format "int64"
                         :json-schema/minimum value
                         :gen/gen (gen/large-integer* {:min (inc value)})}})))

(-> [Over {:value 12}]
    (m/explain 10)
    (me/humanize))
; => ["should be over 12, was 10"]
```

## Schema Registry

Schemas are looked up using a `malli.registry/Registry` protocol, which is effectively a map from schema `type` to a schema recipe (schema ast, `Schema` or `IntoSchema` instance).

Custom `Registry` can be passed in to all/most malli public apis via the optional options map using `:registry` key. If omitted, `malli.core/default-registry` is used.

```clj
;; the default registry
(m/validate [:maybe string?] "kikka")
; => true

;; registry as explicit options
(m/validate [:maybe string?] "kikka" {:registry m/default-registry})
; => true
```

The default immutable registry is merged from the following parts, enabling easy re-composition of custom schema sets:

#### `malli.core/predicate-schemas`

Contains both function values and unqualified symbol representations for all relevant core predicates. Having both representations enables reading forms from both code (function values) and EDN-files (symbols): `any?`, `some?`, `number?`, `integer?`, `int?`, `pos-int?`, `neg-int?`, `nat-int?`, `float?`, `double?`, `boolean?`, `string?`, `ident?`, `simple-ident?`, `qualified-ident?`, `keyword?`, `simple-keyword?`, `qualified-keyword?`, `symbol?`, `simple-symbol?`, `qualified-symbol?`, `uuid?`, `uri?`, `decimal?`, `inst?`, `seqable?`, `indexed?`, `map?`, `vector?`, `list?`, `seq?`, `char?`, `set?`, `nil?`, `false?`, `true?`, `zero?`, `rational?`, `coll?`, `empty?`, `associative?`, `sequential?`, `ratio?` and `bytes?`.

#### `malli.core/class-schemas`

Class-based schemas, contains `java.util.regex.Pattern` & `js/RegExp`.

#### `malli.core/comparator-schemas`

Comparator functions as keywords: `:>`, `:>=`, `:<`, `:<=`, `:=` and `:not=`.

#### `malli.core/type-schemas`

Type-like schemas: `:any`, `:nil`, `:string`, `:int`, `:double`, `:boolean`, `:keyword`, `:symbol`, `:qualified-symbol`, `:qualified-keyword` and `:uuid`.

### `malli.core/sequence-schemas`

Sequence/regex-schemas: `:+`, `:*`, `:?`, `:repeat`, `:cat`, `:alt`, `:catn`, `:altn`.

#### `malli.core/base-schemas`

Contains `:and`, `:or`, `:not`, `:map`, `:map-of`, `:vector`, `:sequential`, `:set`, `:tuple`, `:enum`, `:maybe`, `:multi`, `:re` and `:fn`.

### Custom registry

Example to create a custom registry without the default core predicates and with `:bool` and `:pos-int` Schemas:

```clj
(def registry
  (merge
    (m/class-schemas)
    (m/comparator-schemas)
    (m/base-schemas)
    {:bool (m/-simple-schema {:type :bool, :pred boolean?})
     :pos-int (m/-simple-schema {:type :pos-int, :pred pos-int?})}))

(m/validate [:or :bool :pos-int] 'kikka {:registry registry})
; => false

(m/validate [:or :bool :pos-int] 123 {:registry registry})
; => true
```

Predicate Schemas don't work anymore:

```clj
(m/validate pos-int? 123 {:registry registry})
; Syntax error (ExceptionInfo) compiling
; :malli.core/invalid-schema {:schema pos-int?}
```

### Local registry

Any schema can define a local registry using `:registry` schema property:

```clj
(def Adult
  [:map {:registry {::age [:and int? [:> 18]]}}
   [:age ::age]])

(mg/generate Adult {:size 10, :seed 1})
; => {:age 92}
```

Local registries can be persisted:

```clj
(-> Adult
    (malli.edn/write-string)
    (malli.edn/read-string)
    (m/validate {:age 46}))
; => true
```

See also [Recursive Schemas](#recursive-schemas).

### Changing the default registry

Using custom registries via `:registry` option is a simple solution, but this needs to be done for all public api calls. Also, with ClojureScript, the large (100+ schemas) default registry is not subject to any Dead Code Elimination (DCE), even if the schemas are not used in the application.

Malli allows the default registry to be replaced, with the following compiler/jvm bootstrap:
   * cljs: `:closure-defines {malli.registry/type "custom"}`
   * clj: `:jvm-opts ["-Dmalli.registry/type=custom"]`

It changes the default registry to empty one, which can be changed using `malli.registry/set-default-registy!`. Empty default registry enables DCE for all unused schema implementations.

Malli supports multiple types of registries.

### Immutable registry

```clj
(require '[malli.registry :as mr])

;; - cljs: :closure-defines {malli.registry/type "custom"}
;; -  clj: :jvm-opts ["-Dmalli.registry/type=custom"]
(mr/set-default-registry!
  {:string (m/-string-schema)
   :maybe (m/-maybe-schema)
   :map (m/-map-schema)})

(m/validate
  [:map [:maybe [:maybe :string]]]
  {:maybe "sheep"})
; => true

;; gzipped malli.core size as js down from 12Kb -> 1.2Kb
```

### Mutable registry

[clojure.spec](https://clojure.org/guides/spec) introduces a mutable global registry for specs. The mutable registry in malli forced you to bring in your own state atom and functions how to work with it:

Using a custom registry atom:

```clj
(def registry*
  (atom {:string (m/-string-schema)
         :maybe (m/-maybe-schema)
         :map (m/-map-schema)}))

(defn register! [type ?schema]
  (swap! registry* assoc type ?schema))

;; - cljs: :closure-defines {malli.registry/type "custom"}
;; -  clj: :jvm-opts ["-Dmalli.registry/type=custom"]
(mr/set-default-registry!
  (mr/mutable-registry registry*))

(register! :non-empty-string [:string {:min 1}])

(m/validate :non-empty-string "malli")
; => true
```

The mutable registry can also passed in as explicit option:

```clj
(def registry (mr/mutable-registry registry*))

(m/validate :non-empty-string "malli" {:registry registry})
; => true
```

### Dynamic Registry

If you know what you are doing, you can also use [dynamic scope](https://stuartsierra.com/2013/03/29/perils-of-dynamic-scope) to pass in default schema registry:

```clj
;; - cljs: :closure-defines {malli.registry/type "custom"}
;; -  clj: :jvm-opts ["-Dmalli.registry/type=custom"]
(mr/set-default-registry!
  (mr/dynamic-registry))

(binding [mr/*registry* {:string (m/-string-schema)
                         :maybe (m/-maybe-schema)
                         :map (m/-map-schema)
                         :non-empty-string [:string {:min 1}]}]
  (m/validate :non-empty-string "malli"))
; => true
```

### Lazy Registries

You can provide schemas at runtime using `mr/lazy-registry` - it takes a local registry
and a provider function of `type registry -> schema` as arguments:

```clj
(def registry
  (mr/lazy-registry
    (m/default-schemas)
    (fn [type registry]
      ;; simulates pulling CloudFormation Schemas when needed
      (let [lookup {"AWS::ApiGateway::UsagePlan" [:map {:closed true}
                                                  [:Type [:= "AWS::ApiGateway::UsagePlan"]]
                                                  [:Description {:optional true} string?]
                                                  [:UsagePlanName {:optional true} string?]]
                    "AWS::AppSync::ApiKey" [:map {:closed true}
                                            [:Type [:= "AWS::AppSync::ApiKey"]]
                                            [:ApiId string?]
                                            [:Description {:optional true} string?]]}]
        (println "... loaded" type)
        (some-> type lookup (m/schema {:registry registry}))))))

;; lazy multi, doesn't realize the schemas
(def CloudFormation
  (m/schema
    [:multi {:dispatch :Type, :lazy-refs true}
     "AWS::ApiGateway::UsagePlan"
     "AWS::AppSync::ApiKey"]
    {:registry registry}))

(m/validate
  CloudFormation
  {:Type "AWS::ApiGateway::UsagePlan"
   :Description "laiskanlinna"})
; ... loaded AWS::ApiGateway::UsagePlan
; => true

(m/validate
  CloudFormation
  {:Type "AWS::ApiGateway::UsagePlan"
   :Description "laiskanlinna"})
; => true
```

Inspired by [F# Type providers](https://docs.microsoft.com/en-us/dotnet/fsharp/tutorials/type-providers/).

### Composite Registry

Registries can be composed:

```clj
(require '[malli.core :as m])
(require '[malli.registry :as mr])

;; bring your own evil
(def registry (atom {}))

(defn register! [type schema]
  (swap! registry assoc type schema))

;; - cljs: :closure-defines {malli.registry/type "custom"}
;; -  clj: :jvm-opts ["-Dmalli.registry/type=custom"]
(mr/set-default-registry!
  ;; linear search
  (mr/composite-registry
    ;; immutable registry
    {:map (m/-map-schema)}
    ;; mutable (spec-like) registry
    (mr/mutable-registry registry)
    ;; on the perils of dynamic scope
    (mr/dynamic-registry)))

;; mutate like a boss
(register! :maybe (m/-maybe-schema))

;; ☆.。.:*・°☆.。.:*・°☆.。.:*・°☆.。.:*・°☆
(binding [mr/*registry* {:string (m/-string-schema)}]
  (m/validate
    [:map [:maybe [:maybe :string]]]
    {:maybe "sheep"}))
; => true
```

## Function Schemas

Functions can be described with `:=>`, which has two children: input schema (as `:cat`) and output schemas. Multi-arity functions can be composed with `:function`.

```clj
;; no args, no return
[:=> :cat :nil]

;; two int args, positive int returned
[:=> [:cat int? int?] pos-int?]

;; named varargs, e.g. (fn [x & xs] (* x (apply + xs)))
[:=> [:catn 
      [:x int?]
      [:xs [:* int?]]] int?]
      
;; multi-arity fn
[:function
 [:=> [:cat int?] int?]
 [:=> [:cat int? int? [:* int?]] int?]]
```

Function validation:

```clj
(defn plus [x y] (+ x y))

(def =>plus [:=> [:cat int? int?] int?])

(m/validate =>plus plus)
; => true
```

By default, validation just checks if a value is `ifn?`:

```clj
(m/validate =>plus str)
; => true :(
```

Using generative testing for better results:

```clj
(m/validate =>plus plus {::m/function-checker mg/function-checker})
; => true

(m/validate =>plus str {::m/function-checker mg/function-checker})
; => false

(m/explain =>plus str {::m/function-checker mg/function-checker})
;{:schema [:=> [:cat int? int?] int?],
; :value #object[clojure.core$str],
; :errors (#Error{:path [],
;                 :in [],
;                 :schema [:=> [:cat int? int?] int?],
;                 :value #object[clojure.core$str],
;                 :check {:total-nodes-visited 1,
;                         :depth 0,
;                         :pass? false,
;                         :result false,
;                         :result-data nil,
;                         :time-shrinking-ms 0,
;                         :smallest [(0 0)],
;                         :malli.generator/explain-output {:schema int?,
;                                                          :value "00",
;                                                          :errors (#Error{:path []
;                                                                          :in []
;                                                                          :schema int?
;                                                                          :value "00"})}}})}
``` 

A generated function implementation:

```clj
(def plus-gen (mg/generate =>plus))

(plus-gen 1 2)
; => -1

(plus-gen 1 "2")
; =throws=> :malli.generator/invalid-input {:schema [:cat int? int?], :args [1 "2"]}
```

Multiple arities are defined using `:function`:

```clj
(def SmallInt
  [:int {:min -100, :max 100}])

(def MyFunction
  (m/schema
    [:function
     [:=> [:cat SmallInt] :int]
     [:=> [:cat SmallInt SmallInt [:* SmallInt]] :int]]
    {::m/function-checker mg/function-checker}))

(m/validate
  MyFunction
  (fn
    ([x] x)
    ([x y & z] (apply - (- x y) z))))
; => true

(m/validate
  MyFunction
  (fn
    ([x] x)
    ([x y & z] (str x y z))))
; => false

(m/explain
  MyFunction
  (fn
    ([x] x)
    ([x y & z] (str x y z))))
;{:schema [:function
;          [:=> [:cat SmallInt] :int]
;          [:=> [:cat SmallInt SmallInt [:* SmallInt]] :int]],
; :value #object[],
; :errors (#Error{:path [],
;                 :in [],
;                 :schema [:function
;                          [:=> [:cat SmallInt] :int]
;                          [:=> [:cat SmallInt SmallInt [:* SmallInt]] :int]],
;                 :value #object[],
;                 :check ({:total-nodes-visited 1,
;                          :depth 0,
;                          :pass? false,
;                          :result false,
;                          :result-data nil,
;                          :time-shrinking-ms 0,
;                          :smallest [(0 0)],
;                          :malli.generator/explain-output {:schema :int,
;                                                           :value "00",
;                                                           :errors (#Error{:path []
;                                                                           :in []
;                                                                           :schema :int
;                                                                           :value "00"})}})})}

(def generated-f (mg/generate MyFunction))

(generated-f)
; =throws=> :malli.generator/invalid-arity {:arity 0, :arities #{1 :varargs}, :args nil, :schema ...}

(generated-f 1)
; => 5832893

(generated-f 1 2)
; => -120532359

(generated-f 1 2 3 4)
; => -57994624
```

## Function Schema Registry

Vars can be annotated with function schemas using `m/=>` macro, backed by a global registry:

```clj
(defn square [x] (* x x))

(m/=> square [:=> [:cat int?] pos-int?])
```

Listing registered function Var schemas:

```clj
(m/function-schemas)
;{user
; {square
;  {:schema [:=> [:cat int?] pos-int?]
;   :meta nil
;   :ns malli.generator-test
;   :name square}}}
```

## Clj-kondo

[Clj-kondo](https://github.com/borkdude/clj-kondo) is a linter for Clojure code that sparks joy.

Given functions and function Schemas:

```clj
(defn square [x] (* x x))
(m/=> square [:=> [:cat int?] nat-int?])

(defn plus
  ([x] x)
  ([x y] (+ x y)))

(m/=> plus [:function
            [:=> [:cat int?] int?]
            [:=> [:cat int? int?] int?]])
```

Generating `clj-kondo` configuration from current namespace:

```clj
(require '[malli.clj-kondo :as mc])

(-> (mc/collect *ns*) (mc/linter-config))
;{:lint-as #:malli.schema{defn schema.core/defn},
; :linters
; {:type-mismatch
;  {:namespaces
;   {user {square {:arities {1 {:args [:int]
;                               :ret :pos-int}}}
;          plus {:arities {1 {:args [:int]
;                             :ret :int},
;                          2 {:args [:int :int]
;                             :ret :int}}}}}}}}
```

Emitting confing into `./.clj-kondo/configs/malli/config.edn`:

```clj
(mc/emit!)
```

To enable the integration, you need to have the following config:

```bash
✗ cat .clj-kondo/config.edn
{:config-paths ["configs/malli"]}
```

In action:

![malli](docs/img/clj-kondo.png)

## Visualizing Schemas

Transforming Schemas into [DOT Language](https://en.wikipedia.org/wiki/DOT_(graph_description_language)):

```clj
(require '[malli.dot :as md])

(md/transform
  [:schema
   {:registry {"Country" [:map
                          [:name [:enum :FI :PO]]
                          [:neighbors [:vector [:ref "Country"]]]]
               "Burger" [:map
                         [:name string?]
                         [:description {:optional true} string?]
                         [:origin [:maybe "Country"]]
                         [:price pos-int?]]
               "OrderLine" [:map
                            [:burger "Burger"]
                            [:amount int?]]
               "Order" [:map
                        [:lines [:vector "OrderLine"]]
                        [:delivery [:map
                                    [:delivered boolean?]
                                    [:address [:map
                                               [:street string?]
                                               [:zip int?]
                                               [:country "Country"]]]]]]}}
   "Order"])
; digraph {
;   node [shape="record", style="filled", color="#000000"]
;   edge [dir="back", arrowtail="none"]
;  
;   "Burger" [label="{Burger|:name string?\l:description string?\l:origin [:maybe \"Country\"]\l:price pos-int?\l}", fillcolor="#fff0cd"]
;   "Country" [label="{Country|:name [:enum :FI :PO]\l:neighbors [:vector [:ref \"Country\"]]\l}", fillcolor="#fff0cd"]
;   "Order" [label="{Order|:lines [:vector \"OrderLine\"]\l:delivery Order$Delivery\l}", fillcolor="#fff0cd"]
;   "Order$Delivery" [label="{Order$Delivery|:delivered boolean?\l:address Order$Delivery$Address\l}", fillcolor="#e6caab"]
;   "Order$Delivery$Address" [label="{Order$Delivery$Address|:street string?\l:zip int?\l:country Country\l}", fillcolor="#e6caab"]
;   "OrderLine" [label="{OrderLine|:burger Burger\l:amount int?\l}", fillcolor="#fff0cd"]
;  
;   "Burger" -> "Country" [arrowtail="odiamond"]
;   "Country" -> "Country" [arrowtail="odiamond"]
;   "Order" -> "OrderLine" [arrowtail="odiamond"]
;   "Order" -> "Order$Delivery" [arrowtail="diamond"]
;   "Order$Delivery" -> "Order$Delivery$Address" [arrowtail="diamond"]
;   "Order$Delivery$Address" -> "Country" [arrowtail="odiamond"]
;   "OrderLine" -> "Burger" [arrowtail="odiamond"]
; }
```

Visualized with [Graphviz](https://graphviz.org/):

<img src="https://raw.githubusercontent.com/metosin/malli/master/docs/img/dot.png"/>

## Performance

Validation:

```clj
(require '[clojure.spec.alpha :as s])
(require '[criterium.core :as cc])

;; 40ns
(let [spec (s/and int? (s/or :pos-int pos-int? :neg-int neg-int?))
      valid? (partial s/valid? spec)]
  (cc/quick-bench
    (valid? 0)))

;; 5ns
(let [valid? (m/validator [:and int? [:or pos-int? neg-int?]])]
  (cc/quick-bench
    (valid? 0)))
```

Parsing:

```clj
;; 44µs
(let [spec (s/* (s/cat :prop string?,
                       :val (s/alt :s string?
                                   :b boolean?)))
      parse (partial s/conform spec)]
  (cc/quick-bench
    (parse ["-server" "foo" "-verbose" "-verbose" "-user" "joe"])))

;; 2.5µs
(let [schema [:* [:catn
                  [:prop string?]
                  [:val [:altn
                         [:s string?]
                         [:b boolean?]]]]]
      parse (m/parser schema)]
  (cc/quick-bench
    (parse ["-server" "foo" "-verbose" "-verbose" "-user" "joe"])))
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

## Motivation

We are building dynamic multi-tenant systems where data-models should be first-class: they should drive the runtime value transformations, forms and processes. We should be able to edit the models at runtime, persist them and load them back from database and over the wire, for both Clojure and ClojureScript. Think of [JSON Schema](https://json-schema.org/), but for Clojure/Script.

Hasn't the problem been solved (many times) already?

There is [Schema](https://github.com/plumatic/schema), which is awesome, proven and collaborative open source project, and we absolutely love it. We still use it in most of our projects. Sad part: serializing & de-serializing schemas is non-trivial and there is no back-tracking on branching.

[Spec](https://clojure.org/guides/spec) is the de facto data specification library for Clojure. It has many great ideas, but it is based on macros, it has a global registry and it doesn't support runtime transformations. [Spec-tools](https://github.com/metosin/spec-tools) was created to "fix" some of the things, but after [four years](https://github.com/metosin/spec-tools/commit/18aeb78db7886c985b2881fd87fde6039128b3fb) of developing it, it's still kinda hack and not fun to maintain.

So, we decided to spin out our own library, which would do all the things we feel is important for dynamic system development. It's based on the best parts of the existing libraries and several project-specific tools we have done over the years.

> If you have expectations (of others) that aren't being met, those expectations are your own responsibility. You are responsible for your own needs. If you want things, make them.

- Rich Hickey, [Open Source is Not About You](https://gist.github.com/richhickey/1563cddea1002958f96e7ba9519972d9)

## Links (and thanks)

- Schema https://github.com/plumatic/schema
- Clojure.spec https://clojure.org/guides/spec
- Spell-spec https://github.com/bhauman/spell-spec
- JSON Schema https://json-schema.org/understanding-json-schema/index.html
- Spec-provider: https://github.com/stathissideris/spec-provider
- F# Type Providers: https://docs.microsoft.com/en-us/dotnet/fsharp/tutorials/type-providers/
- Minimallist https://github.com/green-coder/minimallist
- Core.typed https://github.com/clojure/core.typed
- TypeScript https://www.typescriptlang.org/
- Struct https://funcool.github.io/struct/latest/
- Seqexp https://github.com/cgrand/seqexp
- yup https://github.com/jquense/yup
- JOI https://github.com/hapijs/joi

## Alpha

Public api of Malli has been quite stable already in [pre-alpha](https://github.com/metosin/malli/issues/207) and in alpha, we try not to break things. Still, the library is evolving and things like [value destructuring](https://github.com/metosin/malli/issues/241) **could** effect public apis and **most likely** effect the library extenders, e.g. need to implement a new protocol method for custom schemas.

All changes (breaking or not) will be documented in the [CHANGELOG](CHANGELOG.md) and there will be migration guide and path if needed.

The api layers and stability:

* **public api**: public vars, name doesn't start with `-`, e.g. `malli.core/validate`. Most stable part of the library, should not change (much) in alpha
* **extender api**: public vars, name starts with `-`, e.g. `malli.core/-collection-schema`. Not needed with basic use cases, might evolve during the alpha, follow [CHANGELOG](CHANGELOG.md) for details
* **private api**: private vars, all bets are off.

## Supported Java versions

Malli aims to support the LTS releases Java 8 and 11 and the latest Java release, Java 15.

## Running tests

We use [Kaocha](https://github.com/lambdaisland/kaocha) and [cljs-test-runner](https://github.com/Olical/cljs-test-runner) as a test runners. Before running the tests, you need to install NPM dependencies.

```bash
npm install
./bin/kaocha
./bin/node
```

## Installing locally

```bash
clj -Mjar
clj -Minstall
```

## Bundle size for cljs

With default registry (37KB+ Gzipped)

```bash
# no sci
npx shadow-cljs run shadow.cljs.build-report app /tmp/report.html

# with sci
npx shadow-cljs run shadow.cljs.build-report app-sci /tmp/report.html
```

With minimal registry (2.4KB+ Gzipped)

```bash
# no sci
npx shadow-cljs run shadow.cljs.build-report app2 /tmp/report.html

# with sci
npx shadow-cljs run shadow.cljs.build-report app2-sci /tmp/report.html
```

## Checking the generated code

```bash
npx shadow-cljs release app --pseudo-names
```

## Testing on GraalVM

Without sci (11Mb)

```bash
./bin/native-image demo
./demo '[:set :keyword]' '["kikka" "kukka"]'
```

With sci (18Mb):

```clj
./bin/native-image demosci
./demosci '[:fn (fn [x] (and (int? x) (> x 10)))]]' '12'
```

## License

Copyright © 2019-2021 Metosin Oy and contributors.

Available under the terms of the Eclipse Public License 2.0, see `LICENSE`.
