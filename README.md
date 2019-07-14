# malli [![Build Status](https://img.shields.io/circleci/project/github/metosin/malli.svg)](https://circleci.com/gh/metosin/malli)

Plain data Schemas for Clojure/Script.

**STATUS**: *Pre-alpha*, in design and prototyping phase.

<img src="https://raw.githubusercontent.com/metosin/malli/master/docs/img/malli.png" width=240 align="right"/>

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
     [:x boolean?]
     [[:opt :y] int?]
     [:z string?]]
    (m/schema)
    (pr-str)
    (edn/read-string)
    (m/schema)
    (m/validate
      {:x true, :z "kikka"}))
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
