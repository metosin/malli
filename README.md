# malli - plain data schemas

## Rationale

- Schemas as immutable data
- Schema-driven Runtime Validation
- Schema-driven Transformations
- Tools for programming with Schemas
- No global state, explicit everything
- First class error messages
- Fast

## Examples

```clj
(require '[malli.core :as m])

(def schema (m/schema int?))

(m/validate schema "1") ; => false
(m/validate schema 1) ; => true

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

As Schemas are all data, they can be serialized and deserialized easily:

```clj
(require '[clojure.edn :as edn])

(->
  [:map
   [:x boolean?]
   [[:opt :y] int?]
   [:z string?]]
  (m/schema)
  (pr-str) ;; write to edn
  (edn/read-string) ;; read from edn
  (m/schema)
  (m/validate
    {:x true, :y 1, :z "kikka"}))
; => true
```

## Status

WIP

## More

Should have:
- Context dependent pluggable validators
- Context dependent pluggable coercers
- Close compatibility to Json Schema 7
- First class error messages

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

