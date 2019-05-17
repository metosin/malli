# malli - plain data schemas

## Rationale
- Schemas as immutable data
  - can be transported over the line
  - saved to database
  - combined freely
- Does not depend on global registry
- Programmable, on the runtime as well

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

