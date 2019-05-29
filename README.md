# malli - plain data schemas

## Rationale
- Schemas as immutable data
  - Can be transported over the wire
  - Saved to the database
  - Combined freely
- Does not depend on global registry
- Programmable, on the runtime as well

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

## WIP Design and examples

Discussion about format and all open topics at 
[src/malli/design_draft.clj](src/malli/design_draft.clj).
