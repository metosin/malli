# Malli CHANGELOG

We use [Break Versioning][breakver]. The version numbers follow a `<major>.<minor>.<patch>` scheme with the following intent:

| Bump    | Intent                                                     |
| ------- | ---------------------------------------------------------- |
| `major` | Major breaking changes -- check the changelog for details. |
| `minor` | Minor breaking changes -- check the changelog for details. |
| `patch` | No breaking changes, ever!!                                |

`-SNAPSHOT` versions are preview versions for upcoming releases.

[breakver]: https://github.com/ptaoussanis/encore/blob/master/BREAK-VERSIONING.md

## UNRELEASED

* 23.7.2020
  * **BREAKING:**: `sci` is not a default dependency. Enabling sci-support:
    * **Clojure**: add a dependency to `borkdude/sci`
    * **ClojureScript**: also require `sci.core` (directly or via `:preloads`)
* 18.7.2020
  * **BREAKING:**: big cleanup of `malli.transform` internals.
* 12.7.2020
  * **BREAKING:**: `malli.mermaid` is removed (in favor of `malli.dot`)  
* 10.7.2020
  * `[metosin/malli "0.0.1-20200710.075225-19"]`
  * **BREAKING:**: Visitor is implemented using a Walker.
    * `m/accept` -> `m/walk`
    * `m/schema-visitor` -> `m/schema-walker`
    * `m/map-syntax-visitor` -> `m/map-syntax-walker`
* 31.6.2020
  * **BREAKING:** new `-children` method in `Schema`, to return child schemas as instances (instead of just AST)
* 17.6.2020
  * **BREAKING:** change all `malli.core/*-registy` defs into `malli.core/*-schemas` defns to enable DCE for clojurescript
* 9.6.2020 
  * **BREAKING:** `malli.core/name` & `malli.core/-name` renamed to `malli.core/type` & `malli.core/-type`
  * **BREAKING:** `malli.generator/-generator` is renamed to `malli.generator/-schema-generator`
