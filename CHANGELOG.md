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

* 17.6.2020
  * **BREAKING:** change all `malli.core/*-registy` defs into `malli.core/*-schemas` defns to enable DCE for clojurescript
* 9.6.2020 
  * **BREAKING:** `malli.core/name` & `malli.core/-name` renamed to `malli.core/type` & `malli.core/-type`
  * **BREAKING:** `malli.generator/-generator` is renamed to `malli.generator/-schema-generator`
