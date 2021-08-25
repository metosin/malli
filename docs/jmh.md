# Benchmarking with JMH

## Build

Requires tools build

```sh
clj -T:build all
```

## Run

```sh
clojure -M:jmh '{:output "jmh-report.edn"}'
```
