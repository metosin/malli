KAOCHA-TARGETS :=

node_modules:
	npm install

kaocha: node_modules
	./bin/kaocha $(KAOCHA-TARGETS)

.PHONY: test
test: kaocha

.PHONY: test-clj
test-clj: KAOCHA-TARGETS := unit
test-clj: kaocha

.PHONY: test-cljs
test-cljs: KAOCHA-TARGETS := unit-cljs
test-cljs: kaocha