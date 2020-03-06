LANG          := en_US.UTF-8
SHELL         := /usr/bin/env bash
NAME          := federator

default: fast

init:
	mkdir -p ../../dist

.PHONY: install
install: init
	stack install . --pedantic --test --bench --no-run-benchmarks --local-bin-path=dist $(WIRE_STACK_OPTIONS)

.PHONY: fast
fast: init
	stack install . --pedantic --test --bench --no-run-benchmarks --local-bin-path=dist --fast $(WIRE_STACK_OPTIONS)

.PHONY: compile
compile:
	stack build . --fast --test --bench --no-run-benchmarks --no-copy-bins

.PHONY: run
run: fast
	./dist/federator -c federator.integration.yaml
