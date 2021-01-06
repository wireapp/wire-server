LANG                       := en_US.UTF-8
SHELL                      := /usr/bin/env bash
NAME                       := federator
EXE_IT                     := ../../dist/$(NAME)-integration
INTEGRATION_USE_NGINZ      ?= 0

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

.PHONY: i
i:
	INTEGRATION_USE_NGINZ=$(INTEGRATION_USE_NGINZ) ../integration.sh $(EXE_IT) -s $(NAME).integration.yaml -i ../integration.yaml $(WIRE_INTEGRATION_TEST_OPTIONS)

.PHONY: i-list
i-list:
	$(EXE_IT) -s $(NAME).integration.yaml -i ../integration.yaml -l

i-%:
	INTEGRATION_USE_NGINZ=$(INTEGRATION_USE_NGINZ) ../integration.sh $(EXE_IT) -s $(NAME).integration.yaml -i ../integration.yaml -p "$*" $(WIRE_INTEGRATION_TEST_OPTIONS)

.PHONY: integration
integration: fast i

.PHONY: integration-list
integration-list: fast i-list

integration-%: fast
	make "i-$*"
