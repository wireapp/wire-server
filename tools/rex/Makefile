LANG         := en_US.UTF-8
SHELL        := /usr/bin/env bash
SRC          := Main.hs
NAME         := rex
VERSION      ?=

default: clean install

guard-%:
	@ if [ "${${*}}" = "" ]; then \
	      echo "Environment variable $* not set"; \
	    exit 1; \
	fi

.PHONY: clean
clean:
	stack clean $(NAME)
	rm -f rex *.docker *.aci *.asc

init:
	mkdir -p ../../dist

# dynamically compiled rex added to ../../dist/
.PHONY: install
install: init
	stack install . --pedantic --test --bench --no-run-benchmarks --local-bin-path=dist

# dynamically compiled rex added to ../../dist/
.PHONY: fast
fast: init
	stack install . --pedantic --test --bench --no-run-benchmarks --local-bin-path=dist --fast $(WIRE_STACK_OPTIONS)

.PHONY:
compile:
	stack build . --pedantic --test --bench --no-run-benchmarks --no-copy-bins
