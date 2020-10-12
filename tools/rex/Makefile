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

# build a statically linked binary by compiling on alpine. Will result in a 'rex-static' binary in dist.
.PHONY: static
static: $(SRC)
	docker run --rm \
		-v $(CURDIR)/../..:/wire-server \
		quay.io/wire/alpine-builder:develop \
		sh -c 'apk add --no-cache zlib-static && cd /wire-server/tools/rex && stack --system-ghc --local-bin-path . --allow-different-user install --flag rex:static . && mv rex dist/rex-static'
