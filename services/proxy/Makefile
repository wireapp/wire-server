SHELL         := /usr/bin/env bash
CABAL_SANDBOX ?= $(CURDIR)/.cabal-sandbox
NAME          := proxy
VERSION       := $(shell sed -n 's/^version: *\(.*\)$$/\1/p' $(NAME).cabal)
BUILD_NUMBER  ?= 0
BUILD_LABEL   ?= local
BUILD         := $(BUILD_NUMBER)$(shell [ "${BUILD_LABEL}" == "" ] && echo "" || echo ".${BUILD_LABEL}")
VERBOSE       ?= 1
UNREGISTERED  := proxy
ZAKKAGE       ?= $(HOME)/.zakkage

default: clean compile

cabal.sandbox.config:
	cabal sandbox --sandbox=$(CABAL_SANDBOX) init
	[ -d $(ZAKKAGE) ] && echo -e "local-repo: $(ZAKKAGE)\n$$(cat cabal.sandbox.config)" > cabal.sandbox.config

.PHONY: unregister
unregister: cabal.sandbox.config $(addprefix unregister-,$(UNREGISTERED))

unregister-%:
	cabal sandbox hc-pkg unregister $* || true

deps: cabal.sandbox.config
	cabal --require-sandbox install \
		-j \
		--only-dep \
		--enable-tests \
		--enable-bench \
		--force-reinstalls \
		--disable-documentation \
		--verbose=$(VERBOSE)

compile: deps
	cabal --require-sandbox build --verbose=$(VERBOSE)

clean: unregister
	cabal clean
	-rm -f .metadata

.PHONY: dist
dist: sdist deb .metadata

.PHONY: deb
deb: compile
	makedeb --name=$(NAME) \
		--version=$(VERSION) \
		--debian-dir=deb \
		--build=$(BUILD) \
		--architecture=amd64 \
		--output-dir=dist

sdist:
	cabal sdist

.metadata:
	echo -e "NAME=$(NAME)\nVERSION=$(VERSION)\nBUILD_NUMBER=$(BUILD)" > .metadata
