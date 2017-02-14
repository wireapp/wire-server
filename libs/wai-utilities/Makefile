SHELL         := /usr/bin/env bash
NAME          := wai-utilities
CABAL_SANDBOX ?= $(CURDIR)/.cabal-sandbox
ZAKKAGE       ?= $(HOME)/.zakkage

default: build

.PHONY: build
build: cabal.sandbox.config deps
	cabal --require-sandbox build -j

.PHONY: clean
clean:
	cabal clean

.PHONY: compile
compile: build

.PHONY: deps
deps:
	cabal --require-sandbox install -j --only-dep

.PHONY: dist
dist: build
	cabal sdist

.PHONY: distclean
distclean: clean
	cabal sandbox delete

.PHONY: install
install:
	cabal --require-sandbox install \
		--enable-tests --enable-bench --enable-documentation -j

cabal.sandbox.config:
	cabal sandbox --sandbox=$(CABAL_SANDBOX) init
	[ -d $(ZAKKAGE) ] && echo -e "local-repo: $(ZAKKAGE)\n$$(cat cabal.sandbox.config)" > cabal.sandbox.config
