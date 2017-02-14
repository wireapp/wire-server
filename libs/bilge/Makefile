SHELL         := /usr/bin/env bash
CABAL_SANDBOX ?= $(CURDIR)/.cabal-sandbox
NAME          := $(lastword $(subst /, ,$(CURDIR)))
VERSION       := $(shell sed -n 's/^version: *\(.*\)$$/\1/p' $(NAME).cabal)
SRC           := $(shell find src -name "*.hs")
SDIST         := dist/$(NAME)-$(VERSION).tar.gz
ZAKKAGE       ?= $(HOME)/.zakkage

VERBOSE ?= 1


default: clean dist

.PHONY: init
init:
	cabal sandbox --sandbox=$(CABAL_SANDBOX) init
	[ -d $(ZAKKAGE) ] && echo -e "local-repo: $(ZAKKAGE)\n$$(cat cabal.sandbox.config)" > cabal.sandbox.config

.PHONY: clean
clean: init
	cabal clean

.PHONY: configure
configure: init
	cabal configure --enable-tests --enable-bench

.PHONY: compile
compile: configure
	cabal build -j --verbose=$(VERBOSE)

.PHONY: install
install-dep: init
	cabal install --only-dep --enable-bench --disable-documentation \
		--avoid-reinstalls -j --verbose=$(VERBOSE)

.PHONY: dist
dist: install-dep compile $(SDIST)

$(SDIST): $(SRC)
	cabal sdist

.PHONY: lint
lint:
	hlint src

