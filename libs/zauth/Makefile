SHELL         := /usr/bin/env bash
CABAL_SANDBOX ?= $(CURDIR)/.cabal-sandbox
NAME          := zauth
VERSION       := $(shell sed -n 's/^version: *\(.*\)$$/\1/p' $(NAME).cabal)
SDIST         := dist/$(NAME)-$(VERSION).tar.gz
ZAKKAGE       ?= $(HOME)/.zakkage

default: clean install

cabal.sandbox.config:
	cabal sandbox --sandbox=$(CABAL_SANDBOX) init
	[ -d $(ZAKKAGE) ] && echo -e "local-repo: $(ZAKKAGE)\n$$(cat cabal.sandbox.config)" > cabal.sandbox.config

.PHONY: init
init: cabal.sandbox.config

.PHONY: clean
clean: init
	cabal clean

.PHONY: configure
configure: init
	cabal configure --enable-tests --enable-bench

.PHONY: compile
compile: configure
	cabal build

.PHONY: test
test: init
	cabal test

.PHONY: install
install: init
	cabal install --enable-tests --enable-bench --force-reinstalls -j

.PHONY: dist
dist: configure
	cabal sdist

