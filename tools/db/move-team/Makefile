LANG := en_US.UTF-8

SHELL := /usr/bin/env bash

EXECUTABLES_COMPONENTS := wire-server:move-team wire-server:move-team-generate

default: all

all: fast

.PHONY: init
init:
	mkdir -p dist

.PHONY: clean
clean:
	stack clean

.PHONY: compile
compile:
	stack build .

.PHONY: install
install: init
	stack install --pedantic --local-bin-path=dist $(EXECUTABLES_COMPONENTS)

.PHONY: fast
fast: init
	stack install --fast --local-bin-path=dist $(EXECUTABLES_COMPONENTS)
