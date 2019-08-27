LANG := en_US.UTF-8

SHELL := /usr/bin/env bash
# STACK_OPTIONS are the options used for most stack calls in this
# Makefile; WIRE_STACK_OPTIONS is the place for the user to override
# things.
STACK_OPTIONS := --pedantic --test --bench --no-run-benchmarks --local-bin-path=$(shell stack path --project-root)/dist

default: all

all: install

.PHONY: init
init:
	mkdir -p ../../../dist

.PHONY: clean
clean:
	stack clean . $(STACK_OPTIONS)

.PHONY: compile
compile:
	stack build . $(STACK_OPTIONS) $(WIRE_STACK_OPTIONS)

.PHONY: install
install: init
	stack install . $(STACK_OPTIONS) $(WIRE_STACK_OPTIONS)

.PHONY: fast
fast: init
	stack install . $(STACK_OPTIONS) --fast $(WIRE_STACK_OPTIONS)
