LANG := en_US.UTF-8
SHELL        := /usr/bin/env bash
# STACK_OPTIONS are the options used for most stack calls in this
# Makefile; WIRE_STACK_OPTIONS is the place for the user to override
# things.
STACK_OPTIONS := --pedantic --test --bench --no-run-benchmarks --local-bin-path=$(shell stack path --project-root)/dist
NAME         := $(lastword $(subst /, ,$(CURDIR)))
VERSION       = $(shell sed -n 's/^version: *\(.*\)$$/\1/p' $(NAME).cabal)
BUILD_NUMBER ?= 0
BUILD_LABEL  ?= local
BUILD        := $(BUILD_NUMBER)$(shell [ "${BUILD_LABEL}" == "" ] && echo "" || echo ".${BUILD_LABEL}")
DEB          := $(NAME)_$(VERSION)+$(BUILD_NUMBER)_amd64.deb

default: all

all: clean install

init:
	mkdir -p ../../dist

.PHONY: clean
clean:
	stack clean . $(STACK_OPTIONS)
	rm -f .metadata

.PHONY: install
install: init
	stack install . $(STACK_OPTIONS) $(WIRE_STACK_OPTIONS)

.PHONY:
compile:
	stack build . $(STACK_OPTIONS) --no-copy-bins $(WIRE_STACK_OPTIONS)

.PHONY: sdist
sdist: init
	stack sdist . $(STACK_OPTIONS)

.PHONY: dist
dist: sdist install $(DEB) .metadata

$(DEB):
	makedeb --name=$(NAME) \
		--version=$(VERSION) \
		--debian-dir=deb \
		--build=$(BUILD) \
		--architecture=amd64 \
		--output-dir=dist

.metadata:
	echo -e "NAME=$(NAME)\nVERSION=$(VERSION)\nBUILD_NUMBER=$(BUILD_NUMBER)" > .metadata
