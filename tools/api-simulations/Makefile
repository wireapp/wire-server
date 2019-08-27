LANG := en_US.UTF-8
SHELL        := /usr/bin/env bash
NAME         := api-simulations
# STACK_OPTIONS are the options used for most stack calls in this
# Makefile; WIRE_STACK_OPTIONS is the place for the user to override
# things.
STACK_OPTIONS := --pedantic --test --bench --no-run-benchmarks --local-bin-path=$(shell stack path --project-root)/dist
VERSION      ?=
BUILD_NUMBER ?= 0
BUILD_LABEL  ?= local
BUILD        := $(BUILD_NUMBER)$(shell [ "${BUILD_LABEL}" == "" ] && echo "" || echo ".${BUILD_LABEL}")
DEB          := $(NAME)_$(VERSION)+$(BUILD)_amd64.deb

guard-%:
	@ if [ "${${*}}" = "" ]; then \
	      echo "Environment variable $* not set"; \
	    exit 1; \
fi

default: fast

all: clean install

init:
	mkdir -p ../../dist

.PHONY: clean
clean:
	stack clean . $(STACK_OPTIONS)
	rm -f .metadata

.PHONY: fast
fast: init
	stack install . $(STACK_OPTIONS) --fast $(WIRE_STACK_OPTIONS)

.PHONY:
compile:
	stack build . $(STACK_OPTIONS) --no-copy-bins $(WIRE_STACK_OPTIONS)

.PHONY: install
install: init
	stack install . $(STACK_OPTIONS) $(WIRE_STACK_OPTIONS)

.PHONY: dist
dist: guard-VERSION install $(DEB) .metadata

$(DEB):
	makedeb --name=$(NAME) \
	 --version=$(VERSION) \
	 --debian-dir=deb \
	 --build=$(BUILD_NUMBER) \
	 --architecture=amd64 \
	 --output-dir=dist

mailboxes.json:
	khan artifact download --bucket z-config --key simulator/mailboxes.json --file ./mailboxes.json

.metadata:
	echo -e "NAME=$(NAME)\nVERSION=$(VERSION)\nBUILD_NUMBER=$(BUILD)" > .metadata
