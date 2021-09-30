LANG := en_US.UTF-8
SHELL        := /usr/bin/env bash
NAME         := bonanza
VERSION      ?=
BUILD_NUMBER ?= 0
DEB          := dist/$(NAME)_$(VERSION)+$(BUILD_NUMBER)_amd64.deb
EXECUTABLES_COMPONENTS := wire-server:bonanza wire-server:kibanana wire-server:kibana-raw

guard-%:
	@ if [ "${${*}}" = "" ]; then \
	      echo "Environment variable $* not set"; \
	    exit 1; \
	fi

default: all

all: clean install

init:
	mkdir -p ../../dist

.PHONY: clean
clean:
	stack clean bonanza

.PHONY: install
install: init
	stack install $(EXECUTABLES_COMPONENTS) --pedantic --test --bench --no-run-benchmarks --local-bin-path=dist

.PHONY: compile
compile:
	stack build $(EXECUTABLES_COMPONENTS) --pedantic --test --bench --no-run-benchmarks --no-copy-bins --fast $(WIRE_STACK_OPTIONS)

.PHONY: dist
dist: guard-VERSION $(DEB)

$(DEB): install
	makedeb --name=$(NAME) \
          --version=$(VERSION) \
          --debian-dir=deb \
          --build=$(BUILD_NUMBER) \
          --architecture=amd64 \
          --output-dir=dist
