LANG := en_US.UTF-8
SHELL        := /usr/bin/env bash
NAME         := bonanza
# STACK_OPTIONS are the options used for most stack calls in this
# Makefile; WIRE_STACK_OPTIONS is the place for the user to override
# things.
STACK_OPTIONS := --pedantic --test --bench --no-run-benchmarks --local-bin-path=$(shell stack path --project-root)/dist
VERSION      ?=
BUILD_NUMBER ?= 0
DEB          := dist/$(NAME)_$(VERSION)+$(BUILD_NUMBER)_amd64.deb

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
	stack clean . $(STACK_OPTIONS)

.PHONY: install
install: init
	stack install . $(STACK_OPTIONS) $(WIRE_STACK_OPTIONS)

.PHONY: compile
compile:
	stack build . $(STACK_OPTIONS) --no-copy-bins --fast $(WIRE_STACK_OPTIONS)

.PHONY: dist
dist: guard-VERSION $(DEB)

$(DEB): install
	makedeb --name=$(NAME) \
          --version=$(VERSION) \
          --debian-dir=deb \
          --build=$(BUILD_NUMBER) \
          --architecture=amd64 \
          --output-dir=dist
