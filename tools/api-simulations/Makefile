SHELL        := /usr/bin/env bash
NAME         := api-simulations
VERSION      := $(shell sed -n 's/^version: *\(.*\)$$/\1/p' $(NAME).cabal)
BUILD_NUMBER ?= 0
BUILD_LABEL  ?= local
BUILD        := $(BUILD_NUMBER)$(shell [ "${BUILD_LABEL}" == "" ] && echo "" || echo ".${BUILD_LABEL}")
DEB          := $(NAME)_$(VERSION)+$(BUILD)_amd64.deb

default: all

all: clean install

init:
	mkdir -p dist

.PHONY: clean
clean:
	stack clean
	-rm -rf dist
	-rm -f .metadata

.PHONY:
compile:
	stack build --pedantic --test --no-copy-bins

.PHONY: install
install: init
	stack install --pedantic --test --local-bin-path=dist

.PHONY: dist
dist: install $(DEB) .metadata

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

