SHELL         := /usr/bin/env bash
NAME          := cannon
VERSION       := $(shell sed -n 's/^version: *\(.*\)$$/\1/p' $(NAME).cabal)
BUILD_NUMBER  ?= 0
BUILD_LABEL   ?= local
BUILD         := $(BUILD_NUMBER)$(shell [ "${BUILD_LABEL}" == "" ] && echo "" || echo ".${BUILD_LABEL}")

default: clean install

.PHONY: clean
clean:
	stack clean

.PHONY: install
install:
	stack install --pedantic --test --local-bin-path=dist

.PHONY:
compile:
	stack build --pedantic --test --no-copy-bins

.PHONY: sdist
sdist:
	stack sdist

.PHONY: dist
dist: install
	makedeb --name=$(NAME) \
	 --version=$(VERSION) \
	 --debian-dir=deb \
	 --build=$(BUILD) \
	 --architecture=amd64 \
	 --output-dir=dist
