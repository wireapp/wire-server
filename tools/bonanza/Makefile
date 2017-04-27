SHELL        := /usr/bin/env bash
NAME         := bonanza
VERSION      := $(shell sed -n 's/^version: *\(.*\)$$/\1/p' $(NAME).cabal)
BUILD_NUMBER ?= 0
DEB          := dist/$(NAME)_$(VERSION)+$(BUILD_NUMBER)_amd64.deb
SDIST        := dist/$(NAME)-$(VERSION).tar.gz


default: all

all: clean install

init:
	mkdir -p dist/build

.PHONY: clean
clean:
	stack clean
	-rm -rf dist

.PHONY: install
install: init
	stack install --pedantic --test --local-bin-path=dist/build

.PHONY: compile
compile:
	stack build --fast --pedantic --test --no-copy-bins

.PHONY: dist
dist: $(DEB) $(SDIST)

$(SDIST): init
	stack sdist
	cp "$(shell stack path --dist-dir)/$(NAME)-$(VERSION).tar.gz" dist/

$(DEB): install
	makedeb --name=$(NAME) \
          --version=$(VERSION) \
          --debian-dir=deb \
          --build=$(BUILD_NUMBER) \
          --architecture=amd64 \
          --output-dir=dist
