LANG := en_US.UTF-8
SHELL        := /usr/bin/env bash
NAME         := makedeb
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

default: all

all: clean install

init:
	mkdir -p dist
	mkdir -p ../../dist

.PHONY: clean
clean:
	stack clean makedeb
	-rm -rf dist
	-rm -f .metadata

.PHONY: install
install: init
	stack install . --pedantic --test --bench --no-run-benchmarks --local-bin-path=dist

.PHONY:
compile:
	stack build . --pedantic --test --bench --no-run-benchmarks --no-copy-bins

.PHONY: dist
dist: guard-VERSION install $(DEB) .metadata

$(DEB):
	$(eval $@_DIR := $(shell mktemp -d -t makedeb.XXXXXXXXXX))
	cp -R -L deb $($@_DIR)
	sed -i "s/<<VERSION_NUMBER>>/$(VERSION)/g" $($@_DIR)/deb/DEBIAN/control
	sed -i "s/<<BUILD_NUMBER>>/$(BUILD)/g" $($@_DIR)/deb/DEBIAN/control
	sed -i "s/<<ARCHITECTURE>>/amd64/g" $($@_DIR)/deb/DEBIAN/control
	dpkg-deb -b $($@_DIR)/deb ../../dist/$(DEB)
	rm -rf $($@_DIR)

.metadata:
	echo -e "NAME=$(NAME)\nVERSION=$(VERSION)\nBUILD_NUMBER=$(BUILD)" > .metadata
