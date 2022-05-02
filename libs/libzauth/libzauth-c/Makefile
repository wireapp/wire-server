LANG := en_US.UTF-8
SHELL   := /usr/bin/env bash
VERSION := "3.0.0"
ARCH    := $(shell if [ -f "`which dpkg-architecture`" ]; then dpkg-architecture -qDEB_HOST_ARCH; else [ -f "`which dpkg`" ] && dpkg --print-architecture; fi )
BUILD   ?= 1
OS      := $(shell uname -s | tr '[:upper:]' '[:lower:]')
# If we can install libzauth globally, we'll install it there, otherwise
# it'll go into ~/.wire-dev (unless it's overridden)
PREFIX_INSTALL  ?= $(shell [ -w /usr/local ] && echo /usr/local || echo "$(HOME)/.wire-dev")
# If we are building a debian package, just use /usr/local.
PREFIX_PACKAGE  ?= /usr/local

ifeq ($(OS), darwin)
LIB_TYPE := dylib
else
LIB_TYPE := so
endif

all: build

clean:
	cargo clean
	rm -rf test/target
	rm -rf deb$(PREFIX_PACKAGE)/

build:
	cargo build

build-release:
	cargo build --release

install: build-release
	mkdir -p $(PREFIX_INSTALL)/include
	mkdir -p $(PREFIX_INSTALL)/lib/pkgconfig
	cp src/zauth.h $(PREFIX_INSTALL)/include/
	sed -e "s~<<VERSION>>~$(VERSION)~" \
		-e "s~<<PREFIX>>~$(PREFIX_INSTALL)~" \
		src/libzauth.pc > $(PREFIX_INSTALL)/lib/pkgconfig/libzauth.pc
	cp target/release/libzauth.$(LIB_TYPE) $(PREFIX_INSTALL)/lib/

uninstall:
	rm -f $(PREFIX_INSTALL)/include/zauth.h
	rm -f $(PREFIX_INSTALL)/lib/libzauth.$(LIB_TYPE)
	rm -f $(PREFIX_INSTALL)/lib/pkgconfig/libzauth.pc

.PHONY: test
test:
	cd ../libzauth && cargo test

dist: test build-release
	mkdir -p deb$(PREFIX_PACKAGE)/include
	mkdir -p deb$(PREFIX_PACKAGE)/lib/pkgconfig
	cp src/zauth.h deb$(PREFIX_PACKAGE)/include/
	sed -e "s~<<VERSION>>~$(VERSION)~" \
		-e "s~<<PREFIX>>~$(PREFIX_PACKAGE)~" \
		src/libzauth.pc > deb$(PREFIX_PACKAGE)/lib/pkgconfig/libzauth.pc
	cp target/release/libzauth.$(LIB_TYPE) deb$(PREFIX_PACKAGE)/lib/
ifeq ($(OS), linux)
	makedeb --name=libzauth        \
			--version=$(VERSION)   \
			--debian-dir=deb       \
			--build=$(BUILD)       \
			--architecture=$(ARCH) \
			--output-dir=target/release
endif

