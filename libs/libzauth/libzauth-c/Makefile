LANG := en_US.UTF-8
SHELL   := /usr/bin/env bash
VERSION := "3.0.0"
ARCH    := $(shell if [ -f "`which dpkg-architecture`" ]; then dpkg-architecture -qDEB_HOST_ARCH; else [ -f "`which dpkg`" ] && dpkg --print-architecture; fi )
BUILD   ?= 1
OS      := $(shell uname -s | tr '[:upper:]' '[:lower:]')
# If we can install libzauth globally, we'll install it there, otherwise
# it'll go into ~/.wire-dev (unless it's overridden)
PREFIX  ?= $(shell [ -w /usr/local ] && echo /usr/local || echo "$(HOME)/.wire-dev")

ifeq ($(OS), darwin)
LIB_TYPE := dylib
else
LIB_TYPE := so
endif

all: build

clean:
	cargo clean
	rm -rf test/target
	rm -rf deb$(PREFIX)/

build:
	cargo build

build-release:
	cargo build --release

install: build-release
	mkdir -p $(PREFIX)/include
	mkdir -p $(PREFIX)/lib/pkgconfig
	cp src/zauth.h $(PREFIX)/include/
	sed -e "s~<<VERSION>>~$(VERSION)~" \
		-e "s~<<PREFIX>>~$(PREFIX)~" \
		src/libzauth.pc > $(PREFIX)/lib/pkgconfig/libzauth.pc
	cp target/release/libzauth.$(LIB_TYPE) $(PREFIX)/lib/

uninstall:
	rm -f $(PREFIX)/include/zauth.h
	rm -f $(PREFIX)/lib/libzauth.$(LIB_TYPE)
	rm -f $(PREFIX)/lib/pkgconfig/libzauth.pc

dist: build-release
	mkdir -p deb$(PREFIX)/include
	mkdir -p deb$(PREFIX)/lib/pkgconfig
	cp src/zauth.h deb$(PREFIX)/include/
	sed -e "s~<<VERSION>>~$(VERSION)~" \
		-e "s~<<PREFIX>>~$(PREFIX)~" \
		src/libzauth.pc > deb$(PREFIX)/lib/pkgconfig/libzauth.pc
	cp target/release/libzauth.$(LIB_TYPE) deb$(PREFIX)/lib/
ifeq ($(OS), linux)
	makedeb --name=libzauth        \
			--version=$(VERSION)   \
			--debian-dir=deb       \
			--build=$(BUILD)       \
			--architecture=$(ARCH) \
			--output-dir=target/release
endif

