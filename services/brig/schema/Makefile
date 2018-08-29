LANG := en_US.UTF-8
SHELL        := /usr/bin/env bash
NAME         := brig-schema
VERSION       = $(shell sed -n 's/^version: *\(.*\)$$/\1/p' ../brig.cabal)
BUILD_NUMBER ?= 0
BUILD_LABEL  ?= local
BUILD        := $(BUILD_NUMBER)$(shell [ "${BUILD_LABEL}" == "" ] && echo "" || echo ".${BUILD_LABEL}")

DEB := $(NAME)_$(VERSION)+$(BUILD)_amd64.deb

$(DEB):
	makedeb --name=$(NAME) \
		--version=$(VERSION) \
		--debian-dir=deb \
		--build=$(BUILD) \
		--architecture=amd64 \
		--output-dir=.

.PHONY: clean
clean:
	rm -f $(DEB)
