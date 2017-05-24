SHELL         := /usr/bin/env bash
NAME          := cargohold
VERSION       := $(shell sed -n 's/^version: *\(.*\)$$/\1/p' $(NAME).cabal)
BUILD_NUMBER  ?= 0
BUILD_LABEL   ?= local
BUILD         := $(BUILD_NUMBER)$(shell [ "${BUILD_LABEL}" == "" ] && echo "" || echo ".${BUILD_LABEL}")
EXE_IT        := dist/$(NAME)-integration
DEB           := dist/$(NAME)_$(VERSION)+$(BUILD)_amd64.deb
DEB_IT        := dist/$(NAME)-integration_$(VERSION)+$(BUILD)_amd64.deb
SDIST         := dist/$(NAME)-$(VERSION).tar.gz
KEIRETSU_ENV  ?= ../.env

default: clean install

.PHONY: init
init:
	mkdir -p dist

.PHONY: install
install: init
	stack install --pedantic --test --local-bin-path=dist
	cp "$(shell stack path --dist-dir)/build/$(NAME)-integration/$(NAME)-integration" dist/

.PHONY: compile
compile:
	stack build --pedantic --test --no-copy-bins

.PHONY: clean
clean: init
	stack clean
	-rm -rf dist
	-rm -f .metadata

.PHONY: dist
dist: install $(DEB) $(DEB_IT) .metadata

$(DEB):
	makedeb --name=$(NAME) \
		--version=$(VERSION) \
		--debian-dir=deb \
		--build=$(BUILD) \
		--architecture=amd64 \
		--output-dir=dist

$(DEB_IT):
	makedeb --name=$(NAME)-integration \
		--version=$(VERSION) \
		--debian-dir=deb-it \
		--build=$(BUILD) \
		--architecture=amd64 \
		--output-dir=dist

.PHONY: integration
integration: install
	LOG_LEVEL=Info keiretsu --run $(EXE_IT) --env $(KEIRETSU_ENV)

.metadata:
	echo -e "NAME=$(NAME)\nVERSION=$(VERSION)\nBUILD_NUMBER=$(BUILD)" \
	 > .metadata
