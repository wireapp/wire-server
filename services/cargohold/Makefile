SHELL         := /usr/bin/env bash
NAME          := cargohold
VERSION       ?=
BUILD_NUMBER  ?= 0
BUILD_LABEL   ?= local
BUILD         := $(BUILD_NUMBER)$(shell [ "${BUILD_LABEL}" == "" ] && echo "" || echo ".${BUILD_LABEL}")
EXE_IT        := dist/$(NAME)-integration
DEB           := dist/$(NAME)_$(VERSION)+$(BUILD)_amd64.deb
DEB_IT        := dist/$(NAME)-integration_$(VERSION)+$(BUILD)_amd64.deb
SDIST         := dist/$(NAME)-$(VERSION).tar.gz
KEIRETSU_ENV  ?= ../.env
EXECUTABLES   := $(NAME) $(NAME)-integration

guard-%:
	@ if [ "${${*}}" = "" ]; then \
	      echo "Environment variable $* not set"; \
	    exit 1; \
	fi

default: clean install

.PHONY: init
init:
	mkdir -p dist

.PHONY: install
install: init
	stack install --pedantic --test --local-bin-path=dist

.PHONY: compile
compile:
	stack build --pedantic --test --no-copy-bins

.PHONY: clean
clean: init
	stack clean
	-rm -rf dist
	-rm -f .metadata

.PHONY: dist
dist: guard-VERSION install $(DEB) $(DEB_IT) .metadata

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

.PHONY: docker
docker:
	$(foreach executable,$(EXECUTABLES),\
	docker build -t $(executable) \
	-f ../../build/alpine/Dockerfile \
	--build-arg service=$(NAME) \
	--build-arg executable=$(executable) \
	../.. \
	;)
