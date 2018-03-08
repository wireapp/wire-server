SHELL         := /usr/bin/env bash
NAME          := cargohold
DIST          ?= dist
VERSION       ?=
BUILD_NUMBER  ?= 0
BUILD_LABEL   ?= local
BUILD         := $(BUILD_NUMBER)$(shell [ "${BUILD_LABEL}" == "" ] && echo "" || echo ".${BUILD_LABEL}")
EXE_IT        := $(DIST)/$(NAME)-integration
DEB           := $(DIST)/$(NAME)_$(VERSION)+$(BUILD)_amd64.deb
DEB_IT        := $(DIST)/$(NAME)-integration_$(VERSION)+$(BUILD)_amd64.deb
SDIST         := $(DIST)/$(NAME)-$(VERSION).tar.gz
KEIRETSU_ENV  ?= ../.env
EXECUTABLES   := $(NAME) $(NAME)-integration

guard-%:
	@ if [ "${${*}}" = "" ]; then \
	      echo "Environment variable $* not set"; \
	    exit 1; \
	fi

default: install

.PHONY: init
init:
	mkdir -p $(DIST)

.PHONY: install
install: init
	stack install --pedantic --test --local-bin-path=$(DIST)

.PHONY: compile
compile:
	stack build --pedantic --test --no-copy-bins

.PHONY: clean
clean: init
	stack clean
	-rm -rf $(DIST)

.PHONY: dist
dist: $(DIST)

$(DIST): guard-VERSION install $(DEB) $(DEB_IT)

$(DEB):
	makedeb --name=$(NAME) \
		--version=$(VERSION) \
		--debian-dir=deb \
		--build=$(BUILD) \
		--architecture=amd64 \
		--output-dir=$(DIST)

$(DEB_IT):
	makedeb --name=$(NAME)-integration \
		--version=$(VERSION) \
		--debian-dir=deb-it \
		--build=$(BUILD) \
		--architecture=amd64 \
		--output-dir=$(DIST)

.PHONY: integration
integration: install
	LOG_LEVEL=Info keiretsu --run $(EXE_IT) --env $(KEIRETSU_ENV)

.PHONY: docker
docker:
	$(foreach executable,$(EXECUTABLES),\
	docker build -t $(executable) \
	-f ../../build/alpine/Dockerfile \
	--build-arg service=$(NAME) \
	--build-arg executable=$(executable) \
	../.. \
	;)

.PHONY: time
time: clean
	-rm -f .stack-work/logs/*
	stack build --pedantic --no-run-benchmarks --no-copy-bins --ghc-options="-j +RTS -s -RTS"
	@echo -e "\nTotal wall-clock times taken to compile each module (see logs for more detail):"
	@grep Total .stack-work/logs/* | tr -s ' ' | awk -F' ' '{gsub(/s$$/, "", $$6); a[$$1] += $$6}END{for (i in a) {m=gensub(/^.*\/logs\//,"",1,i); m=gensub(/-[0-9].*\.log/,"",1,m); print m, a[i] ++ "s"}}' | sort -grk2
