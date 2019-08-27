LANG         := en_US.UTF-8
SHELL        := /usr/bin/env bash
NAME         := stern
# STACK_OPTIONS are the options used for most stack calls in this
# Makefile; WIRE_STACK_OPTIONS is the place for the user to override
# things.
STACK_OPTIONS := --pedantic --test --bench --no-run-benchmarks --local-bin-path=$(shell stack path --project-root)/dist
VERSION      ?=
BUILD_NUMBER ?= 0
BUILD_LABEL  ?= local
BUILD        := $(BUILD_NUMBER)$(shell [ "${BUILD_LABEL}" == "" ] && echo "" || echo ".${BUILD_LABEL}")
DEB          := $(NAME)_$(VERSION)+$(BUILD)_amd64.deb
EXECUTABLES  := stern
DOCKER_USER  ?= quay.io/wire
DOCKER_TAG   ?= local

guard-%:
	@ if [ "${${*}}" = "" ]; then \
	      echo "Environment variable $* not set"; \
	    exit 1; \
	fi

default: fast

all: clean install

init:
	mkdir -p ../../dist

.PHONY: clean
clean:
	stack clean . $(STACK_OPTIONS)
	rm -f .metadata

.PHONY: install
install: init
	stack install . $(STACK_OPTIONS) $(WIRE_STACK_OPTIONS)

.PHONY: fast
fast: init
	stack install . $(STACK_OPTIONS) --fast $(WIRE_STACK_OPTIONS)

.PHONY:
compile:
	stack build . $(STACK_OPTIONS) --no-copy-bins $(WIRE_STACK_OPTIONS)

.PHONY: dist
dist: guard-VERSION install $(DEB) $(DEB_IT) .metadata

$(DEB):
	makedeb --name=$(NAME) \
	 --version=$(VERSION) \
	 --debian-dir=deb \
	 --build=$(BUILD) \
	 --architecture=amd64 \
	 --output-dir=dist

.metadata:
	echo -e "NAME=$(NAME)\nVERSION=$(VERSION)\nBUILD_NUMBER=$(BUILD)" > .metadata

.PHONY: docker
docker:
	$(foreach executable,$(EXECUTABLES),\
	docker build -t $(DOCKER_USER)/$(executable):$(DOCKER_TAG) \
	-f ../../build/alpine/Dockerfile.executable \
	--build-arg executable=$(executable) \
	../.. && \
	docker tag $(DOCKER_USER)/$(executable):$(DOCKER_TAG) $(DOCKER_USER)/$(executable):latest && \
	if test -n "$$DOCKER_PUSH"; then docker login -u $(DOCKER_USERNAME) -p $(DOCKER_PASSWORD); docker push $(DOCKER_USER)/$(executable):$(DOCKER_TAG); docker push $(DOCKER_USER)/$(executable):latest; fi \
	;)

.PHONY: time
time: clean
	rm -f $(shell stack path --project-root)/.stack-work/logs/*
	stack build . $(STACK_OPTIONS) --no-copy-bins --ghc-options="-j +RTS -s -RTS"
	@echo -e "\nTotal wall-clock times taken to compile each module (see logs for more detail):"
	@grep Total .stack-work/logs/* | tr -s ' ' | awk -F' ' '{gsub(/s$$/, "", $$6); a[$$1] += $$6}END{for (i in a) {m=gensub(/^.*\/logs\//,"",1,i); m=gensub(/-[0-9].*\.log/,"",1,m); print m, a[i] ++ "s"}}' | sort -grk2
