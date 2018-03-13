SHELL         := /usr/bin/env bash
NAME          := gundeck
DIST          ?= ./dist
VERSION       ?=
BUILD_NUMBER  ?= 0
BUILD_LABEL   ?= local
BUILD         := $(BUILD_NUMBER)$(shell [ "${BUILD_LABEL}" == "" ] && echo "" || echo ".${BUILD_LABEL}")
EXE_IT        := $(DIST)/$(NAME)-integration
EXE_TESTS     := dist/$(NAME)-tests
EXE_BENCH     := dist/$(NAME)-bench
EXE_SCHEMA    := $(DIST)/$(NAME)-schema
DEB           := $(NAME)_$(VERSION)+$(BUILD)_amd64.deb
DEB_IT        := $(NAME)-integration_$(VERSION)+$(BUILD)_amd64.deb
DEB_SCHEMA    := $(NAME)-schema_$(VERSION)+$(BUILD)_amd64.deb
SDIST         := dist/$(NAME)-$(VERSION).tar.gz
KEIRETSU_ENV  ?= ../.integration.env
EXECUTABLES   := $(NAME) $(NAME)-integration $(NAME)-schema

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
	stack install --pedantic --test --bench --no-run-benchmarks --local-bin-path=dist
	cp "$(shell stack path --dist-dir)/build/$(NAME)-tests/$(NAME)-tests" dist/
	cp "$(shell stack path --dist-dir)/build/$(NAME)-bench/$(NAME)-bench" dist/

.PHONY: compile
compile:
	stack build --pedantic --test --bench --no-run-benchmarks --no-copy-bins

.PHONY: clean
clean:
	stack clean
	-rm -rf dist
	-rm -f .metadata

.PHONY: dist
dist: guard-VERSION install $(DEB) $(DEB_IT) $(DEB_SCHEMA) .metadata

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

$(DEB_SCHEMA):
	makedeb --name=$(NAME)-schema \
		--version=$(VERSION) \
		--debian-dir=schema/deb \
		--build=$(BUILD) \
		--architecture=amd64 \
		--output-dir=dist

.metadata:
	echo -e "NAME=$(NAME)\nVERSION=$(VERSION)\nBUILD_NUMBER=$(BUILD)" > .metadata

.PHONY: test
test: install
	$(EXE_TESTS)

.PHONY: bench
bench: install
	mkdir -p dist/bench
	$(EXE_BENCH) --output dist/bench/result.html

.PHONY: integration
integration: install
	../integrationtest.sh $(EXE_IT) -s $(NAME).integration.yaml -i ../integration.yaml.example

.PHONY: integration-list
integration-list: install
	../integrationtest.sh $(EXE_IT) -s $(NAME).integration.yaml -i ../integration.yaml.example -l

integration-%: install
	../integrationtest.sh $(EXE_IT) -s $(NAME).integration.yaml -i ../integration.yaml.example -p "$*"

integration-fake-aws: install
	../integrationtest.sh $(EXE_IT) -s $(NAME).integration.yaml -i ../integration.yaml.example -p "!/RealAWS/"

.PHONY: db
db: db-reset

.PHONY: db-reset
db-reset: install
	$(EXE_SCHEMA) --keyspace $(NAME)_test --replication-factor 1 --reset

.PHONY: db-migrate
db-migrate: install
	$(EXE_SCHEMA) --keyspace $(NAME)_test --replication-factor 1

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
