SHELL         := /usr/bin/env bash
NAME          := galley
VERSION       ?=
BUILD_NUMBER  ?= 0
BUILD_LABEL   ?= local
BUILD         := $(BUILD_NUMBER)$(shell [ "${BUILD_LABEL}" == "" ] && echo "" || echo ".${BUILD_LABEL}")
EXE_IT        := ../../dist/$(NAME)-integration
EXE_SCHEMA    := ../../dist/$(NAME)-schema
DEB           := $(NAME)_$(VERSION)+$(BUILD)_amd64.deb
DEB_IT        := $(NAME)-integration_$(VERSION)+$(BUILD)_amd64.deb
DEB_SCHEMA    := $(NAME)-schema_$(VERSION)+$(BUILD)_amd64.deb
DEB_JOURNALER := $(NAME)-journaler_$(VERSION)+$(BUILD)_amd64.deb
EXECUTABLES   := $(NAME) $(NAME)-integration $(NAME)-schema $(NAME)-journaler

guard-%:
	@ if [ "${${*}}" = "" ]; then \
	      echo "Environment variable $* not set"; \
	    exit 1; \
	fi

default: fast

all: clean install

init:
	mkdir -p dist ../../dist

.PHONY: clean
clean:
	stack clean
	-rm -rf dist
	-rm -f .metadata

.PHONY: install
install: init
	stack install --pedantic --test --local-bin-path=../../dist

.PHONY: fast
fast: init
	stack install --pedantic --test --local-bin-path=../../dist --fast

.PHONY:
compile:
	stack build --pedantic --test --no-copy-bins

.PHONY: dist
dist: guard-VERSION install $(DEB) $(DEB_IT) $(DEB_SCHEMA) $(DEB_JOURNALER) .metadata

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
	 --debian-dir=deb-schema \
	 --build=$(BUILD) \
	 --architecture=amd64 \
	 --output-dir=dist

$(DEB_JOURNALER):
	makedeb --name=$(NAME)-journaler \
	 --version=$(VERSION) \
	 --debian-dir=deb-journaler \
	 --build=$(BUILD) \
	 --architecture=amd64 \
	 --output-dir=dist

.PHONY: i
i:
	../integration.sh $(EXE_IT) -s $(NAME).integration.yaml -i ../integration.yaml

.PHONY: i-list
i-list:
	$(EXE_IT) -s $(NAME).integration.yaml -i ../integration.yaml -l

i-%:
	../integration.sh $(EXE_IT) -s $(NAME).integration.yaml -i ../integration.yaml -p "$*"

.PHONY: integration
integration: fast i

.PHONY: integration-list
integration-list: fast i-list

integration-%: fast
	make i-"$*"

.metadata:
	echo -e "NAME=$(NAME)\nVERSION=$(VERSION)\nBUILD_NUMBER=$(BUILD)" > .metadata

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
