SHELL         := /usr/bin/env bash
NAME          := galley
VERSION       ?=
BUILD_NUMBER  ?= 0
BUILD_LABEL   ?= local
BUILD         := $(BUILD_NUMBER)$(shell [ "${BUILD_LABEL}" == "" ] && echo "" || echo ".${BUILD_LABEL}")
EXE_IT        := dist/$(NAME)-integration
EXE_SCHEMA    := dist/$(NAME)-schema
DEB           := $(NAME)_$(VERSION)+$(BUILD)_amd64.deb
DEB_IT        := $(NAME)-integration_$(VERSION)+$(BUILD)_amd64.deb
DEB_SCHEMA    := $(NAME)-schema_$(VERSION)+$(BUILD)_amd64.deb
DEB_JOURNALER := $(NAME)-journaler_$(VERSION)+$(BUILD)_amd64.deb
KEIRETSU_ENV  ?= ../.env
EXECUTABLES   := $(NAME) $(NAME)-integration $(NAME)-schema $(NAME)-journaler

guard-%:
	@ if [ "${${*}}" = "" ]; then \
	      echo "Environment variable $* not set"; \
	    exit 1; \
	fi

default: all

all: clean install

init:
	mkdir -p dist

.PHONY: clean
clean:
	stack clean
	-rm -rf dist
	-rm -f .metadata

.PHONY: install
install: init
	stack install --pedantic --test --local-bin-path=dist

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

.PHONY: integration
integration: install
	keiretsu --run $(EXE_IT) --env $(KEIRETSU_ENV)

integration-list:
	keiretsu --run '$(EXE_IT) -l' --env $(KEIRETSU_ENV)

integration-%:
	keiretsu --run '$(EXE_IT) --pattern="$*"' --env $(KEIRETSU_ENV)

.metadata:
	echo -e "NAME=$(NAME)\nVERSION=$(VERSION)\nBUILD_NUMBER=$(BUILD)" > .metadata

.PHONY: fast
fast:
	stack install --pedantic --local-bin-path=dist --fast

.PHONY: test
test: fast
	keiretsu --run $(EXE_IT) --env $(KEIRETSU_ENV)

test-%: fast
	keiretsu --run '$(EXE_IT) --pattern="$*"' --env $(KEIRETSU_ENV)

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
