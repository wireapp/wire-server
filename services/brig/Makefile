SHELL         := /usr/bin/env bash
NAME          := brig
DIST          ?= ./dist
VERSION       ?=
BUILD_NUMBER  ?= 0
BUILD_LABEL   ?= local
BUILD         := $(BUILD_NUMBER)$(shell [ "${BUILD_LABEL}" == "" ] && echo "" || echo ".${BUILD_LABEL}")
EXE_IT        := $(DIST)/$(NAME)-integration
EXE_SCHEMA    := $(DIST)/$(NAME)-schema
EXE_INDEX     := dist/$(NAME)-index
DEB           := dist/$(NAME)_$(VERSION)+$(BUILD)_amd64.deb
DEB_IT        := dist/$(NAME)-integration_$(VERSION)+$(BUILD)_amd64.deb
DEB_SCHEMA    := dist/$(NAME)-schema_$(VERSION)+$(BUILD)_amd64.deb
DEB_INDEX     := dist/$(NAME)-index_$(VERSION)+$(BUILD)_amd64.deb
KEIRETSU_ENV  ?= ../.integration.env
EXECUTABLES   := $(NAME) $(NAME)-integration $(NAME)-schema $(NAME)-index

guard-%:
	@ if [ "${${*}}" = "" ]; then \
	      echo "Environment variable $* not set"; \
	    exit 1; \
	fi

default: clean install

init:
	mkdir -p dist

.PHONY: install
install: init
	stack install --pedantic --test --local-bin-path=dist

.PHONY: clean
clean:
	stack clean
	-rm -rf dist
	-rm -f .metadata

.PHONY: compile
compile:
	stack build --fast --test --no-copy-bins

.PHONY: dist
dist: guard-VERSION install $(DEB) $(DEB_IT) $(DEB_SCHEMA) $(DEB_INDEX) .metadata

.metadata:
	echo -e "NAME=$(NAME)\nVERSION=$(VERSION)\nBUILD_NUMBER=$(BUILD)" \
	 > .metadata

$(DEB): install
	makedeb --name=$(NAME) \
		--version=$(VERSION) \
		--debian-dir=deb \
		--build=$(BUILD) \
		--architecture=amd64 \
		--output-dir=dist

$(DEB_IT): install
	makedeb --name=$(NAME)-integration \
		--version=$(VERSION) \
		--debian-dir=deb-it \
		--build=$(BUILD) \
		--architecture=amd64 \
		--output-dir=dist

$(DEB_SCHEMA): install
	makedeb --name=$(NAME)-schema \
		--version=$(VERSION) \
		--debian-dir=schema/deb \
		--build=$(BUILD) \
		--architecture=amd64 \
		--output-dir=dist

$(DEB_INDEX): install
	makedeb --name=$(NAME)-index \
		--version=$(VERSION) \
		--debian-dir=index/deb \
		--build=$(BUILD) \
		--architecture=amd64 \
		--output-dir=dist

.PHONY: integration
integration: db-migrate index-reset
	LOG_LEVEL=Info keiretsu --run '$(EXE_IT)' --delay 3000 --env $(KEIRETSU_ENV)

.PHONY: integration-list
integration-list: install
	LOG_LEVEL=Info keiretsu --run '$(EXE_IT) -l; sleep 1' --env $(KEIRETSU_ENV)

integration-%: db-migrate index-reset
	LOG_LEVEL=Info keiretsu --run '$(EXE_IT) --pattern="$*"' --env $(KEIRETSU_ENV)


.PHONY: fast
fast:
	stack install --fast --local-bin-path=dist

.PHONY: test
test: fast
	LOG_LEVEL=Info keiretsu --run '$(EXE_IT)' --env $(KEIRETSU_ENV) --delay 3000

test-%: fast
	LOG_LEVEL=Info keiretsu --run '$(EXE_IT) --pattern="$*"' --env $(KEIRETSU_ENV) --delay 3000


.PHONY: db
db: db-reset

.PHONY: db-reset
db-reset: install
	$(EXE_SCHEMA) --keyspace $(NAME)_test --replication-factor 1 --reset

.PHONY: db-migrate
db-migrate: install
	$(EXE_SCHEMA) --keyspace $(NAME)_test --replication-factor 1

.PHONY: index
index: index-reset

.PHONY: index-reset
index-reset: install
	$(EXE_INDEX) reset

.PHONY: docker
docker:
	$(foreach executable,$(EXECUTABLES),\
	docker build -t $(executable) \
	-f Dockerfile \
	--build-arg executable=$(executable) \
	../.. \
	;)

.PHONY: time
time: clean
	-rm -f .stack-work/logs/*
	stack build --pedantic --no-run-benchmarks --no-copy-bins --ghc-options="-j +RTS -s -RTS"
	@echo -e "\nTotal wall-clock times taken to compile each module (see logs for more detail):"
	@grep Total .stack-work/logs/* | tr -s ' ' | awk -F' ' '{gsub(/s$$/, "", $$6); a[$$1] += $$6}END{for (i in a) {m=gensub(/^.*\/logs\//,"",1,i); m=gensub(/-[0-9].*\.log/,"",1,m); print m, a[i] ++ "s"}}' | sort -grk2
