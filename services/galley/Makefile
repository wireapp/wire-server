SHELL        := /usr/bin/env bash
NAME         := galley
VERSION      ?=
BUILD_NUMBER ?= 0
BUILD_LABEL  ?= local
BUILD        := $(BUILD_NUMBER)$(shell [ "${BUILD_LABEL}" == "" ] && echo "" || echo ".${BUILD_LABEL}")
EXE_IT       := dist/$(NAME)-integration
EXE_SCHEMA   := dist/$(NAME)-schema
DEB          := $(NAME)_$(VERSION)+$(BUILD)_amd64.deb
DEB_IT       := $(NAME)-integration_$(VERSION)+$(BUILD)_amd64.deb
DEB_SCHEMA   := $(NAME)-schema_$(VERSION)+$(BUILD)_amd64.deb
KEIRETSU_ENV ?= ../.env

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
	cp "$(shell stack path --dist-dir)/build/$(NAME)-integration/$(NAME)-integration" dist/

.PHONY:
compile:
	stack build --pedantic --test --no-copy-bins

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
	 --debian-dir=deb-schema \
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

.PHONY: db
db: db-reset

.PHONY: db-reset
db-reset: install
	$(EXE_SCHEMA) --keyspace $(NAME)_test --replication-factor 1 --reset

.PHONY: db-migrate
db-migrate: install
	$(EXE_SCHEMA) --keyspace $(NAME)_test --replication-factor 1

