SHELL        := /usr/bin/env bash
NAME         := cannon
VERSION      ?=
BUILD_NUMBER ?= 0
BUILD_LABEL  ?= local
BUILD        := $(BUILD_NUMBER)$(shell [ "${BUILD_LABEL}" == "" ] && echo "" || echo ".${BUILD_LABEL}")
DEB          := $(NAME)_$(VERSION)+$(BUILD)_amd64.deb
EXECUTABLES  := $(NAME)
EXE_IT		 := dist/$(NAME)

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
dist: guard-VERSION install $(DEB) .metadata

$(DEB):
	makedeb --name=$(NAME) \
	 --version=$(VERSION) \
	 --debian-dir=deb \
	 --build=$(BUILD) \
	 --architecture=amd64 \
	 --output-dir=dist

.PHONY: integration
integration: install
	keiretsu --run $(EXE_IT)

integration-list:
	keiretsu --run '$(EXE_IT) -l'

integration-%:
	keiretsu --run '$(EXE_IT) --pattern="$*"'

.metadata:
	echo -e "NAME=$(NAME)\nVERSION=$(VERSION)\nBUILD_NUMBER=$(BUILD)" > .metadata

.PHONY: docker
docker:
	$(foreach executable,$(EXECUTABLES),\
	docker build -t $(executable) \
	-f ../../build/alpine/Dockerfile \
	--build-arg service=$(NAME) \
	--build-arg executable=$(executable) \
	../.. \
	;)
