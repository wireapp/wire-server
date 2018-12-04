LANG := en_US.UTF-8
SHELL         := /usr/bin/env bash
NAME          := cargohold
VERSION       ?=
BUILD_NUMBER  ?= 0
BUILD_LABEL   ?= local
BUILD         := $(BUILD_NUMBER)$(shell [ "${BUILD_LABEL}" == "" ] && echo "" || echo ".${BUILD_LABEL}")
EXE_IT        := ../../dist/$(NAME)-integration
DEB           := dist/$(NAME)_$(VERSION)+$(BUILD)_amd64.deb
DEB_IT        := dist/$(NAME)-integration_$(VERSION)+$(BUILD)_amd64.deb
SDIST         := dist/$(NAME)-$(VERSION).tar.gz
EXECUTABLES   := $(NAME) $(NAME)-integration

guard-%:
	@ if [ "${${*}}" = "" ]; then \
	      echo "Environment variable $* not set"; \
	    exit 1; \
	fi

default: fast

.PHONY: init
init:
	mkdir -p dist ../../dist

.PHONY: install
install: init
	stack install . --pedantic --test --bench --no-run-benchmarks --local-bin-path=dist

.PHONY: fast
fast: init
	stack install . --pedantic --test --bench --no-run-benchmarks --local-bin-path=dist --fast $(WIRE_STACK_OPTIONS)

.PHONY: compile
compile:
	stack build . --pedantic --test --bench --no-run-benchmarks --no-copy-bins

.PHONY: clean
clean: init
	stack clean cargohold
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

.PHONY: i
i:
	../integration.sh $(EXE_IT) -s $(NAME).integration.yaml -i ../integration.yaml

.PHONY: i-aws
i-aws:
	INTEGRATION_USE_REAL_AWS=1 ../integration.sh $(EXE_IT) -s $(NAME).integration-aws.yaml -i ../integration.yaml

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
	make "i-$*"

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

.PHONY: time
time: clean
	-rm -f .stack-work/logs/*
	stack build . --pedantic --no-run-benchmarks --no-copy-bins --ghc-options="-j +RTS -s -RTS"
	@echo -e "\nTotal wall-clock times taken to compile each module (see logs for more detail):"
	@grep Total .stack-work/logs/* | tr -s ' ' | awk -F' ' '{gsub(/s$$/, "", $$6); a[$$1] += $$6}END{for (i in a) {m=gensub(/^.*\/logs\//,"",1,i); m=gensub(/-[0-9].*\.log/,"",1,m); print m, a[i] ++ "s"}}' | sort -grk2
