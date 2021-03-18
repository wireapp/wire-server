LANG := en_US.UTF-8
SHELL             := /usr/bin/env bash
NAME              := spar
VERSION           ?=
BUILD_NUMBER      ?= 0
BUILD_LABEL       ?= local
BUILD             := $(BUILD_NUMBER)$(shell [ "${BUILD_LABEL}" == "" ] && echo "" || echo ".${BUILD_LABEL}")
EXE_IT            := ../../dist/$(NAME)-integration
EXE_SCHEMA        := ../../dist/$(NAME)-schema
DEB               := dist/$(NAME)_$(VERSION)+$(BUILD)_amd64.deb
DEB_SCHEMA        := dist/$(NAME)-schema_$(VERSION)+$(BUILD)_amd64.deb
DEB_MIGRATE_DATA  := dist/$(NAME)-migrate-data_$(VERSION)+$(BUILD)_amd64.deb
EXECUTABLES       := $(NAME) $(NAME)-integration $(NAME)-schema
DOCKER_USER        ?= quay.io/wire
DOCKER_TAG         ?= local

guard-%:
	@ if [ "${${*}}" = "" ]; then \
	      echo "Environment variable $* not set"; \
	    exit 1; \
	fi

default: fast

init:
	mkdir -p ../../dist

.PHONY: install
install: init
	stack install --pedantic --test --bench --no-run-benchmarks --local-bin-path=dist spar

.PHONY: fast
fast: init
	stack install --pedantic --test --bench --no-run-benchmarks --local-bin-path=dist --fast $(WIRE_STACK_OPTIONS) spar

.PHONY: clean
clean:
	stack clean $(NAME)
	rm -f .metadata

.PHONY: compile
compile:
	stack build --fast --test --bench --no-run-benchmarks --no-copy-bins spar

.PHONY: dist
dist: guard-VERSION install $(DEB) $(DEB_SCHEMA) $(DEB_MIGRATE_DATA) .metadata

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

$(DEB_SCHEMA): install
	makedeb --name=$(NAME)-schema \
		--version=$(VERSION) \
		--debian-dir=schema/deb \
		--build=$(BUILD) \
		--architecture=amd64 \
		--output-dir=dist

$(DEB_MIGRATE_DATA): install
	makedeb --name=$(NAME)-migrate-data \
		--version=$(VERSION) \
		--debian-dir=migrate-data/deb \
		--build=$(BUILD) \
		--architecture=amd64 \
		--output-dir=dist

.PHONY: i
i:
	../integration.sh bash -c "\
	./test-scim-suite/runsuite.sh && \
	$(EXE_IT) -s $(NAME).integration.yaml -i ../integration.yaml $(WIRE_INTEGRATION_TEST_OPTIONS) \
	"

.PHONY: i-aws
i-aws:
	INTEGRATION_USE_REAL_AWS=1 ../integration.sh $(EXE_IT) -s $(NAME).integration-aws.yaml -i ../integration.yaml $(WIRE_INTEGRATION_TEST_OPTIONS)

.PHONY: i-list
i-list:
	$(EXE_IT) -s $(NAME).integration.yaml -i ../integration.yaml -l

i-%:
	INTEGRATION_SKIP_SCIM_SUITE=1 ../integration.sh $(EXE_IT) -s $(NAME).integration.yaml -i ../integration.yaml --match="$*" $(WIRE_INTEGRATION_TEST_OPTIONS)

.PHONY: integration
integration: fast i

.PHONY: integration-list
integration-list: fast i-list

integration-%: fast
	make "i-$*"

.PHONY: db
db: db-reset

.PHONY: db-reset
db-reset: fast
	$(EXE_SCHEMA) --keyspace $(NAME)_test --replication-factor 1 --reset

.PHONY: db-migrate
db-migrate: fast
	$(EXE_SCHEMA) --keyspace $(NAME)_test --replication-factor 1

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
	-rm -f .stack-work/logs/*
	stack build --pedantic --no-run-benchmarks --no-copy-bins --ghc-options="-j +RTS -s -RTS"
	@echo -e "\nTotal wall-clock times taken to compile each module (see logs for more detail):"
	@grep Total .stack-work/logs/* | tr -s ' ' | awk -F' ' '{gsub(/s$$/, "", $$6); a[$$1] += $$6}END{for (i in a) {m=gensub(/^.*\/logs\//,"",1,i); m=gensub(/-[0-9].*\.log/,"",1,m); print m, a[i] ++ "s"}}' | sort -grk2
