SHELL            := /usr/bin/env bash
LANG             := en_US.UTF-8
DOCKER_USER      ?= quay.io/wire
DOCKER_TAG       ?= local

default: fast

init:
	mkdir -p dist

# Build all Haskell services and executables, run unit tests
.PHONY: install
install: init
	stack install --pedantic --test --bench --no-run-benchmarks --local-bin-path=dist

# Build all Haskell services and executables with -O0, run unit tests
.PHONY: fast
fast: init
	stack install --pedantic --test --bench --no-run-benchmarks --local-bin-path=dist --fast $(WIRE_STACK_OPTIONS)

# Build everything (Haskell services and nginz)
.PHONY: services
services: init install
	$(MAKE) -C services/nginz

# Build haddocks
.PHONY: haddock
haddock:
	WIRE_STACK_OPTIONS="--haddock --haddock-internal" make fast

# Build haddocks only for wire-server
.PHONY: haddock-shallow
haddock-shallow:
	WIRE_STACK_OPTIONS="--haddock --haddock-internal --no-haddock-deps" make fast

# Clean
.PHONY: clean
clean:
	stack clean
	$(MAKE) -C services/nginz clean
	-rm -rf dist
	-rm -f .metadata

#################################
## running integration tests

# Build services with --fast and run tests
.PHONY: integration
integration: fast i

# Run tests without building services
.PHONY: i
i:
	$(MAKE) -C services/cargohold i
	$(MAKE) -C services/galley i
	$(MAKE) -C services/brig i
	$(MAKE) -C services/gundeck i
	$(MAKE) -C services/spar i

# Build services and run tests using AWS
.PHONY: integration-aws
integration-aws: fast i-aws

# Run tests using AWS
.PHONY: i-aws
i-aws:
	$(MAKE) -C services/cargohold i-aws
	$(MAKE) -C services/galley i-aws
	$(MAKE) -C services/brig i-aws
	$(MAKE) -C services/gundeck i-aws
	$(MAKE) -C services/spar i-aws

# Build services and run tests of one service using AWS
.PHONY: integration-aws-%
integration-aws-%: fast
	$(MAKE) "i-aws-$*"

# Run tests of one service using AWS
.PHONY: i-aws-%
i-aws-%:
	$(MAKE) -C "services/$*" i-aws

# Build services and run tests of one service
.PHONY: integration-%
integration-%: fast
	$(MAKE) "i-$*"

# Run tests of one service
.PHONY: i-%
i-%:
	$(MAKE) -C "services/$*" i

#################################
## docker targets

.PHONY: docker-prebuilder
docker-prebuilder:
	# `docker-prebuilder` needs to be built or pulled only once (unless native dependencies change)
	$(MAKE) -C build/alpine prebuilder

.PHONY: docker-deps
docker-deps:
	# `docker-deps` needs to be built or pulled only once (unless native dependencies change)
	$(MAKE) -C build/alpine deps

.PHONY: docker-builder
docker-builder:
	# `docker-builder` needs to be built or pulled only once (unless native dependencies change)
	$(MAKE) -C build/alpine builder

.PHONY: docker-intermediate
docker-intermediate:
	# `docker-intermediate` needs to be built whenever code changes - this essentially runs `stack clean && stack install` on the whole repo
	docker build -t $(DOCKER_USER)/alpine-intermediate:$(DOCKER_TAG) -f build/alpine/Dockerfile.intermediate --build-arg builder=$(DOCKER_USER)/alpine-builder --build-arg deps=$(DOCKER_USER)/alpine-deps .;
	docker tag $(DOCKER_USER)/alpine-intermediate:$(DOCKER_TAG) $(DOCKER_USER)/alpine-intermediate:latest;
	if test -n "$$DOCKER_PUSH"; then docker login -u $(DOCKER_USERNAME) -p $(DOCKER_PASSWORD); docker push $(DOCKER_USER)/alpine-intermediate:$(DOCKER_TAG); docker push $(DOCKER_USER)/alpine-intermediate:latest; fi;

.PHONY: docker-migrations
docker-migrations:
	# `docker-migrations` needs to be built whenever docker-intermediate was rebuilt AND new schema migrations were added.
	docker build -t $(DOCKER_USER)/migrations:$(DOCKER_TAG) -f build/alpine/Dockerfile.migrations --build-arg intermediate=$(DOCKER_USER)/alpine-intermediate --build-arg deps=$(DOCKER_USER)/alpine-deps .
	docker tag $(DOCKER_USER)/migrations:$(DOCKER_TAG) $(DOCKER_USER)/migrations:latest
	if test -n "$$DOCKER_PUSH"; then docker login -u $(DOCKER_USERNAME) -p $(DOCKER_PASSWORD); docker push $(DOCKER_USER)/migrations:$(DOCKER_TAG); docker push $(DOCKER_USER)/migrations:latest; fi;

.PHONY: docker-exe-%
docker-exe-%:
	docker image ls | grep $(DOCKER_USER)/alpine-deps > /dev/null || (echo "'make docker-deps' required.", exit 1)
	docker image ls | grep $(DOCKER_USER)/alpine-intermediate > /dev/null || (echo "'make docker-intermediate' required."; exit 1)
	docker build -t $(DOCKER_USER)/"$*":$(DOCKER_TAG) -f build/alpine/Dockerfile.executable --build-arg executable="$*" --build-arg intermediate=$(DOCKER_USER)/alpine-intermediate --build-arg deps=$(DOCKER_USER)/alpine-deps .
	docker tag $(DOCKER_USER)/"$*":$(DOCKER_TAG) $(DOCKER_USER)/"$*":latest
	if test -n "$$DOCKER_PUSH"; then docker login -u $(DOCKER_USERNAME) -p $(DOCKER_PASSWORD); docker push $(DOCKER_USER)/"$*":$(DOCKER_TAG); docker push $(DOCKER_USER)/"$*":latest; fi;

.PHONY: docker-services
docker-services:
	# make docker-services doesn't compile, only makes small images out of the `docker-intermediate` image
	# to recompile, run `docker-intermediate` first.
	docker image ls | grep $(DOCKER_USER)/alpine-deps > /dev/null || (echo "'make docker-deps' required.", exit 1)
	docker image ls | grep $(DOCKER_USER)/alpine-intermediate > /dev/null || (echo "'make docker-intermediate' required."; exit 1)
	# `make -C services/brig docker` == `make docker-exe-brig docker-exe-brig-integration docker-exe-brig-schema docker-exe-brig-index`
	$(MAKE) -C services/brig docker
	$(MAKE) -C services/gundeck docker
	$(MAKE) -C services/galley docker
	$(MAKE) -C services/cannon docker
	$(MAKE) -C services/proxy docker
	$(MAKE) -C services/spar docker
	$(MAKE) docker-exe-zauth
	$(MAKE) -C services/nginz docker

DOCKER_DEV_NETWORK := --net=host
DOCKER_DEV_VOLUMES := -v `pwd`:/wire-server
DOCKER_DEV_IMAGE   := quay.io/wire/alpine-builder:$(DOCKER_TAG)
.PHONY: run-docker-builder
run-docker-builder:
	@echo "if this does not work, consider 'docker pull', 'docker tag', or 'make -C build-alpine builder'."
	docker run --workdir /wire-server -it $(DOCKER_DEV_NETWORK) $(DOCKER_DEV_VOLUMES) --rm $(DOCKER_DEV_IMAGE) /bin/bash

#################################
## dependencies

libzauth:
	$(MAKE) -C libs/libzauth install
