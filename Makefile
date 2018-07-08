HASKELL_SERVICES := proxy cannon cargohold brig galley gundeck
SERVICES         := $(HASKELL_SERVICES) nginz
DOCKER_USER      ?= wireserver
DOCKER_TAG       ?= local

default: fast

init:
	mkdir -p dist

.PHONY: install
install: init
	stack install --pedantic --test --local-bin-path=dist

.PHONY: fast
fast: init
	stack install --pedantic --test --local-bin-path=dist --fast $(WIRE_STACK_OPTIONS)

.PHONY: clean
clean:
	stack clean
	$(MAKE) -C services/nginz clean
	-rm -rf dist
	-rm -f .metadata

.PHONY: services
services: init install
	$(MAKE) -C services/nginz

.PHONY: integration
integration: fast
	# We run "i" instead of "integration" to avoid useless rebuilds
	# (since after "fast" everything will be built already)
	$(MAKE) -C services/cargohold i
	$(MAKE) -C services/galley i
	$(MAKE) -C services/brig i
	$(MAKE) -C services/gundeck i-fake-aws
	$(MAKE) -C services/spar i

#################################
## docker targets

.PHONY: docker-services
docker-services:
	$(MAKE) -C build/alpine
	$(foreach service,$(SERVICES),$(MAKE) -C services/$(service) docker;)

.PHONY: docker-deps
docker-deps:
	$(MAKE) -C build/alpine deps

.PHONY: docker-builder
docker-builder:
	$(MAKE) -C build/alpine builder

.PHONY: docker-intermediate
docker-intermediate:
	docker build -t $(DOCKER_USER)/intermediate:$(DOCKER_TAG) -f build/alpine/Dockerfile.intermediate .;
	docker tag $(DOCKER_USER)/intermediate:$(DOCKER_TAG) $(DOCKER_USER)/intermediate:latest;
	if test -n "$$DOCKER_PUSH"; then docker login -u $(DOCKER_USERNAME) -p $(DOCKER_PASSWORD); docker push $(DOCKER_USER)/intermediate:$(DOCKER_TAG); docker push $(DOCKER_USER)/intermediate:latest; fi;

.PHONY: docker-migrations
docker-migrations:
	docker build -t $(DOCKER_USER)/migrations:$(DOCKER_TAG) -f build/alpine/Dockerfile.migrations .
	docker tag $(DOCKER_USER)/migrations:$(DOCKER_TAG) $(DOCKER_USER)/migrations:latest
	if test -n "$$DOCKER_PUSH"; then docker login -u $(DOCKER_USERNAME) -p $(DOCKER_PASSWORD); docker push $(DOCKER_USER)/migrations:$(DOCKER_TAG); docker push $(DOCKER_USER)/migrations:latest; fi;

.PHONY: docker-exe-%
docker-exe-%:
	docker build -t $(DOCKER_USER)/"$*":$(DOCKER_TAG) -f build/alpine/Dockerfile.executable --build-arg executable="$*" .
	docker tag $(DOCKER_USER)/"$*":$(DOCKER_TAG) $(DOCKER_USER)/"$*":latest
	if test -n "$$DOCKER_PUSH"; then docker login -u $(DOCKER_USERNAME) -p $(DOCKER_PASSWORD); docker push $(DOCKER_USER)/"$*":$(DOCKER_TAG); docker push $(DOCKER_USER)/"$*":latest; fi;

.PHONY: docker-service-%
docker-service-%:
	$(MAKE) -C services/"$*" docker

DOCKER_DEV_NETWORK := --net=host
DOCKER_DEV_VOLUMES := -v `pwd`:/src/wire-server
DOCKER_DEV_IMAGE   := quay.io/wire/alpine-builder:local
.PHONY: run-docker-builder
run-docker-builder:
	docker run -it $(DOCKER_DEV_NETWORK) $(DOCKER_DEV_VOLUMES) --rm $(DOCKER_DEV_IMAGE) /bin/bash || \
	( echo "$(DOCKER_DEV_IMAGE) not found.  building locally.  hit ^C to interrupt." && \
	  make -C build/alpine builder && \
	  make $@ )

#################################
## dependencies

libzauth:
	$(MAKE) -C libs/libzauth install
