HASKELL_SERVICES := proxy cannon cargohold brig galley gundeck
SERVICES         := $(HASKELL_SERVICES) nginz
DOCKER_USER      ?= wireserver
DOCKER_TAG       ?= local

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


.PHONY: services
services:
	$(foreach service,$(HASKELL_SERVICES),$(MAKE) -C services/$(service) clean install;)

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