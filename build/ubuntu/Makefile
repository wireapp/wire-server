LANG := en_US.UTF-8
DOCKER_USER   ?= quay.io/wire
DOCKER_TAG    ?= local

default: deps prebuilder builder

.PHONY: deps
deps:
	docker build -t $(DOCKER_USER)/ubuntu20-deps:$(DOCKER_TAG) -f Dockerfile.deps .
	docker tag $(DOCKER_USER)/ubuntu20-deps:$(DOCKER_TAG) $(DOCKER_USER)/ubuntu20-deps:latest
	if test -n "$$DOCKER_PUSH"; then docker push $(DOCKER_USER)/ubuntu20-deps:$(DOCKER_TAG); docker push $(DOCKER_USER)/ubuntu20-deps:latest; fi;

.PHONY: prebuilder
prebuilder:
	docker build -t $(DOCKER_USER)/ubuntu20-prebuilder:$(DOCKER_TAG) -f Dockerfile.prebuilder .
	docker tag $(DOCKER_USER)/ubuntu20-prebuilder:$(DOCKER_TAG) $(DOCKER_USER)/ubuntu20-prebuilder:latest
	if test -n "$$DOCKER_PUSH"; then docker push $(DOCKER_USER)/ubuntu20-prebuilder:$(DOCKER_TAG); docker push $(DOCKER_USER)/ubuntu20-prebuilder:latest; fi;

.PHONY: builder
builder:
	docker build --build-arg prebuilder=$(DOCKER_USER)/ubuntu20-prebuilder -t $(DOCKER_USER)/ubuntu20-builder:$(DOCKER_TAG) -f Dockerfile.builder .
	docker tag $(DOCKER_USER)/ubuntu20-builder:$(DOCKER_TAG) $(DOCKER_USER)/ubuntu20-builder:latest
	if test -n "$$DOCKER_PUSH"; then docker push $(DOCKER_USER)/ubuntu20-builder:$(DOCKER_TAG); docker push $(DOCKER_USER)/ubuntu20-builder:latest; fi;
