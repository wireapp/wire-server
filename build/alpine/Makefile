LANG                    := en_US.UTF-8
DOCKER_USER             ?= quay.io/wire
DOCKER_TAG              ?= $(USER)
DOCKER_DOWNLOAD_TAG     ?= develop

default: deps prebuilder builder

.PHONY: deps
deps:
	docker build -t $(DOCKER_USER)/alpine-deps:$(DOCKER_TAG) -f Dockerfile.deps .
	docker tag $(DOCKER_USER)/alpine-deps:$(DOCKER_TAG) $(DOCKER_USER)/alpine-deps:latest
	if test -n "$$DOCKER_PUSH"; then docker push $(DOCKER_USER)/alpine-deps:$(DOCKER_TAG); docker push $(DOCKER_USER)/alpine-deps:latest; fi;

.PHONY: prebuilder
prebuilder:
	docker build -t $(DOCKER_USER)/alpine-prebuilder:$(DOCKER_TAG) -f Dockerfile.prebuilder .
	docker tag $(DOCKER_USER)/alpine-prebuilder:$(DOCKER_TAG) $(DOCKER_USER)/alpine-prebuilder:latest
	if test -n "$$DOCKER_PUSH"; then docker push $(DOCKER_USER)/alpine-prebuilder:$(DOCKER_TAG); docker push $(DOCKER_USER)/alpine-prebuilder:latest; fi;

.PHONY: builder
builder:
	docker build --build-arg prebuilder=$(DOCKER_USER)/alpine-prebuilder -t $(DOCKER_USER)/alpine-builder:$(DOCKER_TAG) -f Dockerfile.builder .
	docker tag $(DOCKER_USER)/alpine-builder:$(DOCKER_TAG) $(DOCKER_USER)/alpine-builder:latest
	if test -n "$$DOCKER_PUSH"; then docker push $(DOCKER_USER)/alpine-builder:$(DOCKER_TAG); docker push $(DOCKER_USER)/alpine-builder:latest; fi;

.PHONY: download-as-local
download-as-local:
	docker pull $(DOCKER_USER)/alpine-deps:$(DOCKER_DOWNLOAD_TAG) && docker tag quay.io/wire/alpine-deps:$(DOCKER_DOWNLOAD_TAG) quay.io/wire/alpine-deps:$(DOCKER_TAG)
	docker pull $(DOCKER_USER)/alpine-builder:$(DOCKER_DOWNLOAD_TAG) && docker tag quay.io/wire/alpine-builder:$(DOCKER_DOWNLOAD_TAG) quay.io/wire/alpine-builder:$(DOCKER_TAG)
