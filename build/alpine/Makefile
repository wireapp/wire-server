DOCKER_USER   ?= wireserver
DOCKER_TAG    ?= local

default: deps builder

.PHONY: deps
deps:
	docker build -t $(DOCKER_USER)/alpine-deps:$(DOCKER_TAG) -f Dockerfile.deps .
	docker tag $(DOCKER_USER)/alpine-deps:$(DOCKER_TAG) $(DOCKER_USER)/alpine-deps:latest
	if test -n "$$DOCKER_PUSH"; then docker push $(DOCKER_USER)/alpine-deps:$(DOCKER_TAG); docker push $(DOCKER_USER)/alpine-deps:latest; fi;

.PHONY: builder
builder:
	docker build -t $(DOCKER_USER)/alpine-builder:$(DOCKER_TAG) -f Dockerfile.builder .
	docker tag $(DOCKER_USER)/alpine-builder:$(DOCKER_TAG) $(DOCKER_USER)/alpine-builder:latest
	if test -n "$$DOCKER_PUSH"; then docker push $(DOCKER_USER)/alpine-builder:$(DOCKER_TAG); docker push $(DOCKER_USER)/alpine-builder:latest; fi;
