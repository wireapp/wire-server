LANG         := en_US.UTF-8
SHELL        := /usr/bin/env bash
NAME         := backoffice-frontend
EXECUTABLE   := backoffice-frontend
DOCKER_USER  ?= quay.io/wire
DOCKER_TAG   ?= local
FE_VERSION   ?= backoffice-0.1.8%2B0

guard-%:
	@ if [ "${${*}}" = "" ]; then \
	      echo "Environment variable $* not set"; \
	    exit 1; \
	fi

default: bo docker

# pull files from s3 and move them around a little to where
# docker-build will find them.
.PHONY: bo
bo:
	curl -L https://s3-eu-west-1.amazonaws.com/public.wire.com/artifacts/$(FE_VERSION).tar.gz -o backoffice.tar.gz
	mkdir -p var/www/swagger-ui/api-docs
	tar zxvf backoffice.tar.gz -C var/www/swagger-ui --strip-components=1
	cp conf/resources.json var/www/swagger-ui/api-docs
	rm backoffice.tar.gz

# docker build, tag, push (if applicable).
.PHONY: docker
docker:
	docker build -t $(DOCKER_USER)/$(EXECUTABLE):$(DOCKER_TAG) . && \
	docker tag $(DOCKER_USER)/$(EXECUTABLE):$(DOCKER_TAG) $(DOCKER_USER)/$(EXECUTABLE):latest && \
	if test -n "$$DOCKER_PUSH"; then docker login -u $(DOCKER_USERNAME) -p $(DOCKER_PASSWORD); docker push $(DOCKER_USER)/$(EXECUTABLE):$(DOCKER_TAG); docker push $(DOCKER_USER)/$(EXECUTABLE):latest; fi

# run docker image in develop mode.
.PHONY: run-develop
run-develop:
	@echo '*** please open http://localhost:8080/'
	docker run -it --net=host --rm $(DOCKER_USER)/$(EXECUTABLE):$(DOCKER_TAG)

# run docker image in demo-mode, as a composite with stern from
# quay.io.
.PHONY: run-demo
run-demo:
	@echo '*** please open http://localhost:8080/'
	docker-compose --file docker-compose.yaml up
