SHELL                 := /usr/bin/env bash
LANG                  := en_US.UTF-8
DOCKER_USER           ?= quay.io/wire
# kubernetes namespace for running integration tests
NAMESPACE             ?= test-$(USER)
# default docker image tag is your system username, you can override it via environment variable.
DOCKER_TAG            ?= $(USER)
# default helm chart version must be 0.0.42 for local development (because 42 is the answer to the universe and everything)
HELM_SEMVER           ?= 0.0.42
# The list of helm charts needed for integration tests on kubernetes
CHARTS_INTEGRATION    := wire-server databases-ephemeral fake-aws nginx-ingress-controller nginx-ingress-services
# The list of helm charts to publish on S3
# FUTUREWORK: after we "inline local subcharts",
# (e.g. move charts/brig to charts/wire-server/brig)
# this list could be generated from the folder names under ./charts/ like so:
# CHARTS_RELEASE := $(shell find charts/ -maxdepth 1 -type d | xargs -n 1 basename | grep -v charts)
CHARTS_RELEASE        := wire-server redis-ephemeral databases-ephemeral fake-aws fake-aws-s3 fake-aws-sqs aws-ingress  fluent-bit kibana backoffice calling-test demo-smtp elasticsearch-curator elasticsearch-external elasticsearch-ephemeral fluent-bit minio-external cassandra-external nginx-ingress-controller nginx-ingress-services reaper wire-server-metrics sftd
BUILDAH_PUSH          ?= 0
KIND_CLUSTER_NAME     := wire-server
BUILDAH_KIND_LOAD     ?= 1

# This ensures that focused unit tests written in hspec fail. This is supposed
# to help us avoid merging PRs with focused tests. This will not catch focused
# integration tests as they are run in kubernetes where this Makefile doesn't
# get executed. This is set here as the CI uses this Makefile, this could live
# in several Makefiles we have in this repository, but there is little point of
# doing so.
#
# Additionally, if stack is being used with nix, environment variables do not
# make it into the shell where hspec is run, to tackle that this variable is
# also exported in stack-deps.nix.
export HSPEC_OPTIONS = --fail-on-focused

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

# formats all Haskell files (which don't contain CPP)
.PHONY: format
format:
	./tools/ormolu.sh

# formats all Haskell files even if local changes are not committed to git
.PHONY: formatf
formatf:
	./tools/ormolu.sh -f

# checks that all Haskell files are formatted; fail if a `make format` run is needed.
.PHONY: formatc
formatc:
	./tools/ormolu.sh -c

# For any Haskell or Rust file, update or add a license header if necessary.
# Headers should be added according to Ormolu's formatting rules, but please check just in case.
.PHONY: add-license
add-license:
	# Check headroom is installed. If not, please run 'stack install headroom'
	command -v headroom
	headroom run
	@echo ""
	@echo "you might want to run 'make formatf' now to make sure ormolu is happy"

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
	docker build -t $(DOCKER_USER)/alpine-intermediate:$(DOCKER_TAG) -f build/alpine/Dockerfile.intermediate --build-arg builder=$(DOCKER_USER)/alpine-builder:develop --build-arg deps=$(DOCKER_USER)/alpine-deps:develop .;
	docker tag $(DOCKER_USER)/alpine-intermediate:$(DOCKER_TAG) $(DOCKER_USER)/alpine-intermediate:latest;
	if test -n "$$DOCKER_PUSH"; then docker login -u $(DOCKER_USERNAME) -p $(DOCKER_PASSWORD); docker push $(DOCKER_USER)/alpine-intermediate:$(DOCKER_TAG); docker push $(DOCKER_USER)/alpine-intermediate:latest; fi;

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
	$(MAKE) -C tools/stern docker
	$(MAKE) docker-exe-zauth
	$(MAKE) -C services/nginz docker

DOCKER_DEV_NETWORK := --net=host
DOCKER_DEV_VOLUMES := -v `pwd`:/wire-server
DOCKER_DEV_IMAGE   := quay.io/wire/alpine-builder:$(DOCKER_TAG)
.PHONY: run-docker-builder
run-docker-builder:
	@echo "if this does not work, consider 'docker pull', 'docker tag', or 'make -C build-alpine builder'."
	docker run --workdir /wire-server -it $(DOCKER_DEV_NETWORK) $(DOCKER_DEV_VOLUMES) --rm $(DOCKER_DEV_IMAGE) /bin/bash

CASSANDRA_CONTAINER := $(shell docker ps | grep '/cassandra:' | perl -ne '/^(\S+)\s/ && print $$1')
.PHONY: git-add-cassandra-schema
git-add-cassandra-schema: db-reset
	( echo '-- automatically generated with `make git-add-cassandra-schema`' ; docker exec -i $(CASSANDRA_CONTAINER) /usr/bin/cqlsh -e "DESCRIBE schema;" ) > ./docs/reference/cassandra-schema.cql
	git add ./docs/reference/cassandra-schema.cql

.PHONY: cqlsh
cqlsh:
	@echo "make sure you have ./deploy/dockerephemeral/run.sh running in another window!"
	docker exec -it $(CASSANDRA_CONTAINER) /usr/bin/cqlsh

.PHONY: db-reset
db-reset:
	@echo "make sure you have ./deploy/dockerephemeral/run.sh running in another window!"
	make -C services/brig db-reset
	make -C services/galley db-reset
	make -C services/gundeck db-reset
	make -C services/spar db-reset

#################################
## dependencies

libzauth:
	$(MAKE) -C libs/libzauth install

#################################
# Useful when using Haskell IDE Engine
# https://github.com/haskell/haskell-ide-engine
#
# Run this again after changes to libraries or dependencies.
.PHONY: hie.yaml
hie.yaml: stack-dev.yaml
	stack build implicit-hie
	stack exec gen-hie | nix-shell --command 'yq "{cradle: {stack: {stackYaml: \"./stack-dev.yaml\", components: .cradle.stack}}}" > hie.yaml'

.PHONY: stack-dev.yaml
stack-dev.yaml:
	cp stack.yaml stack-dev.yaml
	echo -e '\n\nghc-options:\n "$$locals": -O0 -Wall -Werror' >> stack-dev.yaml

#####################################
# Today we pretend to be CI and run integration tests on kubernetes
# (see also docs/developer/processes.md)
#
# NOTE: This uses local helm charts from .local/charts (which it builds before running this)
#
# NOTE/WARNING: By default, it uses local docker image tags,
# which will not work at this time on your remote kubernetes cluster. [FUTUREWORK: local kubernetes cluster]
#
# If you wish to use docker images that are uploaded to quay.io, you must set DOCKER_TAG
#
#   DOCKER_TAG=<desired-wire-server-docker-tag> make kube-integration
#
# and if you don't know what a good DOCKER_TAG might be, you can run
#
#   make latest-tag
#
# This task requires: [FUTUREWORK: add tooling setup to wire-server]
#   - helm (version 3.1.1)
#   - kubectl
#   - a valid kubectl context configured (i.e. access to a kubernetes cluster)
.PHONY: kube-integration
kube-integration:  kube-integration-setup kube-integration-test

.PHONY: kube-integration-setup
kube-integration-setup: charts-integration
	export NAMESPACE=$(NAMESPACE); ./hack/bin/integration-setup-federation.sh

.PHONY: kube-integration-test
kube-integration-test:
	export NAMESPACE=$(NAMESPACE); ./hack/bin/integration-test.sh

.PHONY: kube-integration-teardown
kube-integration-teardown:
	export NAMESPACE=$(NAMESPACE); ./hack/bin/integration-teardown-federation.sh

.PHONY: kube-integration-setup-sans-federation
kube-integration-setup-sans-federation: guard-tag charts-integration
	# by default "test-<your computer username> is used as namespace
	# you can override the default by setting the NAMESPACE environment variable
	export NAMESPACE=$(NAMESPACE); ./hack/bin/integration-setup.sh

.PHONY: kube-integration-teardown-sans-federation
kube-integration-teardown-sans-federation:
	export NAMESPACE=$(NAMESPACE); ./hack/bin/integration-teardown.sh

.PHONY: kube-restart-%
kube-restart-%:
	kubectl delete pod -n $(NAMESPACE) -l wireService=$(*)
	kubectl delete pod -n $(NAMESPACE)-fed2 -l wireService=$(*)

.PHONY: latest-tag
latest-tag:
	./hack/bin/find-latest-docker-tag.sh

.PHONY: release-chart-%
release-chart-%:
	@if [ "${HELM_SEMVER}" = "0.0.42" ]; then \
	      echo "Environment variable HELM_SEMVER not set to non-default value. Re-run with HELM_SEMVER=<something>"; \
	    exit 1; \
	fi
	@if [ "${DOCKER_TAG}" = "${USER}" ]; then \
	      echo "Environment variable DOCKER_TAG not set to non-default value. Re-run with DOCKER_TAG=<something>"; \
	    exit 1; \
	fi
	make chart-$(*)

.PHONY: guard-tag
guard-tag:
	@if [ "${DOCKER_TAG}" = "${USER}" ]; then \
	      echo "Environment variable DOCKER_TAG not set to non-default value. Re-run with DOCKER_TAG=<something>. Try using 'make latest-brig-tag' for latest develop docker image tag";\
	    exit 1; \
	fi

# Rationale for copying charts to a gitignored folder before modifying helm versions and docker image tags:
#
# * we want to keep git history clean and not clutter it with version bump commits
#   * synchronizing version bumps with multiple PRs, releases to master and merges back to develop is hard to do in git
#   * we don't want to spend time modifying version tags manually all the time
# * we want version pinning for helm charts and docker images for reproducible results during deployments
#   * CI will keep track of versioning and upload charts to an S3 mirror
#   * if you need to do this locally, also use this make target and set desired versions accordingly.
.PHONY: chart-%
chart-%:
	./hack/bin/copy-charts.sh $(*)
	./hack/bin/set-wire-server-image-version.sh $(DOCKER_TAG)
	./hack/bin/set-helm-chart-version.sh "$*" $(HELM_SEMVER)

# Usecase for this make target:
#  * for local integration testing of wire-server inside kubernetes
.PHONY: charts-integration
charts-integration: $(foreach chartName,$(CHARTS_INTEGRATION),chart-$(chartName))

.PHONY: charts-serve
charts-serve: charts-integration
	./hack/bin/serve-charts.sh $(CHARTS_INTEGRATION)

# Usecase for this make target:
# 1. for releases of helm charts
# 2. for testing helm charts more generally
.PHONY: charts-release
charts-release: $(foreach chartName,$(CHARTS_RELEASE),release-chart-$(chartName))

.PHONY: clean-charts
clean-charts:
	rm -rf .local/charts

##########################################
# Helm chart releasing (mirroring to S3)
# Only CI should run these targets ideally

# Usecases for this make target:
# To release one single helm chart to S3 mirror
# (assummption: CI sets DOCKER_TAG and HELM_SEMVER)
.PHONY: upload-chart-%
upload-chart-%: release-chart-%
	./hack/bin/upload-helm-charts-s3.sh -r $(HELM_REPO) -d .local/charts/$(*)

# Usecases for this make target:
# To uplaod all helm charts in the CHARTS_RELEASE list (see top of the time)
# (assummption: CI sets DOCKER_TAG and HELM_SEMVER)
.PHONY: upload-charts
upload-charts: charts-release
	./hack/bin/upload-helm-charts-s3.sh -r $(HELM_REPO)

.PHONY: echo-release-charts
echo-release-charts:
	@echo ${CHARTS_RELEASE}

.PHONY: buildah-docker
buildah-docker: buildah-docker-nginz
	./hack/bin/buildah-compile.sh
	BUILDAH_PUSH=${BUILDAH_PUSH} KIND_CLUSTER_NAME=${KIND_CLUSTER_NAME} BUILDAH_KIND_LOAD=${BUILDAH_KIND_LOAD}  ./hack/bin/buildah-make-images.sh

.PHONY: buildah-docker-nginz
buildah-docker-nginz:
	BUILDAH_PUSH=${BUILDAH_PUSH} KIND_CLUSTER_NAME=${KIND_CLUSTER_NAME} BUILDAH_KIND_LOAD=${BUILDAH_KIND_LOAD}  ./hack/bin/buildah-make-images-nginz.sh

.PHONY: buildah-docker-%
buildah-docker-%:
	./hack/bin/buildah-compile.sh $(*)
	BUILDAH_PUSH=${BUILDAH_PUSH} EXECUTABLES=$(*) KIND_CLUSTER_NAME=${KIND_CLUSTER_NAME} BUILDAH_KIND_LOAD=${BUILDAH_KIND_LOAD} ./hack/bin/buildah-make-images.sh

.PHONY: buildah-clean
buildah-clean:
	./hack/bin/buildah-clean.sh

.PHONY: kind-cluster
kind-cluster:
	kind create cluster --name $(KIND_CLUSTER_NAME)

.PHONY: kind-delete
kind-delete:
	rm -f $(CURDIR)/.local/kind-kubeconfig
	kind delete cluster --name $(KIND_CLUSTER_NAME)

.PHONY: kind-reset
kind-reset: kind-delete kind-cluster

.local/kind-kubeconfig:
	mkdir -p $(CURDIR)/.local
	kind get kubeconfig --name $(KIND_CLUSTER_NAME) > $(CURDIR)/.local/kind-kubeconfig

# This guard is a fail-early way to save needing to debug nginz container not
# starting up in the second namespace of the kind cluster in some cases. Error
# message was:
#     nginx PID: 8
#     Couldn't initialize inotify: No file descriptors available
#     Try increasing the value of /proc/sys/fs/inotify/max_user_instances
#     inotifywait failed, killing nginx
.PHONY: guard-inotify
guard-inotify:
	@if [[ $$(cat /proc/sys/fs/inotify/max_user_instances) -lt 200 ]]; then \
		echo "Your /proc/sys/fs/inotify/max_user_instances value is most likely too low to run two full environments of wire-server in kind/kubernetes"; \
		echo "You can run: "; \
		echo ""; \
		echo "  echo \"1000\" | sudo tee /proc/sys/fs/inotify/max_user_instances"; \
		echo ""; \
		echo "(or, to make that change permanent across reboots, you can run: )"; \
		echo ""; \
		echo "  echo 'fs.inotify.max_user_instances = 1000' | sudo tee /etc/sysctl.d/99-whatever.conf;"; \
		echo "  sudo sysctl -p --system"; \
		echo ""; \
		exit 1; \
	fi

.PHONY: kind-integration-setup
kind-integration-setup: guard-inotify .local/kind-kubeconfig
	ENABLE_KIND_VALUES="1" KUBECONFIG=$(CURDIR)/.local/kind-kubeconfig make kube-integration-setup

.PHONY: kind-integration-test
kind-integration-test: .local/kind-kubeconfig
	ENABLE_KIND_VALUES="1" KUBECONFIG=$(CURDIR)/.local/kind-kubeconfig make kube-integration-test

kind-integration-e2e: .local/kind-kubeconfig
	cd services/brig && KUBECONFIG=$(CURDIR)/.local/kind-kubeconfig ./federation-tests.sh $(NAMESPACE)

kind-restart-all: .local/kind-kubeconfig
	export KUBECONFIG=$(CURDIR)/.local/kind-kubeconfig && \
	kubectl delete pod -n $(NAMESPACE) -l release=$(NAMESPACE)-wire-server && \
	kubectl delete pod -n $(NAMESPACE)-fed2 -l release=$(NAMESPACE)-fed2-wire-server

kind-restart-nginx-ingress: .local/kind-kubeconfig
	export KUBECONFIG=$(CURDIR)/.local/kind-kubeconfig && \
	kubectl delete pod -n $(NAMESPACE) -l app=nginx-ingress && \
	kubectl delete pod -n $(NAMESPACE)-fed2 -l app=nginx-ingress

kind-restart-%: .local/kind-kubeconfig
	export KUBECONFIG=$(CURDIR)/.local/kind-kubeconfig && \
	kubectl delete pod -n $(NAMESPACE) -l wireService=$(*) && \
	kubectl delete pod -n $(NAMESPACE)-fed2 -l wireService=$(*)

# This target can be used to template a helm chart with values filled in from
# hack/helm_vars (what CI uses) as overrrides, if available. This allows debugging helm
# templating issues without actually installing anything, and without needing
# access to a kubernetes cluster. e.g.:
#   make helm-template-wire-server
helm-template-%: clean-charts charts-integration
	./hack/bin/helm-template.sh $(*)

