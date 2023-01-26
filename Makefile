SHELL                 := /usr/bin/env bash
DOCKER_USER           ?= quay.io/wire
# kubernetes namespace for running integration tests
NAMESPACE             ?= test-$(USER)
# default docker image tag is your system username, you can override it via environment variable.
DOCKER_TAG            ?= $(USER)
# default helm chart version must be 0.0.42 for local development (because 42 is the answer to the universe and everything)
HELM_SEMVER           ?= 0.0.42
# The list of helm charts needed on internal kubernetes testing environments
CHARTS_INTEGRATION    := wire-server databases-ephemeral redis-cluster fake-aws nginx-ingress-controller nginx-ingress-services fluent-bit kibana sftd restund coturn
# The list of helm charts to publish on S3
# FUTUREWORK: after we "inline local subcharts",
# (e.g. move charts/brig to charts/wire-server/brig)
# this list could be generated from the folder names under ./charts/ like so:
# CHARTS_RELEASE := $(shell find charts/ -maxdepth 1 -type d | xargs -n 1 basename | grep -v charts)
CHARTS_RELEASE := wire-server redis-ephemeral redis-cluster databases-ephemeral	\
fake-aws fake-aws-s3 fake-aws-sqs aws-ingress fluent-bit kibana backoffice		\
calling-test demo-smtp elasticsearch-curator elasticsearch-external				\
elasticsearch-ephemeral minio-external cassandra-external						\
nginx-ingress-controller nginx-ingress-services reaper sftd restund coturn		\
inbucket k8ssandra-test-cluster
KIND_CLUSTER_NAME     := wire-server

package ?= all
EXE_SCHEMA := ./dist/$(package)-schema

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

default: install

init:
	mkdir -p dist

# Build all Haskell services and executables, run unit tests
.PHONY: install
install: init
	cabal build all
	./hack/bin/cabal-run-all-tests.sh
	./hack/bin/cabal-install-artefacts.sh all

# Clean
.PHONY: full-clean
full-clean: clean
	rm -rf ~/.cache/hie-bios
	rm -rf ./dist-newstyle ./.env
	direnv reload
	@echo -e "\n\n*** NOTE: you may want to also 'rm -rf ~/.cabal/store \$$CABAL_DIR/store', not sure.\n"

.PHONY: clean
clean:
	cabal clean
	-rm -rf dist

.PHONY: clean-hint
clean-hint:
	@echo -e "\n\n\n>>> PSA: if you get errors that are hard to explain,"
	@echo -e ">>> try 'git submodule update --init --recursive' and 'make full-clean' and run your command again."
	@echo -e ">>> see https://github.com/wireapp/wire-server/blob/develop/docs/developer/building.md#linker-errors-while-compiling"
	@echo -e ">>> to never have to remember submodules again, try `git config --global submodule.recurse true`"
	@echo -e "\n\n\n"

.PHONY: cabal.project.local
cabal.project.local:
	echo "optimization: False" > ./cabal.project.local
	./hack/bin/cabal-project-local-template.sh "ghc-options: -O0" >> ./cabal.project.local

# Usage: make c package=brig test=1
.PHONY: c
c: cabal-fmt
	cabal build $(WIRE_CABAL_BUILD_OPTIONS) $(package) || ( make clean-hint; false )
ifeq ($(test), 1)
	./hack/bin/cabal-run-tests.sh $(package) $(testargs)
endif
	./hack/bin/cabal-install-artefacts.sh $(package)

# ci here doesn't refer to continuous integration, but to cabal-integration
# Usage: make ci package=brig test=1
# If you want to pass arguments to the test-suite call the script directly.
.PHONY: ci
ci: c db-migrate
	./hack/bin/cabal-run-integration.sh $(package)

.PHONY: cabal-fmt
cabal-fmt:
	./hack/bin/cabal-fmt.sh $(package)

# Get a ghci environment running for the given package.
.PHONY: repl
repl: cabal-fmt
	cabal repl $(WIRE_CABAL_BUILD_OPTIONS) $(package)

# Use ghcid to watch a particular package.
# pass target=package:name to specify which target is watched.
.PHONY: ghcid
ghcid:
	ghcid -l=hlint --command "cabal repl $(target)"

# Used by CI
.PHONY: lint-all
lint-all: formatc hlint-check-all check-local-nix-derivations treefmt

.PHONY: hlint-check-all
hlint-check-all:
	./tools/hlint.sh -f all -m check

.PHONY: hlint-inplace-all
hlint-inplace-all:
	./tools/hlint.sh -f all -m inplace

.PHONY: hlint-check-pr
hlint-check-pr:
	./tools/hlint.sh -f pr -m check

.PHONY: hlint-inplace-pr
hlint-inplace-pr:
	./tools/hlint.sh -f pr -m inplace

.PHONY: hlint-check
hlint-check:
	./tools/hlint.sh -f changeset -m check

.PHONY: hlint-inplace
hlint-inplace:
	./tools/hlint.sh -f changeset -m inplace

regen-local-nix-derivations:
	./hack/bin/generate-local-nix-packages.sh

check-local-nix-derivations: regen-local-nix-derivations
	git diff --exit-code

# Build everything (Haskell services and nginz)
.PHONY: services
services: init install
	$(MAKE) -C services/nginz

# formats all Haskell files (which don't contain CPP)
.PHONY: format
format:
	./tools/ormolu.sh

# formats all Haskell files even if local changes are not committed to git
.PHONY: formatf
formatf:
	./tools/ormolu.sh -f pr

# formats all Haskell files even if local changes are not committed to git
.PHONY: formatf-all
formatf-all:
	./tools/ormolu.sh -f all

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

.PHONY: treefmt
treefmt:
	treefmt

#################################
## docker targets

.PHONY: build-image-%
build-image-%:
	nix-build ./nix -A wireServer.imagesNoDocs.$(*) && \
	./result | docker load | tee /tmp/imageName-$(*) && \
	imageName=$$(grep quay.io /tmp/imageName-$(*) | awk '{print $$3}') && \
	echo 'You can run your image locally using' && \
	echo "  docker run -it --entrypoint bash $$imageName" && \
	echo 'or upload it using' && \
	echo "  docker push $$imageName"

.PHONY: upload-images
upload-images:
	./hack/bin/upload-images.sh imagesNoDocs

.PHONY: upload-images-dev
upload-images-dev:
	./hack/bin/upload-images.sh imagesUnoptimizedNoDocs

upload-hoogle-image:
	./hack/bin/upload-image.sh wireServer.hoogleImage

#################################
## cassandra management

.PHONY: git-add-cassandra-schema
git-add-cassandra-schema: db-reset git-add-cassandra-schema-impl

.PHONY: git-add-cassandra-schema-impl
git-add-cassandra-schema-impl:
	$(eval CASSANDRA_CONTAINER := $(shell docker ps | grep '/cassandra:' | perl -ne '/^(\S+)\s/ && print $$1'))
	( echo '-- automatically generated with `make git-add-cassandra-schema`'; \
      docker exec -i $(CASSANDRA_CONTAINER) /usr/bin/cqlsh -e "DESCRIBE schema;" ) \
    | sed "s/CREATE TABLE galley_test.member_client/-- NOTE: this table is unused. It was replaced by mls_group_member_client\nCREATE TABLE galley_test.member_client/g" \
      > ./cassandra-schema.cql
	git add ./cassandra-schema.cql

.PHONY: cqlsh
cqlsh:
	$(eval CASSANDRA_CONTAINER := $(shell docker ps | grep '/cassandra:' | perl -ne '/^(\S+)\s/ && print $$1'))
	@echo "make sure you have ./deploy/dockerephemeral/run.sh running in another window!"
	docker exec -it $(CASSANDRA_CONTAINER) /usr/bin/cqlsh

.PHONY: db-reset-package
db-reset-package:
	@echo "Deprecated! Please use 'db-reset' instead"
	$(MAKE) db-reset package=$(package)

.PHONY: db-migrate-package
db-migrate-package:
	@echo "Deprecated! Please use 'db-migrate' instead"
	$(MAKE) db-migrate package=$(package)

# Usage:
#
# Reset all keyspaces
# make db-reset
#
# Reset keyspace for only one service, say galley:
# make db-reset package=galley
.PHONY: db-reset
db-reset: c
	@echo "Make sure you have ./deploy/dockerephemeral/run.sh running in another window!"
ifeq ($(package), all)
	./dist/brig-schema --keyspace brig_test --replication-factor 1 --reset
	./dist/galley-schema --keyspace galley_test --replication-factor 1 --reset
	./dist/gundeck-schema --keyspace gundeck_test --replication-factor 1 --reset
	./dist/spar-schema --keyspace spar_test --replication-factor 1 --reset
else
	$(EXE_SCHEMA) --keyspace $(package)_test --replication-factor 1 --reset
endif

# Usage:
#
# Migrate all keyspaces
# make db-migrate
#
# Migrate keyspace for only one service, say galley:
# make db-migrate package=galley
.PHONY: db-migrate
db-migrate: c
ifeq ($(package), all)
	./dist/brig-schema --keyspace brig_test --replication-factor 1
	./dist/galley-schema --keyspace galley_test --replication-factor 1
	./dist/gundeck-schema --keyspace gundeck_test --replication-factor 1
	./dist/spar-schema --keyspace spar_test --replication-factor 1
# How this check works: https://stackoverflow.com/a/9802777
else ifeq ($(package), $(filter $(package),brig galley gundeck spar))
	$(EXE_SCHEMA) --keyspace $(package)_test --replication-factor 1
else
	@echo No schema migrations for $(package)
endif


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
hie.yaml:
	echo -e 'cradle:\n  cabal: {}' > hie.yaml

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

.PHONY: kube-integration-e2e-telepresence
kube-integration-e2e-telepresence:
	./services/brig/federation-tests.sh $(NAMESPACE)

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
	kubectl delete pod -n $(NAMESPACE) -l app=$(*)
	kubectl delete pod -n $(NAMESPACE)-fed2 -l app=$(*)

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
	      echo "Environment variable DOCKER_TAG not set to non-default value. Re-run with DOCKER_TAG=<something>. Try using 'make latest-tag' for latest develop docker image tag";\
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

.PHONY: charts-serve-all
charts-serve-all: $(foreach chartName,$(CHARTS_RELEASE),chart-$(chartName))
	./hack/bin/serve-charts.sh $(CHARTS_RELEASE)

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

.PHONY: kind-cluster
kind-cluster:
	kind create cluster --name $(KIND_CLUSTER_NAME)

.PHONY: kind-delete
kind-delete:
	rm -f $(CURDIR)/.local/kind-kubeconfig
	kind delete cluster --name $(KIND_CLUSTER_NAME)

.PHONY: kind-reset
kind-reset: kind-delete kind-cluster

.PHONY: kind-upload-images
kind-upload-images:
	DOCKER_TAG=$(DOCKER_TAG) KIND_CLUSTER_NAME=$(KIND_CLUSTER_NAME) ./hack/bin/kind-upload-images.sh

.PHONY: kind-upload-image
kind-upload-image-%:
	DOCKER_TAG=$(DOCKER_TAG) KIND_CLUSTER_NAME=$(KIND_CLUSTER_NAME) ./hack/bin/kind-upload-image.sh wireServer.imagesUnoptimizedNoDocs.$(*)

.local/kind-kubeconfig:
	mkdir -p $(CURDIR)/.local
	kind get kubeconfig --name $(KIND_CLUSTER_NAME) > $(CURDIR)/.local/kind-kubeconfig
	chmod 0600 $(CURDIR)/.local/kind-kubeconfig

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
	HELMFILE_ENV="kind" KUBECONFIG=$(CURDIR)/.local/kind-kubeconfig make kube-integration-setup

.PHONY: kind-integration-test
kind-integration-test: .local/kind-kubeconfig
	HELMFILE_ENV="kind" KUBECONFIG=$(CURDIR)/.local/kind-kubeconfig make kube-integration-test

kind-integration-e2e: .local/kind-kubeconfig
	cd services/brig && KUBECONFIG=$(CURDIR)/.local/kind-kubeconfig ./federation-tests.sh $(NAMESPACE)

kind-restart-all: .local/kind-kubeconfig
	export KUBECONFIG=$(CURDIR)/.local/kind-kubeconfig && \
	kubectl delete pod -n $(NAMESPACE) -l release=$(NAMESPACE)-wire-server && \
	kubectl delete pod -n $(NAMESPACE)-fed2 -l release=$(NAMESPACE)-wire-server-2

kind-restart-nginx-ingress: .local/kind-kubeconfig
	export KUBECONFIG=$(CURDIR)/.local/kind-kubeconfig && \
	kubectl delete pod -n $(NAMESPACE) -l app=nginx-ingress && \
	kubectl delete pod -n $(NAMESPACE)-fed2 -l app=nginx-ingress

kind-restart-%: .local/kind-kubeconfig
	export KUBECONFIG=$(CURDIR)/.local/kind-kubeconfig && \
	kubectl delete pod -n $(NAMESPACE) -l app=$(*) && \
	kubectl delete pod -n $(NAMESPACE)-fed2 -l app=$(*)

# This target can be used to template a helm chart with values filled in from
# hack/helm_vars (what CI uses) as overrrides, if available. This allows debugging helm
# templating issues without actually installing anything, and without needing
# access to a kubernetes cluster. e.g.:
#   make helm-template-wire-server
helm-template-%: clean-charts charts-integration
	./hack/bin/helm-template.sh $(*)
