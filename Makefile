SHELL = bash

.DEFAULT_GOAL := docs

MKFILE_DIR = $(abspath $(dir $(abspath $(lastword $(MAKEFILE_LIST)))))

DOCKER_USER   ?= quay.io/wire
DOCKER_IMAGE  = alpine-sphinx
DOCKER_TAG    ?= latest

# You can set these variables from the command line, and also
# from the environment for the first two.
SPHINXOPTS    ?= -q
SPHINXBUILD   ?= sphinx-build
SOURCEDIR     = $(MKFILE_DIR)/src
BUILDDIR      = $(MKFILE_DIR)/build

VENV_DIR = $(MKFILE_DIR)/.venv
VENV_BIN = $(VENV_DIR)/bin


.PHONY: Makefile


.DEFAULT: docs
.PHONY: docs
docs:
	docker run --rm -v $$(pwd):/mnt $(DOCKER_USER)/$(DOCKER_IMAGE):$(DOCKER_TAG) make clean html

# Only build part of the documentation
# See 'exclude_patterns' in source/conf.py
docs-administrate:
	docker run --rm -e SPHINXOPTS='-t administrate' -v $$(pwd):/mnt $(DOCKER_USER)/$(DOCKER_IMAGE):$(DOCKER_TAG) make clean html
	cd build && zip -r administration-wire-$$(date +"%Y-%m-%d").zip html

.PHONY: exec
exec:
	docker run -it -v $(MKFILE_DIR):/mnt $(DOCKER_USER)/$(DOCKER_IMAGE):$(DOCKER_TAG)

.PHONY: docker
docker:
	docker build -t $(DOCKER_USER)/$(DOCKER_IMAGE):$(DOCKER_TAG) $(MKFILE_DIR)

.PHONY: push
push:
	aws s3 sync $(BUILDDIR)/html s3://origin-docs.wire.com/

.PHONY: dev-install
dev-install:
	python3 -m venv --copies --clear $(VENV_DIR)
	$(VENV_BIN)/pip3 install sphinx sphinx-autobuild recommonmark

.PHONY: dev-run
dev-run: export PATH := $(VENV_BIN):$(PATH)
dev-run:
	rm -rf "$(BUILDDIR)"
	sphinx-autobuild \
		--port 3000 \
		--host 127.0.0.1 \
		-b html \
		$(SPHINXOPTS) \
		"$(SOURCEDIR)" "$(BUILDDIR)"

.PHONY: dev-build
dev-build: export PATH := $(VENV_BIN):$(PATH)
dev-build:
	make clean html

.PHONY: help
help:
	@$(SPHINXBUILD) -M help "$(SOURCEDIR)" "$(BUILDDIR)" $(SPHINXOPTS)

# Catch-all target: route all unknown targets to Sphinx. This "converts" unknown targets into sub-commands (or more precicly
# into `buildername`) of the $(SPHINXBUILD) CLI (see https://www.gnu.org/software/make/manual/html_node/Last-Resort.html).
%:
	@$(SPHINXBUILD) -M $@ "$(SOURCEDIR)" "$(BUILDDIR)" $(SPHINXOPTS)
