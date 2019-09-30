DOCKER_USER   ?= quay.io/wire
DOCKER_IMAGE  = alpine-sphinx
DOCKER_TAG    ?= latest

# You can set these variables from the command line, and also
# from the environment for the first two.
SPHINXOPTS    ?=
SPHINXBUILD   ?= sphinx-build
SOURCEDIR     = src
BUILDDIR      = build

.PHONY: help Makefile push docker docs exec

docs:
	docker run --rm -v $$(pwd):/mnt $(DOCKER_USER)/$(DOCKER_IMAGE):$(DOCKER_TAG) make clean html

# Only build part of the documentation
# See 'exclude_patterns' in source/conf.py
docs-administrate:
	docker run --rm -e SPHINXOPTS='-t administrate' -v $$(pwd):/mnt $(DOCKER_USER)/$(DOCKER_IMAGE):$(DOCKER_TAG) make clean html

exec:
	docker run -it -v $$(pwd):/mnt $(DOCKER_USER)/$(DOCKER_IMAGE):$(DOCKER_TAG)

docker:
	docker build -t $(DOCKER_USER)/$(DOCKER_IMAGE):$(DOCKER_TAG) .

push:
	aws s3 sync build/html s3://origin-docs.wire.com/

help:
	@$(SPHINXBUILD) -M help "$(SOURCEDIR)" "$(BUILDDIR)" $(SPHINXOPTS) $(O)

# Catch-all target: route all unknown targets to Sphinx using the new
# "make mode" option.  $(O) is meant as a shortcut for $(SPHINXOPTS).
%: Makefile
	@$(SPHINXBUILD) -M $@ "$(SOURCEDIR)" "$(BUILDDIR)" $(SPHINXOPTS) $(O)
