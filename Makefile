DOCKER_USER   ?= quay.io/wire
DOCKER_IMAGE  = alpine-sphinx
DOCKER_TAG    ?= latest

# You can set these variables from the command line.
SPHINXOPTS    =
SPHINXBUILD   = sphinx-build
SPHINXPROJ    = Wire
SOURCEDIR     = source
BUILDDIR      = build

.PHONY: help Makefile push docker docs

docs:
	docker run --rm -v $$(pwd):/mnt $(DOCKER_USER)/$(DOCKER_IMAGE):$(DOCKER_TAG) make html

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


