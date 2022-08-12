SHELL = bash

.DEFAULT_GOAL := docs-all

MKFILE_DIR = $(abspath $(dir $(abspath $(lastword $(MAKEFILE_LIST)))))

# You can set these variables (with a ?=) from the command line, and also
# from the environment.
SPHINXOPTS    ?= -q
SPHINXBUILD   ?= sphinx-build
SOURCEDIR     = $(MKFILE_DIR)/src
BUILDDIR      = $(MKFILE_DIR)/build

.PHONY: Makefile

.PHONY: docs
docs: clean html

.PHONY: docs-pdf
docs-pdf: clean pdf

.PHONY: docs-latex
docs-latex: clean latex

.PHONY: docs-all
docs-all: clean html pdf

.PHONY: clean
clean:
	rm -rf "$(BUILDDIR)"

# Only build part of the documentation
# See 'exclude_patterns' in source/conf.py
docs-administrate:
	SPHINXOPTS='-t administrate' make clean html

.PHONY: push
push:
	aws s3 sync $(BUILDDIR)/html s3://origin-docs.wire.com/
	[[ -f  $(BUILDDIR)/latex/main.pdf ]] && aws s3 cp $(BUILDDIR)/latex/main.pdf s3://origin-docs.wire.com/main.pdf || exit 0

.PHONY: dev-run
dev-run: clean
	sphinx-autobuild \
		--port 3000 \
		--host 127.0.0.1 \
		-b html \
		$(SPHINXOPTS) \
		"$(SOURCEDIR)" "$(BUILDDIR)"

.PHONY: dev-pdf
dev-pdf: clean
	sphinx-autobuild \
		--port 3000 \
		--host 127.0.0.1 \
		-b pdf \
		$(SPHINXOPTS) \
		"$(SOURCEDIR)" "$(BUILDDIR)"

# Catch-all target: route all unknown targets to Sphinx. This "converts" unknown targets into sub-commands (or more precicly
# into `buildername`) of the $(SPHINXBUILD) CLI (see https://www.gnu.org/software/make/manual/html_node/Last-Resort.html).
%:
	$(SPHINXBUILD) -M $@ "$(SOURCEDIR)" "$(BUILDDIR)" $(SPHINXOPTS)
