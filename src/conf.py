# Configuration file for the Sphinx documentation builder.
#
# This file only contains a selection of the most common options. For a full
# list see the documentation:
# http://www.sphinx-doc.org/en/master/config

# -- Path setup --------------------------------------------------------------

# If extensions (or modules to document with autodoc) are in another directory,
# add these directories to sys.path here. If the directory is relative to the
# documentation root, use os.path.abspath to make it absolute, like shown here.
#
# import os
# import sys
# sys.path.insert(0, os.path.abspath('.'))


# -- Project information -----------------------------------------------------

project = 'Wire'
copyright = '2021, Wire'
author = 'Wire Swiss GmbH'
version = '0.0.3'
# the 'release' variable is used in latex-based PDF generation
release = version


# -- General configuration ---------------------------------------------------

# Add any Sphinx extension module names here, as strings. They can be
# extensions coming with Sphinx (named 'sphinx.ext.*') or your custom
# ones.
extensions = [
    'sphinxcontrib.kroki',
    'rst2pdf.pdfbuilder',
    'sphinx_multiversion'
]

# Grouping the document tree into PDF files. List of tuples
# (source start file, target name, title, author, options).
pdf_documents = [
    # ('index', 'all-docs', 'All the Wire Docs', 'Wire Swiss GmbH'),
    # ('understand/index', 'understand', 'Understand', 'Wire Swiss GmbH')
    ('understand/federation/index', 'wire_federation', 'Wire Federation', 'Wire Swiss GmbH')
]

latex_documents = [
    ('understand/federation/index', 'main.tex', 'Wire Federation', 'Wire Swiss GmbH', 'howto', 'False')
]




# Add section number to section
referencespdf_use_numbered_links = True

pdf_fit_mode = "shrink"

# see https://rst2pdf.org/static/manual.pdf for more pdf configuration options

# Add any paths that contain templates here, relative to this directory.
templates_path = ['_templates']

html_sidebars = {
    # instead of a wildcard **, a regex could optionally
    # show the version sidebar only on some pages but not all of them.
    '**': ['versioning.html', 'globaltoc.html', 'sourcelink.html', 'searchbox.html'],
}


# The master toctree document.
master_doc = 'index'

# List of patterns, relative to source directory, that match files and
# directories to ignore when looking for source files.
# This pattern also affects html_static_path and html_extra_path.
exclude_patterns = [ '**.inc.rst' ]
if tags and tags.has('administrate'):
    exclude_patterns = ['**/*single*/**', '**/*install*/**', 'understand/**']

# -- Options for HTML output -------------------------------------------------

# The theme to use for HTML and HTML Help pages.  See the documentation for
# a list of builtin themes.
#
html_theme = 'sphinx_rtd_theme'

# Add any paths that contain custom static files (such as style sheets) here,
# relative to this directory. They are copied after the builtin static files,
# so a file named "default.css" will overwrite the builtin "default.css".
html_static_path = ['_static']


smv_tag_whitelist = ''
smv_branch_whitelist = r'^(install-with-poetry)$'
smv_remote_whitelist = r'^(origin)$'
smv_released_pattern = r'^remotes/.+$'

smv_outputdir_format = 'versions/{ref.name}'
smv_prefer_remote_refs = True
