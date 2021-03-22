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
version = "0.0.1"


# -- General configuration ---------------------------------------------------

# Add any Sphinx extension module names here, as strings. They can be
# extensions coming with Sphinx (named 'sphinx.ext.*') or your custom
# ones.
extensions = [
    'rst2pdf.pdfbuilder'
]

# Grouping the document tree into PDF files. List of tuples
# (source start file, target name, title, author, options).
pdf_documents = [
    # ('index', 'all-docs', 'All the Wire Docs', 'Wire Swiss GmbH'),
    # ('understand/index', 'understand', 'Understand', 'Wire Swiss GmbH')
    ('understand/federation/index', 'wire_federation', 'Wire Federation', 'Wire Swiss GmbH')
]


# Add section number to section
referencespdf_use_numbered_links = True

# see https://rst2pdf.org/static/manual.pdf for more pdf configuration options

# Add any paths that contain templates here, relative to this directory.
templates_path = ['_templates']

# The master toctree document.
master_doc = 'index'

# List of patterns, relative to source directory, that match files and
# directories to ignore when looking for source files.
# This pattern also affects html_static_path and html_extra_path.
exclude_patterns = [ '**.inc.rst' ]
if tags.has('administrate'):
    exclude_patterns = ['**/*single*/**', '**/*install*/**', 'understand/**']

# -- Options for HTML output -------------------------------------------------

# The theme to use for HTML and HTML Help pages.  See the documentation for
# a list of builtin themes.
#
html_theme_path = ['_themes']
html_theme = 'wire-theme'

# Add any paths that contain custom static files (such as style sheets) here,
# relative to this directory. They are copied after the builtin static files,
# so a file named "default.css" will overwrite the builtin "default.css".
html_static_path = ['_static']
