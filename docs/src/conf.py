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
import os
import sys

# unset SOURCE_DATE_EPOCH to aviod side effects caused by sphinx
if 'SOURCE_DATE_EPOCH' in os.environ:
    del os.environ['SOURCE_DATE_EPOCH']

# -- Project information -----------------------------------------------------

project = 'Wire'
author = 'Wire Swiss GmbH'
copyright = f'2019 - 2023, Wire Swiss GmbH'
version = '0.0.4'
# the 'release' variable is used in latex-based PDF generation
release = version


# -- General configuration ---------------------------------------------------

sys.path.insert(0, os.path.abspath('.')) # for local extensions like grepinclude

# Add any Sphinx extension module names here, as strings. They can be
# extensions coming with Sphinx (named 'sphinx.ext.*') or your custom
# ones.
extensions = [
    'sphinxcontrib.kroki',
    'sphinxcontrib.plantuml',
    "myst_parser",
    'rst2pdf.pdfbuilder',
    'sphinx_multiversion',
    'sphinx_reredirects',
    'sphinx_copybutton',
    'grepinclude',
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

# NOTE: if you want to fully override default theme css, use this property (presumes it is in _static folder)
#html_style = 'css/wire.css'

# additional css files (presumes they are in _static folder)
html_css_files = [
    'css/wire.css',
]

html_favicon = '_static/favicon/favicon.ico'
html_logo = '_static/image/Wire_logo.svg'

html_context = {
  'display_github': True,
  'github_user': 'wireapp',
  'github_repo': 'wire-server',
  'github_version': 'develop/docs/src/',
}

smv_tag_whitelist = ''
smv_branch_whitelist = r'^(install-with-poetry)$'
smv_remote_whitelist = r'^(origin)$'
smv_released_pattern = r'^remotes/.+$'

smv_outputdir_format = 'versions/{ref.name}'
smv_prefer_remote_refs = True

# As per https://myst-parser.readthedocs.io/en/latest/syntax/optional.html?highlight=anchor#auto-generated-header-anchors
myst_heading_anchors = 4

redirects = {
        "security-responses/log4shell": "2021-12-15_log4shell.html",
        "security-responses/cve-2021-44521": "2022-02-21_cve-2021-44521.html",
        "security-responses/2022-05_website_outage": "2022-05-23_website_outage.html",
        "how-to/single-sign-on/index": "../../understand/single-sign-on/index.html",
        "how-to/scim/index": "../../understand/single-sign-on/main.html#user-provisioning"
}
