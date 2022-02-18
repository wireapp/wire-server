# Wire-docs

Source files for wire-server documentation hosted on https://docs.wire.com

## Reading the documentation

Visit https://docs.wire.com/

## Making contributions

The structure of this document has been heavily inspired by [this blog
post](https://www.divio.com/blog/documentation/).

We use [sphinx](https://sphinx-doc.org/) for rendering documentation.

Most documentation is written in RestructuredText (with `.rst` file extension).
The reason for that is A) the default support from sphinx and B) some of the
features of RST such as includes, "typesafe" ("well, giving warnings on
compiling when broken") cross-linking to another section in a different file,
etc.

For dealing with RST, here are some resources:

* here is a [cheat sheet](https://docutils.sourceforge.net/docs/user/rst/quickref.html)
* [here is another one](https://docutils.sourceforge.net/docs/user/rst/cheatsheet.html).
* And [another one](https://sublime-and-sphinx-guide.readthedocs.io/en/latest/references.html).

At the popular request, there is now also some support for markdown files (`.md` file
extension). Note that this is [commonmark](https://spec.commonmark.org/0.30/) which is
different to e.g.  github-flavoured markdown. See
[recommonmark](https://recommonmark.readthedocs.io/en/latest/) for the
currently-supported markdown.

At this point, a documentation contributor creating a new file can choose to use
commonmark markdown as the file format. Please hold off converting any existing files to
a different format until better markdown support (`myst_parser` support, see
[MyST](https://myst-parser.readthedocs.io/en/latest/)) has been added to wire-docs and
pros and cons of the formats have been internally evaluated.

### Conventions

The re-structured spec text allows for choosing any underline/overline symbol
for any level. In this repository we have not been very consistent. For any new
contribution let's stick to this convention:

```rst
######
Part 1
######

*********
Chapter 1
*********

Section 1
=========

Sub-section 1
-------------

Sub-sub-section 1
^^^^^^^^^^^^^^^^^

Paragraph 1
~~~~~~~~~~~

Sub-paragraph 1
+++++++++++++++
```

If another level is needed, please add the chosen symbol here.

## Building the docs

### Dependencies

Install the dependencies locally:

1. Install [Nix](https://nixos.org/download.html)
   * MacOS users with a recent Mac might need to follow [these
   instructions](https://nixos.org/nix/manual/#sect-macos-installation)
   * Debian users can use their distro's `nix` package, and should remember
   to add their user to the `nix-users` group in /etc/group, and re-start
   their login session.
2. Install [Direnv](https://direnv.net/).
   * On debian, you can install the `direnv` package. On MacOS use `brew install direnv`.
   * On NixOS with home-manager, you can set `programs.direnv.enable = true;`.
   * Make sure direnv is hooked into your shell via it's appripriate `rc` file.
     Add `eval "$(direnv hook bash|zsh|fish)"` to your ~/.(bash|zsh|fish)rc .
   * When successfully installed and hooked, direnv should ask you to `direnv allow`
     the current `.envrc` when you cd to this repository.
     See the [Installation documentation](https://direnv.net/docs/installation.html) for further details.

Now, whenever you cd to wire-docs, you will have the relevant binaries (make, sphinx, rst2pdf, ...) in your PATH.

### Generating html output (one-off)

```
make html
```

### Generating html output continuously (recommended) with file watching

Enter a *development mode* by running

```
make dev-run
```

to start a local server and file watcher. Then, point your browser at `http://localhost:3000`. (or, alternatively, look at results by opening `build/html/index.html`) which will auto-update whenever files under `./src` change.

### Generating a PDF file

NOTE: support is experimental and resulting pdf may not have great formatting. See the [rst2pdf](https://rst2pdf.org/static/manual.pdf) manual to improve the configuration here so the resulting PDF becomes nicer.

Run `make pdf` and look at files in `./build/pdf/`.

You can use the `make dev-pdf` target to get auto-refreshing PDF files as you save source files. (requires a PDF viewer installed globally)

<details>

<summary> Alternative ways to build the documentation for preview without nix+direnv </summary>

*Note: when switching from a docker-based building to a local building, you might encounter permission issues due to the build directory being owned by root. These can be solved by cleaning the build directory: `sudo rm -rf ./build/*`

## Building the docs with docker

You need `docker` available on your system.
The docker image that is used is defined in the `Makefile`. To build the docker image locally (e.g. after updating dependencies) run `make docker`.

### html

Generate docs using docker (so you don't need to install python dependencies yourself)

```
make docs
```

See build/html/index.html

### pdf

```
make docs-pdf
```

Then see build/pdf/

</details>

## For maintainers (Wire employees)

### CI/CD configuration

On a Pull request, the script inside [pr.yml](ci/pr.yml) will run. On a commit to master, the script inside [compile-and-upload.yml](ci/compile-and-upload.yml) will run.

The actual concourse pipeline doing so is configured in [this private repository only available for Wire employees](https://github.com/zinfra/cailleach/blob/master/wire-server-private/ci/pipelines/wire_docs.yml)

### Upload to S3

CI is set up to do this automatically on a push to master. If for some reason you wish to upload manually to S3:

(You need amazon credentials for pushing to S3)

```
make push
```

Please note that cloudfront CDN has a certain cache duration (at the time of writing: 1 minute), so changes will take a bit of time to take effect.
