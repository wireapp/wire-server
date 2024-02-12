# Wire-docs

Source files for wire-server documentation hosted on https://docs.wire.com

## Reading the documentation

Visit https://docs.wire.com/

## Making contributions

The structure of this document has been heavily inspired by [this blog
post](https://www.divio.com/blog/documentation/).

We use [sphinx](https://www.sphinx-doc.org/) for rendering documentation.

Most documentation is written in RestructuredText (with `.rst` file extension).
The reason for that is A) the default support from sphinx and B) some of the
features of RST such as includes, "typesafe" ("well, giving warnings on
compiling when broken") cross-linking to another section in a different file,
etc.

For dealing with RST, here are some resources:

* here is a [cheat sheet](https://docutils.sourceforge.io/docs/user/rst/quickref.html)
* [here is another one](https://docutils.sourceforge.io/docs/user/rst/cheatsheet.html).
* And [another one](https://sublime-and-sphinx-guide.readthedocs.io/en/latest/references.html).

At the popular request, there is now also some support for markdown files (`.md` file
extension), provided by [myst-parser](https://myst-parser.readthedocs.io).

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

Assuming you're set up for wire-server development, you should already have Nix
and Direnv installed.

This folder contains another `.envrc` file that adds all the binaries needed to
build the docs to `$PATH`.

In short, when you `cd` into this folder, you should see this message:

```sh
direnv: error wire-server/docs/.envrc is blocked. Run `direnv allow` to approve its content
```

Run `direnv allow` to allow the `.envrc` file to modify your environment. Then, you should have everything (binaries, environment variables) needed to build the docs.

### Generating html output (one-off)

```
make html
```

### Generating html output continuously (recommended) with file watching

Enter a *development mode* by running

```
make dev-run
```

to start a local server and file watcher. Then, point your browser at `http://localhost:3000`.

### Generating a PDF file

NOTE: support is experimental and resulting pdf may not have great formatting. See the [rst2pdf](https://rst2pdf.org/static/manual.pdf) manual to improve the configuration here so the resulting PDF becomes nicer.

Run `make pdf` and look at files in `./build/pdf/`.

You can use the `make dev-pdf` target to get auto-refreshing PDF files as you save source files. This is also acessible at `http://localhost:3000/wire_federation.pdf`.

### Testing CI build locally

In order to test changes to the deployment process (eg. nix changes, new grepinclude defaults), the build process can be invoked locally with

```
nix-build --no-out-link ./nix -A docs
```

If the command succeeds, the static content can be viewed in the browser directly from the nix build dir (adjust the build path)

```
firefox /nix/store/isjbzhmm34kr1i1xdgwfrrn98s4hgj43-wire-docs/html/index.html
```


### Upload to S3

CI is set up to do this automatically on a push to master. If for some reason you wish to upload manually to S3:

(You need amazon credentials for pushing to S3)

```
make push
```

Please note that cloudfront CDN has a certain cache duration (at the time of writing: 1 minute), so changes will take a bit of time to take effect.
