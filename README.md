Check back later, work in progress...


## Reading the documentation

Visit https://docs.wire.com/


## Making contributions

The structure of this document has been heavily inspired by [this blog
post](https://www.divio.com/blog/documentation/).

We use [sphinx](http://sphinx-doc.org/) for rendering.  Here is a [cheat
sheet](http://docutils.sourceforge.net/docs/user/rst/quickref.html)
for writing re-structured text (`*.rst`).

Generate docs (using docker, so you don't need to install python dependencies yourself)

```
make
```

(Alternatively, if you have python dependencies installed, run `make html`).


Look at results by opening build/html/index.html

Upload to S3

```
make push
```

(You need wire credentials for pushing to S3.  If you don't have any,
just [open a PR](https://github.com/wireapp/wire-docs/).)

All-in one

```
make docs push
```
