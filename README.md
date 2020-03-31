# Wire-docs

Source files for wire-server documentation.

## Reading the documentation

Visit https://docs.wire.com/


## Making contributions

The structure of this document has been heavily inspired by [this blog
post](https://www.divio.com/blog/documentation/).

We use [sphinx](http://sphinx-doc.org/) for rendering.  Here is a [cheat
sheet](http://docutils.sourceforge.net/docs/user/rst/quickref.html)
for writing re-structured text (`*.rst`).
[here is another one](http://docutils.sourceforge.net/docs/user/rst/cheatsheet.html).

Generate docs (using docker, so you don't need to install python dependencies yourself)

```
make
```

You can also install the dependencies (requires `python3`) locally with `make dev-install` and
enter a *development mode* by executing `make dev-srun` to start a local server and file watcher.
Alternatively, if you already have all python dependencies installed globally, run `make html`.

Look at results by opening build/html/index.html

## For maintainers (Wire employees)

### Upload to S3

CI is set up to do this automatically on a push to master. If for some reason you wish to upload manually to S3:

(You need amazon credentials for pushing to S3)

```
make push
```

Please note that cloudfront CDN has a certain cache duration (at the time of writing: 1 minute), so changes will take a bit of time to take effect.
