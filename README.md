Check back later, work in progress...


## For maintainers of this repo

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

All-in one

```
make docs push
```
