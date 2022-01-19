# Overview

To create docker images, you need to install [docker version >= 17.05](https://www.docker.com/) and [`make`](https://www.gnu.org/software/make/).

* `Dockerfile.builder` contains all the compile-time dependencies necessary to compile any of the Haskell services (it also downloads, builds and caches some Haskell libraries). This image is fairly large, ~1.7GB compressed.
* `Dockerfile.deps` contains all the run-time dependencies e.g. shared libraries, at a total size of ~52MB compressed.

Both of the above need to be built first (only once) to be able to actually build a service docker image.

* `Dockerfile.intermediate` - based on `Dockerfile.deps`/`Dockerfile.builder`, this is an intermediate image compiling all dynamically linked binaries (obtained when running `make install` in the top-level directory).
* `Dockerfile.executable` - based on `Dockerfile.deps`/`Dockerfile.intermediate`, this extracts a single executable from the intermediate image, yielding a small image (~30MB compressed) with a single dynamically linked binary.

### Build the `builder` and `deps` docker images locally

(from within the `wire-server` directory)
```bash
make docker-builder
make docker-deps
```

### Build a service docker image, e.g. brig:

```bash
make docker-intermediate # recompiles all the haskell code
make docker-exe-brig # this only extracts one binary from the intermediate image above and makes it the default entrypoint. Nothing gets recompiled
```

## Other dockerfiles

* `Dockerfile.prebuilder` - dependencies of `Dockerfile.builder` that are expected to change very rarely (GHC, system libraries).
