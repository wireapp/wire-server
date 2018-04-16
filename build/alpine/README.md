# Overview

To create docker images, you need to install [docker version >= 17.05](https://www.docker.com/) and [`make`](https://www.gnu.org/software/make/).

* `Dockerfile.builder` contains all the compile-time dependencies necessary to compile any of the Haskell services (it also downloads, builds and caches some Haskell libraries). This image is fairly large, ~4GB uncompressed.
* `Dockerfile.deps` contains all the run-time dependencies e.g. shared libraries, at a total size of ~18MB compressed.

Both of the above need to be built first (only once) to be able to actually build a service docker image.

* `Dockerfile` depends on the two images above to compile code inside the `builder` image and then copy the resulting binary into the `deps` image to finally construct a service (e.g. `brig`) docker image that can be used for testing or deployment. The final image is ~30MB (compressed) in size.

### Build the `builder` and `deps` docker images locally

```bash
cd build/alpine && make
```

### Build a service, e.g. brig:

```bash
cd services/brig && make docker
```

## Other dockerfiles

* `Dockerfile.intermediate` - based on `Dockerfile.deps`/`Dockerfile.builder`, this is an intermediate image compiling all dynamically linked binaries (obtained when running `make install` in the top-level directory).
* `Dockerfile.executable` - based on `Dockerfile.deps`/`Dockerfile.intermediate`, this extracts a single executable from the intermediate image, yielding a small image with a single dynamically linked binary.
* `Dockerfile.migrations` - same as `Dockerfile.executable`, with a fixed set of database migration binaries.
* `Dockerfile.prebuilder` - dependencies of `Dockerfile.builder` that are expected to change very rarely (GHC, system libraries).
