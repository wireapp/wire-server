# Requires docker >= 17.05 (requires support for multi-stage builds)
# Requires to have created the wire-server-builder and wire-server-deps docker images (run `make` in this directory)
# Usage example:
#   (from wire-server root directory)
#   SERVICE=galley; docker build -f build/alpine/Dockerfile -t $SERVICE --build-arg service=$SERVICE .

ARG builder=wireserver/alpine-builder
ARG deps=wireserver/alpine-deps

#--- Builder stage ---
FROM ${builder} as builder

ARG service
ARG target=install

# FIXME: this merges two directories (the one from the builder and the local
# one), which can lead to problems such as
# https://github.com/wireapp/wire-server/pull/332#pullrequestreview-122162356.
COPY . /src/wire-server/

RUN cd /src/wire-server/services/${service} && make ${target}

#--- Minified stage ---
FROM ${deps}

ARG executable
COPY --from=builder /src/wire-server/dist/${executable} /usr/bin/${exectuable}

# ARGs are not available at runtime, create symlink at build time
# more info: https://stackoverflow.com/questions/40902445/using-variable-interpolation-in-string-in-docker
RUN ln -s /usr/bin/${executable} /usr/bin/service
ENTRYPOINT ["/usr/bin/dumb-init", "--", "/usr/bin/service"]
