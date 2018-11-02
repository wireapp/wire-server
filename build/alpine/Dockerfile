# Requires docker >= 17.05 (requires support for multi-stage builds)
# Requires to have created the wire-server-builder and wire-server-deps docker images (run `make` in this directory)
# Usage example:
#   (from wire-server root directory)
#   SERVICE=galley; docker build -f build/alpine/Dockerfile -t $SERVICE --build-arg service=$SERVICE .

ARG builder=quay.io/wire/alpine-builder
ARG deps=quay.io/wire/alpine-deps

#--- Builder stage ---
FROM ${builder} as builder

ARG service
ARG target=install

# ensure no stale files remain if they get deleted from the branch.
RUN find /src/wire-server/ -maxdepth 1 -mindepth 1 | grep -v .stack- | xargs rm -rf

COPY . /src/wire-server/

RUN cd /src/wire-server/services/${service} && make ${target}

#--- Minified stage ---
FROM ${deps}

ARG executable
COPY --from=builder /src/wire-server/dist/${executable} /usr/bin/${executable}

# ARGs are not available at runtime, create symlink at build time
# more info: https://stackoverflow.com/questions/40902445/using-variable-interpolation-in-string-in-docker
RUN ln -s /usr/bin/${executable} /usr/bin/service
ENTRYPOINT ["/usr/bin/dumb-init", "--", "/usr/bin/service"]
