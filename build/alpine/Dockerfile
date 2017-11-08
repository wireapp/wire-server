# Requires docker >= 17.05 (requires support for multi-stage builds)
# Requires to have created the wire-server-builder and wire-server-deps docker images (run `make` in this directory)
# Usage example:
#   (from wire-server root directory)
#   SERVICE=galley; docker build -f build/alpine/Dockerfile -t $SERVICE --build-arg service=$SERVICE .

#--- Builder stage ---
FROM wire-server-builder:alpine as builder

ARG service
COPY . /src/wire-server/

RUN cd /src/wire-server/services/${service} && make install

#--- Minified stage ---
FROM wire-server-deps:alpine

ARG service
ARG executable
COPY --from=builder /src/wire-server/services/${service}/dist/${executable} /usr/bin/${exectuable}

# ARGs are not available at runtime, create symlink at build time
# more info: https://stackoverflow.com/questions/40902445/using-variable-interpolation-in-string-in-docker
RUN ln -s /usr/bin/${executable} /usr/bin/service
ENTRYPOINT ["/usr/bin/dumb-init", "--", "/usr/bin/service"]
