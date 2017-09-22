# Requires docker >= 17.05 (requires support for multi-stage builds)
# Requires to have created the wire-server-builder and wire-server-deps docker images (run `make` in this directory)
# Usage example:
#   (from wire-server root directory)
#   SERVICE=galley; docker build -f build/alpine/Dockerfile -t $SERVICE --build-arg service=$SERVICE .

#--- Builder stage ---
FROM wire-server-builder:alpine as builder

ARG service
COPY . /src

RUN cd /src/services/${service} && make install

#--- Minified stage ---
FROM wire-server-deps:alpine

ARG service
COPY --from=builder /src/services/${service}/dist/${service} /usr/local/bin/${service}

# ARGs are not available at runtime, create symlink at build time
# more info: https://stackoverflow.com/questions/40902445/using-variable-interpolation-in-string-in-docker
RUN ln -s /usr/local/bin/${service} /usr/local/bin/service
ENTRYPOINT ["/usr/local/bin/service"]
