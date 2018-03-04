# Requires docker >= 17.05 (requires support for multi-stage builds)
# Requires to have created the wire-server-builder and wire-server-deps docker images

ARG builder=wireserver/alpine-builder
ARG deps=wireserver/alpine-deps

#--- Builder stage ---
FROM ${builder} as builder

ARG target=install

COPY . /src/wire-server/

RUN cd /src/wire-server/services/brig && make ${target}

#--- Minified stage ---
FROM ${deps}

ARG executable
COPY --from=builder /src/wire-server/services/brig/dist/${executable} /usr/bin/${exectuable}
COPY --from=builder /src/wire-server/services/brig/deb/opt/brig/templates/ /usr/share/wire/templates/

# ARGs are not available at runtime, create symlink at build time
# more info: https://stackoverflow.com/questions/40902445/using-variable-interpolation-in-string-in-docker
RUN ln -s /usr/bin/${executable} /usr/bin/service
ENTRYPOINT ["/usr/bin/dumb-init", "--", "/usr/bin/service"]
