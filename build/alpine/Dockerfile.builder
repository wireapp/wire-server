# Requires docker >= 17.05 (requires support for multi-stage builds)

ARG prebuilder=quay.io/wire/alpine-prebuilder

FROM ${prebuilder}
WORKDIR /src/wire-server

# Download stack indices and compile/cache dependencies to speed up subsequent
# container creation.
#
# We do a full 'stack build' to get dependencies like 'aws' to build, but then
# we do a 'stack clean' so that packages in the repo wouldn't get into the final
# image – that's because Stack sometimes doesn't realize that local packages
# should be recompiled, and we really don't want that to happen during CI.
RUN apk add --no-cache git ncurses && \
    mkdir -p /src && cd /src && \
    git clone https://github.com/wireapp/wire-server.git && \
    cd wire-server && \
    stack update && \
    stack upgrade && \
    echo -e "allow-different-user: true\nwork-dir: .stack-docker" >> /root/.stack/config.yaml && \
    stack build --pedantic --haddock --test --dependencies-only --no-run-tests
