# Requires docker >= 17.05 (requires support for multi-stage builds)

ARG prebuilder_tag=latest
ARG prebuilder=quay.io/wire/alpine-prebuilder:${prebuilder_tag}
ARG wire_server_branch=develop

FROM ${prebuilder}
WORKDIR /

# Download stack indices and compile/cache dependencies to speed up subsequent
# container creation.
#
# We build docs for haskell-src-exts without hyperlinking enabled to avoid
# a Haddock segfault. See https://github.com/haskell/haddock/issues/928

RUN apk add --no-cache && \
    git clone -b ${wire_server_branch} https://github.com/wireapp/wire-server.git && \
    cd /wire-server && \
    stack update && \
    echo "allow-different-user: true" >> /root/.stack/config.yaml && \
    stack build --haddock --dependencies-only haskell-src-exts && \
    stack build --haddock --no-haddock-hyperlink-source haskell-src-exts && \
    stack build --pedantic --haddock --test --no-run-tests --bench --no-run-benchmarks --dependencies-only && \
    cd / && \
    # we run the build only to cache the built source in /root/.stack, we can remove the source code itself
    rm -rf /wire-server
