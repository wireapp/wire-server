# Requires docker >= 17.05 (requires support for multi-stage builds)

ARG prebuilder=quay.io/wire/alpine-prebuilder

FROM ${prebuilder}
WORKDIR /

# Download stack indices and compile/cache dependencies to speed up subsequent
# container creation.
#
# We also build profiling versions of all libraries. Due to a bug in Stack,
# they have to be built in a separate directory. See this issue:
# https://github.com/commercialhaskell/stack/issues/4032
#
# Finally, we build docs for haskell-src-exts without hyperlinking enabled
# to avoid a Haddock segfault. See https://github.com/haskell/haddock/issues/928

RUN apk add --no-cache git ncurses && \
    git clone -b develop https://github.com/wireapp/wire-server.git && \ # Clone into /wire-server
    cd /wire-server && \
    stack update && \
    echo "allow-different-user: true" >> /root/.stack/config.yaml && \
    stack build --haddock --dependencies-only --profile haskell-src-exts && \
    stack build --haddock --no-haddock-hyperlink-source --profile haskell-src-exts && \
    stack build --pedantic --haddock --test --no-run-tests --bench --no-run-benchmarks --dependencies-only --profile && \
    cd / && \
    # we run the build only to cache the built source in /root/.stack, we can remove the source code itself
    rm -rf /wire-server
