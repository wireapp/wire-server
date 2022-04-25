ARG prebuilder=quay.io/wire/ubuntu20-prebuilder

FROM ${prebuilder}
WORKDIR /

# Download stack indices and compile/cache dependencies to speed up subsequent
# container creation.
#
# We build docs for haskell-src-exts without hyperlinking enabled to avoid
# a Haddock segfault. See https://github.com/haskell/haddock/issues/928

ARG wire_server_branch=develop
ARG THREADS=4
RUN set -x && \
    echo ${wire_server_branch} && \
    git clone -b ${wire_server_branch} https://github.com/wireapp/wire-server.git && \
    cd /wire-server && \
    cabal update && \
    cabal build all --dependencies-only && \
    cd / && \
    # we run the build only to cache the built source in /root/.cabal, we can remove the source code itself
    rm -rf /wire-server
