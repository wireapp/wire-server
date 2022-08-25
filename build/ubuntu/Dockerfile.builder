ARG prebuilder=quay.io/wire/ubuntu20-prebuilder

FROM rust:1.63 as mls-test-cli-builder

# compile mls-test-cli tool
RUN cd /tmp && \
    git clone https://github.com/wireapp/mls-test-cli && \
    cd mls-test-cli && \
    git rev-parse HEAD

RUN cd /tmp/mls-test-cli && RUSTFLAGS='-C target-feature=+crt-static' cargo build --release --target x86_64-unknown-linux-gnu

FROM ${prebuilder}

COPY --from=mls-test-cli-builder /tmp/mls-test-cli/target/x86_64-unknown-linux-gnu/release/mls-test-cli /usr/bin/mls-test-cli

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
