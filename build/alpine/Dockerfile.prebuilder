# Requires docker >= 17.05 (requires support for multi-stage builds)

FROM alpine:3.8 as cryptobox-builder

# compile cryptobox-c
RUN apk add --no-cache cargo libsodium-dev git && \
    cd /tmp && \
    git clone https://github.com/wireapp/cryptobox-c.git && \
    cd cryptobox-c && \
    cargo build --release

FROM alpine:3.8

# install cryptobox-c in the new container
COPY --from=cryptobox-builder /tmp/cryptobox-c/target/release/libcryptobox.so /usr/lib/libcryptobox.so
COPY --from=cryptobox-builder /tmp/cryptobox-c/src/cbox.h /usr/include/cbox.h

# development packages required for wire-server Haskell services
RUN apk add --no-cache \
        alpine-sdk \
        ca-certificates \
        linux-headers \
        zlib-dev \
        ghc \
        ghc-dev \
        ghc-doc \
        libsodium-dev \
        openssl-dev \
        protobuf \
        icu-dev \
        geoip-dev \
        snappy-dev \
        llvm-libunwind-dev \
        bash \
        xz \
        libxml2-dev \
        git \
        ncurses \
        sed

# get static version of Haskell Stack and use system ghc by default
ARG STACK_ALPINE_VERSION=1.9.1
RUN curl -sSfL https://github.com/commercialhaskell/stack/releases/download/v${STACK_ALPINE_VERSION}/stack-${STACK_ALPINE_VERSION}-linux-x86_64-static.tar.gz \
    | tar --wildcards -C /usr/local/bin --strip-components=1 -xzvf - '*/stack' && chmod 755 /usr/local/bin/stack && \
    stack config set system-ghc --global true

# upgrade stack to current master (2019-03-28).  this fixes an issue
# with building internal libraries as used in cql-io-1.1.0.
# details: https://github.com/commercialhaskell/stack/pull/4596
RUN git clone https://github.com/commercialhaskell/stack && \
    cd stack && \
    git checkout f0b66a1ab60fb5be85f6cb60491915bf53d3cd3c && \
    sed -i -e 's/lts-12.20/lts-12.14/' snapshot-lts-12.yaml && \
    stack install --system-ghc --stack-yaml=stack-lts-12.yaml --system-ghc && \
    cp /root/.local/bin/stack /usr/local/bin/stack
