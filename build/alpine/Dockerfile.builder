# Requires docker >= 17.05 (requires support for multi-stage builds)

FROM alpine:3.7 as cryptobox-builder

# compile cryptobox-c
RUN apk add --no-cache cargo libsodium-dev git && \
    cd /tmp && \
    git clone https://github.com/wireapp/cryptobox-c.git && \
    cd cryptobox-c && \
    cargo build --release

FROM alpine:3.7

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
        libsodium-dev \
        openssl-dev \
        protobuf \
        icu-dev \
        geoip-dev \
        snappy-dev \
        llvm-libunwind-dev \
        bash \
        xz

# get static version of Haskell Stack and use system ghc by default
ARG STACK_VERSION=1.6.3
RUN curl -sSfL https://github.com/commercialhaskell/stack/releases/download/v${STACK_VERSION}/stack-${STACK_VERSION}-linux-x86_64-static.tar.gz \
    | tar --wildcards -C /usr/local/bin --strip-components=1 -xzvf - '*/stack' && chmod 755 /usr/local/bin/stack && \
    stack config set system-ghc --global true

# As done by https://github.com/TerrorJack/meikyu,
# Install packages needed for newer version of GHC
WORKDIR /root
ENV LANG en_US.UTF-8
ENV GHC_REV ghc-8.2.2-release
ENV GHC_VER ghc-8.2.2
ENV PATH /root/.local/bin:/root/.cabal/bin:/root/.stack/programs/x86_64-linux/$GHC_VER/bin:$PATH
ADD ghc/build.mk ghc/config.yaml /tmp/
RUN stack --no-terminal --resolver lts-9 --system-ghc install \
        alex \
        happy \
        hscolour && \
    apk add --no-cache --no-progress \
    autoconf \
    automake \
    binutils-gold \
    bzip2 \
    ca-certificates \
    coreutils \
    file \
    findutils \
    g++ \
    gawk \
    gcc \
    ghc \
    git \
    gmp-dev \
    gzip \
    libffi-dev \
    make \
    musl-dev \
    ncurses-dev \
    openssh \
    patch \
    perl \
    py3-sphinx \
    sed \
    tar \
    zlib-dev

# Install newer version of GHC
RUN cd /tmp && \
    git clone git://git.haskell.org/ghc.git && \
    cd ghc && \
    git checkout $GHC_REV && \
    git submodule update --init --recursive && \
    mv /tmp/build.mk mk/

RUN cd /tmp/ghc && \
    ./boot && \
    SPHINXBUILD=/usr/bin/sphinx-build-3 ./configure --prefix=/root/.stack/programs/x86_64-linux/$GHC_VER --disable-ld-override && \
    echo "compiling GHC, may take an hour. Log output sent to /dev/null due to travis log length restrictions." && \
    make -j4 &> /dev/null && \
    make install &> /dev/null && \
    mv /tmp/config.yaml /root/.stack/

# download stack indices and compile/cache dependencies to speed up subsequent container creation
# TODO: make this caching step optional?
RUN apk add --no-cache git && \
    mkdir -p /src && cd /src && \
    git clone https://github.com/wireapp/wire-server.git && \
    cd wire-server && \
    stack update && \
    cd services/proxy && stack build --pedantic --test --dependencies-only && cd - && \
    cd services/brig && stack build --pedantic --test --dependencies-only && cd - && \
    cd services/galley && stack build --pedantic --test --dependencies-only && cd - && \
    cd services/cannon && stack build --pedantic --test --dependencies-only && cd - && \
    cd services/cargohold && stack build --pedantic --test --dependencies-only && cd - && \
    cd services/gundeck && stack build --pedantic --test --dependencies-only && cd -
