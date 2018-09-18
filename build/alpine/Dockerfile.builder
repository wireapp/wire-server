# Requires docker >= 17.05 (requires support for multi-stage builds)

ARG prebuilder=quay.io/wire/alpine-prebuilder

FROM ${prebuilder}
WORKDIR /src/wire-server

# Get newer Stack

# later versions don't have static binaries (temporarily)
ARG STACK_VERSION=1.6.5
RUN curl -sSfL https://github.com/commercialhaskell/stack/releases/download/v${STACK_VERSION}/stack-${STACK_VERSION}-linux-x86_64-static.tar.gz \
    | tar --wildcards -C /usr/local/bin --strip-components=1 -xzvf - '*/stack' && chmod 755 /usr/local/bin/stack

# Download stack indices and compile/cache dependencies to speed up subsequent
# container creation.
#
# We also build profiling versions of all libraries. Due to a bug in Stack,
# they have to be built in a separate directory. See this issue:
# https://github.com/commercialhaskell/stack/issues/4032

RUN apk add --no-cache git ncurses libxml2-dev && \
    mkdir -p /src && cd /src && \
    git clone -b develop https://github.com/wireapp/wire-server.git && \
    cd wire-server && \
    stack update && \
    echo -e "allow-different-user: true\n" >> /root/.stack/config.yaml && \
    stack --work-dir .stack-docker-profile build --pedantic --haddock --test --dependencies-only --no-run-tests --profile && \
    stack --work-dir .stack-docker         build --pedantic --haddock --test --dependencies-only --no-run-tests
