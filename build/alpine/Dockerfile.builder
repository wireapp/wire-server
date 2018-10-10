# Requires docker >= 17.05 (requires support for multi-stage builds)

ARG prebuilder=quay.io/wire/alpine-prebuilder

FROM ${prebuilder}
WORKDIR /src/wire-server

# Get newer Stack

# Download stack indices and compile/cache dependencies to speed up subsequent
# container creation.
#
# We also build profiling versions of all libraries. Due to a bug in Stack,
# they have to be built in a separate directory. See this issue:
# https://github.com/commercialhaskell/stack/issues/4032

RUN apk add --no-cache git ncurses && \
    mkdir -p /src && cd /src && \
    git clone -b develop https://github.com/wireapp/wire-server.git && \
    cd wire-server && \
    stack update && \
    echo -e "allow-different-user: true\nwork-dir: .stack-docker\n" >> /root/.stack/config.yaml && \
    stack --work-dir .stack-docker-profile build --pedantic --haddock --test --dependencies-only --no-run-tests --profile && \
    stack --work-dir .stack-docker         build --pedantic --haddock --test --dependencies-only --no-run-tests
