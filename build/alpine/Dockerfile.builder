# Requires docker >= 17.05 (requires support for multi-stage builds)

FROM wireserver/alpine-prebuilder

# download stack indices and compile/cache dependencies to speed up subsequent container creation
RUN apk add --no-cache git && \
    mkdir -p /src && cd /src && \
    git clone https://github.com/wireapp/wire-server.git && \
    cd wire-server && \
    stack update && \
    stack build --pedantic --haddock --test --no-run-tests
