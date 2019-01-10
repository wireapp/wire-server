# Requires docker >= 17.05 (requires support for multi-stage builds)

ARG prebuilder=quay.io/wire/alpine-prebuilder

FROM ${prebuilder}
WORKDIR /src/wire-server

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
    mkdir -p /src && cd /src && \
    git clone -b develop https://github.com/wireapp/wire-server.git && \
    cd wire-server && \
    stack update && \
    echo "allow-different-user: true"                                       >> /root/.stack/config.yaml && \
    echo                                                                    >> /root/.stack/config.yaml && \
    echo '# NB: do not touch following line!'                               >> /root/.stack/config.yaml && \
    echo '# this image is used both for building docker images with the'    >> /root/.stack/config.yaml && \
    echo '# integration tests (so they can be run on the ci) and for'       >> /root/.stack/config.yaml && \
    echo '# interactive integration testing (with the working copy of the'  >> /root/.stack/config.yaml && \
    echo '# host system mounted into the docker container).  in the latter' >> /root/.stack/config.yaml && \
    echo '# use case, we want the docker container to write to its own'     >> /root/.stack/config.yaml && \
    echo '# stack-work directory and not pollute the one on the host.'      >> /root/.stack/config.yaml && \
    echo 'work-dir: .stack-docker'                                          >> /root/.stack/config.yaml && \
    stack --work-dir .stack-docker-profile build --haddock --dependencies-only --profile haskell-src-exts && \
    stack --work-dir .stack-docker         build --haddock --dependencies-only           haskell-src-exts && \
    stack --work-dir .stack-docker-profile build --haddock --no-haddock-hyperlink-source --profile haskell-src-exts && \
    stack --work-dir .stack-docker         build --haddock --no-haddock-hyperlink-source           haskell-src-exts && \
    stack --work-dir .stack-docker-profile build --pedantic --haddock --test --no-run-tests --bench --no-run-benchmarks --dependencies-only --profile && \
    stack --work-dir .stack-docker         build --pedantic --haddock --test --no-run-tests --bench --no-run-benchmarks --dependencies-only
