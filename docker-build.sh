#/bin/bash

docker build \
    -f ./build/alpine/Dockerfile.executable \
    --build-arg executable=brig-schema \
    -t docker-build \
    .
