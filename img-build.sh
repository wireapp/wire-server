#/bin/bash

docker run  \
    -v "$(pwd)/:/wire-server" \
    -it \
    -v "$(pwd)/image:/image" \
    -e REPOSITORY="img-build" \
    -e TAG="latest" \
    -e DOCKERFILE=/wire-server/build/alpine/Dockerfile.executable \
    -e CONTEXT=/wire-server \
    --privileged \
    --entrypoint /usr/bin/build \
    quay.io/chris_wire/builder:latest 
