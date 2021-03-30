FROM nixos/nix AS builder

RUN set -e -x ;\
    apk add --no-cache bash git

COPY . /wire-docs

RUN nix-env -f /wire-docs/nix/default.nix -iA env && \
    rm -rf /wire-docs

WORKDIR /mnt

ENV USE_POETRY=0
