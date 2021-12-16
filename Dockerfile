FROM nixos/nix:2.3.12 AS builder

RUN set -e -x ;\
    apk add --no-cache bash git

COPY . /wire-docs

RUN nix-env -f /wire-docs/nix/default.nix -iA env && \
    rm -rf /wire-docs

WORKDIR /mnt
