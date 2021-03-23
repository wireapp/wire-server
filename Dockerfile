FROM nixos/nix AS builder

RUN set -e -x ;\
    apk add --no-cache bash git

COPY . /wire-docs

RUN nix-env -f /wire-docs/nix/default.nix -iA env && \
    rm -rf /wire-docs

SHELL ["/bin/bash", "-c"]
ENTRYPOINT "/bin/bash"

ENV USE_POETRY=0
