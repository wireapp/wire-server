#!/usr/bin/env bash

set -euo pipefail

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
TOP_LEVEL="$(cd "$DIR/../.." && pwd)"

env=$(nix-build --no-out-link "$TOP_LEVEL/direnv.nix")
direnv="$(nix-build -A direnv "$TOP_LEVEL/nix")/bin/direnv"
eval "$("$direnv" stdlib)"

load_prefix "$env"

haskell-language-server-wrapper "$@"
