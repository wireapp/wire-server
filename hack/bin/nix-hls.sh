#!/usr/bin/env bash

set -euo pipefail

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
TOP_LEVEL="$(cd "$DIR/../.." && pwd)"

env=$(nix-build --no-out-link "$TOP_LEVEL/direnv.nix")
eval "$(direnv stdlib)"
load_prefix "$env"

haskell-language-server-wrapper "$@"
