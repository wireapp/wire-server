#!/usr/bin/env bash

set -euo pipefail

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
TOP_LEVEL="$(cd "$DIR/../.." && pwd)"

env=$(nix-build --no-out-link "$PWD/direnv.nix")
export PATH="$env/bin:$PATH"
haskell-language-server-wrapper "$@"
