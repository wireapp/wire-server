#!/usr/bin/env bash

set -euo pipefail

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
TOP_LEVEL="$(cd "$DIR/../.." && pwd)"

direnv="$(nix-build --no-out-link "$TOP_LEVEL/nix" -A pkgs.direnv)/bin/direnv"

"$direnv" exec "$TOP_LEVEL" haskell-language-server-wrapper "$@"
