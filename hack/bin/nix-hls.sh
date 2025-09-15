#!/usr/bin/env bash

set -euo pipefail

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
TOP_LEVEL="$(cd "$DIR/../.." && pwd)"

direnv="$(nix build --no-link --print-out-paths "$TOP_LEVEL#pkgs.direnv")/bin/direnv"

# shellcheck disable=SC2016
maxMemory=$("$direnv" exec "$TOP_LEVEL" bash -c 'echo "$HLS_MAX_MEMORY"')

if [[ -z "$maxMemory" ]]; then
    "$direnv" exec "$TOP_LEVEL" haskell-language-server-wrapper "$@"
else
    systemd-run --scope -p MemoryMax="$maxMemory" --user "$direnv" exec "$TOP_LEVEL" haskell-language-server-wrapper "$@"
fi
