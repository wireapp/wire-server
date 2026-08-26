#!/usr/bin/env bash

# treefmt adapter for headroom (see treefmt.toml and .headroom.yaml).
#
# treefmt hands us a list of file paths, headroom wants them as repeated
# '-s' arguments.  Everything else (templates, variables, run-mode) comes
# from .headroom.yaml, which headroom picks up from the working directory
# (treefmt runs formatters from the root of the tree).

set -euo pipefail

[[ $# -eq 0 ]] && exit 0

args=()
for file in "$@"; do
    args+=(-s "$file")
done

# '-a' matches 'run-mode: add' from the config; we pass it explicitly so that
# this stays add-only even if someone changes the config's default run-mode.
headroom run -a "${args[@]}"
