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

# headroom touches a per-user SQLite KV store (~/.headroom/cache.sqlite) on
# every startup (update check; no --no-cache flag in v0.4.3.0).  Concurrent CI
# jobs on the same worker share $HOME and race on that file, and the embedded
# persistent-sqlite has no busy timeout, so the loser dies with
# "SQLite3 returned ErrorBusy ... database is locked".  Point HOME at a
# throwaway dir with the update check disabled so no shared state is touched.
hr_home="$(mktemp -d)"
mkdir -p "$hr_home/.headroom"
printf 'updates:\n  check-for-updates: false\n  update-interval-days: 7\n' > "$hr_home/.headroom/global-config.yaml"
HOME="$hr_home" headroom run -a "${args[@]}"
