#!/usr/bin/env bash
#
# Runs INSIDE Envoy's build container, invoked by ../build.sh. Not meant to be
# run directly on a host.
#
# This exists as its own file rather than as a command string passed to
# ci/run_envoy_docker.sh because that script forwards the command through
# `exec ${DOCKER_COMMAND}` — an UNQUOTED expansion (see ci/docker-compose.yml).
# The result is word-split into argv, and shell operators are never re-parsed
# from an expansion, so a `cmd1 && cmd2` string is handed to cmd1 with `&&` as a
# literal argument. Bazel then fails with a baffling
#   ERROR: no such target '//: ': target ' ' not declared in package ''
# Passing a single-token script path is the only thing that survives intact.
#
set -euo pipefail

# ci/docker-compose.yml starts us in ENVOY_DOCKER_SOURCE_DIR (default /source),
# which is the mounted Envoy checkout.
OUT="${PWD}/envoy-static"

# Escape hatch for tuning the build without editing this file. Envoy's
# ci/docker-compose.yml already forwards BAZEL_BUILD_EXTRA_OPTIONS into the
# container, so exporting it on the host is enough. Typical use is capping
# resources when a big machine OOMs partway through:
#   BAZEL_BUILD_EXTRA_OPTIONS='--jobs=8 --local_ram_resources=HOST_RAM*.5'
read -r -a extra_flags <<<"${BAZEL_BUILD_EXTRA_OPTIONS:-}"

bazel build --config=aws-lc-fips -c opt "${extra_flags[@]}" //source/exe:envoy-static

# bazel-bin is a symlink into the /build mount; cp dereferences it, so the
# binary lands on the host through the /source bind mount.
cp -f bazel-bin/source/exe/envoy-static "$OUT"
chmod 0755 "$OUT"

echo "wrote ${OUT}"
