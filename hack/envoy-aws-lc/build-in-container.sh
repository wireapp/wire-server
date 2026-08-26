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

# @aws_lc//:ninja_bin bootstraps ninja from source, and ninja's configure.py
# compiles it with ${CXX:-c++}. Bazel runs actions with a restricted PATH
# (/bin:/usr/bin:/usr/local/bin), and the Envoy build image has no `c++` at all:
# it installs g++-13 but only aliases `gcc` via update-alternatives, and keeps
# clang in /opt/llvm/bin, which is off the action PATH. The bootstrap therefore
# dies with "c++: not found" long before anything of ours compiles.
#
# Passing an absolute CXX through --action_env fixes it. This is safe to do
# globally: the actual AWS-LC build (bazel/external/aws_lc.genrule_cmd) pins its
# compilers with a CMake toolchain file pointing at the Bazel-provided LLVM, so
# it ignores CXX entirely.
resolve_cxx() {
    local c
    for c in c++ g++ clang++; do
        if command -v "$c" >/dev/null 2>&1; then command -v "$c"; return 0; fi
    done
    for c in "${LLVM_ROOT:-/opt/llvm}/bin/clang++" /usr/bin/g++-*; do
        if [[ -x "$c" ]]; then printf '%s\n' "$c"; return 0; fi
    done
    return 1
}

cxx_bin="${CXX:-}"
if [[ -z "$cxx_bin" ]]; then
    cxx_bin="$(resolve_cxx)" || {
        echo "error: no C++ compiler found in the build container; set CXX explicitly" >&2
        exit 1
    }
fi
echo "using CXX=${cxx_bin} for the ninja bootstrap"

bazel build --config=aws-lc-fips -c opt \
    --action_env=CXX="$cxx_bin" \
    "${extra_flags[@]}" //source/exe:envoy-static

# bazel-bin is a symlink into the /build mount; cp dereferences it, so the
# binary lands on the host through the /source bind mount.
cp -f bazel-bin/source/exe/envoy-static "$OUT"
chmod 0755 "$OUT"

echo "wrote ${OUT}"
