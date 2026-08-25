#!/usr/bin/env bash
#
# Build an Envoy proxy image linked against AWS-LC instead of BoringSSL.
#
# WHY: BSI TR-02102-2 names SecP256r1MLKEM768 and SecP384r1MLKEM1024 as the
# hybrid post-quantum key agreement groups it intends to recommend. BoringSSL —
# what the stock envoyproxy/envoy image links against — implements neither, so
# Envoy rejects the listener if you put those names in ecdhCurves. AWS-LC
# implements both, and Envoy supports it upstream via `--config=aws-lc-fips`.
# See the "Post-quantum key agreement" section of charts/wire-ingress/README.md.
#
# This is deliberately NOT part of the nix image set: the AWS-LC genrule wants
# Bazel's own downloaded LLVM toolchain plus pinned cmake/ninja/go, which is
# exactly what nixpkgs' Envoy derivation patches out. So we drive Envoy's own
# build container instead, the same way the Envoy project does.
#
# Expect a multi-hour build and tens of GB of disk on first run.
#
# Usage:
#   ./build.sh                      # build and tag locally
#   PUSH=1 ./build.sh               # build, then push
#   ENVOY_VERSION=v1.39.0 ./build.sh
#
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# MUST match the Envoy version your Envoy Gateway ships, because Envoy Gateway
# generates bootstrap config for a specific Envoy version. To find it:
#   kubectl -n envoy-gateway-system get deploy -l gateway.envoyproxy.io/owning-gateway-name \
#     -o jsonpath='{.items[0].spec.template.spec.containers[?(@.name=="envoy")].image}'
# Envoy Gateway v1.8.3 ships envoyproxy/envoy:distroless-v1.38.3.
ENVOY_VERSION="${ENVOY_VERSION:-v1.38.3}"

IMAGE="${IMAGE:-quay.io/wire/envoy-aws-lc}"
TAG="${TAG:-${ENVOY_VERSION}-aws-lc}"
PUSH="${PUSH:-0}"

# Both of these get large (the Bazel output base alone is tens of GB), so keep
# them off tmpfs. Override if /var/tmp is small on your machine.
WORK_DIR="${WORK_DIR:-/var/tmp/envoy-aws-lc}"
CHECKOUT_DIR="${CHECKOUT_DIR:-${WORK_DIR}/src}"
export ENVOY_DOCKER_BUILD_DIR="${ENVOY_DOCKER_BUILD_DIR:-${WORK_DIR}/build}"

log() { printf '\n\033[1m==> %s\033[0m\n' "$*"; }
die() { printf '\033[1;31merror:\033[0m %s\n' "$*" >&2; exit 1; }

# --- preflight ---------------------------------------------------------------

command -v docker >/dev/null || die "docker is required"
command -v git >/dev/null || die "git is required"
docker info >/dev/null 2>&1 || die "cannot talk to the docker daemon"

arch="$(uname -m)"
case "$arch" in
    x86_64|aarch64|ppc64le) ;;
    *) die "AWS-LC builds of Envoy support x86_64, aarch64 and ppc64le only (found: $arch)" ;;
esac

[[ "$(uname -s)" == Linux ]] || die "the Envoy build container only runs on Linux (found: $(uname -s))"

avail_gb="$(df -BG --output=avail "$(dirname "$WORK_DIR")" | tail -1 | tr -dc '0-9')"
if [[ -n "$avail_gb" && "$avail_gb" -lt 60 ]]; then
    die "only ${avail_gb}G free at $(dirname "$WORK_DIR"); the build needs roughly 60G. Set WORK_DIR elsewhere."
fi

mkdir -p "$ENVOY_DOCKER_BUILD_DIR"

# --- source ------------------------------------------------------------------

if [[ -d "${CHECKOUT_DIR}/.git" ]]; then
    log "Reusing checkout at ${CHECKOUT_DIR}"
    git -C "$CHECKOUT_DIR" fetch --depth 1 origin "refs/tags/${ENVOY_VERSION}:refs/tags/${ENVOY_VERSION}" 2>/dev/null || true
    git -C "$CHECKOUT_DIR" checkout --force "$ENVOY_VERSION"
else
    log "Cloning envoy ${ENVOY_VERSION} into ${CHECKOUT_DIR}"
    mkdir -p "$(dirname "$CHECKOUT_DIR")"
    git clone --depth 1 --branch "$ENVOY_VERSION" https://github.com/envoyproxy/envoy "$CHECKOUT_DIR"
fi

# Guard against a silent no-op: if upstream ever renames the config, bazel would
# fall back to a plain BoringSSL build and we would ship a normal Envoy.
grep -q 'common:aws-lc-fips' "${CHECKOUT_DIR}/.bazelrc" \
    || die "envoy ${ENVOY_VERSION} has no 'aws-lc-fips' config in .bazelrc — check bazel/SSL.md for the current flag"

# --- build -------------------------------------------------------------------

log "Building envoy-static with --config=aws-lc-fips (this takes hours)"
(
    cd "$CHECKOUT_DIR"
    ./ci/run_envoy_docker.sh \
        'bazel build --config=aws-lc-fips -c opt //source/exe:envoy-static \
         && cp -f bazel-bin/source/exe/envoy-static /source/envoy-static \
         && chmod 0755 /source/envoy-static'
)

[[ -f "${CHECKOUT_DIR}/envoy-static" ]] || die "build finished but ${CHECKOUT_DIR}/envoy-static is missing"

# --- image -------------------------------------------------------------------

log "Building image ${IMAGE}:${TAG}"
cp -f "${CHECKOUT_DIR}/envoy-static" "${SCRIPT_DIR}/envoy-static"
trap 'rm -f "${SCRIPT_DIR}/envoy-static"' EXIT
docker build -t "${IMAGE}:${TAG}" "$SCRIPT_DIR"

# --- verify ------------------------------------------------------------------

log "Verifying the crypto library"
version_output="$(docker run --rm --entrypoint /usr/local/bin/envoy "${IMAGE}:${TAG}" --version)"
echo "$version_output"
grep -q 'AWS-LC' <<<"$version_output" \
    || die "expected 'AWS-LC' in 'envoy --version', got: ${version_output}. The build silently fell back to another SSL library."

if [[ "$PUSH" == "1" ]]; then
    log "Pushing ${IMAGE}:${TAG}"
    docker push "${IMAGE}:${TAG}"
fi

cat <<EOF

Done: ${IMAGE}:${TAG}

Point wire-ingress at it (see charts/wire-ingress/README.md):

  gateway:
    manageServiceType: false
    envoyProxy:
      create: true
      spec:
        provider:
          type: Kubernetes
          kubernetes:
            envoyService:
              type: LoadBalancer
            envoyDeployment:
              container:
                image: ${IMAGE}:${TAG}
    tls:
      sslLibrary: aws-lc
      ecdhCurves:
        - SecP256r1MLKEM768
        - SecP384r1MLKEM1024
        - P-256
        - P-384
        - P-521
EOF
