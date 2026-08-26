# Envoy proxy image with AWS-LC

Builds an Envoy proxy image linked against [AWS-LC](https://github.com/aws/aws-lc)
instead of BoringSSL, so that the Gateway can offer the hybrid post-quantum key
agreement groups `SecP256r1MLKEM768` and `SecP384r1MLKEM1024`.

Those are the two groups BSI TR-02102-2 names as its intended recommendation
once [draft-ietf-tls-ecdhe-mlkem](https://datatracker.ietf.org/doc/draft-ietf-tls-ecdhemlkem/)
becomes an RFC. BoringSSL implements neither, so on a stock
`envoyproxy/envoy` image Envoy rejects the listener if they appear in
`ecdh_curves`. AWS-LC implements both, and Envoy supports it upstream through
`bazel build --config=aws-lc-fips` (see
[bazel/SSL.md](https://github.com/envoyproxy/envoy/blob/main/bazel/SSL.md)).

```bash
./build.sh              # build and tag locally
PUSH=1 ./build.sh       # build, then push
```

Or from the repo root: `make build-envoy-aws-lc-image [PUSH=1]`.

`build-in-container.sh` is the part that runs inside Envoy's build container. It
is a separate file on purpose: `ci/run_envoy_docker.sh` forwards the command
through an unquoted `exec ${DOCKER_COMMAND}`, so anything containing spaces is
word-split into argv and `&&` reaches bazel as a literal target — which surfaces
as `ERROR: no such target '//: '`. Only a single-token script path survives.

| Variable | Default | Purpose |
|---|---|---|
| `ENVOY_VERSION` | `v1.38.3` | Envoy tag to build; must match your Envoy Gateway |
| `IMAGE` / `TAG` | `quay.io/wire/envoy-aws-lc` / `<version>-aws-lc` | Image coordinates |
| `PUSH` | `0` | Push after a successful build and verification |
| `WORK_DIR` | `/var/tmp/envoy-aws-lc` | Checkout and Bazel output base; keep it off tmpfs |
| `MIN_DISK_GB` | `60` | Free-space precheck; `0` disables it |
| `BAZEL_BUILD_EXTRA_OPTIONS` | _(empty)_ | Extra bazel flags. Use if the build OOMs: `--jobs=8 --local_ram_resources=HOST_RAM*.5` |

## Iterating on the build

Keep a log so failures can be read back:

```bash
set -o pipefail
make build-envoy-aws-lc-image 2>&1 | tee envoy-build.log
```

For a hands-off loop, let `entr` rebuild whenever the build scripts change. `-r`
kills an in-flight build as soon as a fix lands, so you never wait on a run that
is already known-bad:

```bash
ls hack/envoy-aws-lc/build.sh \
   hack/envoy-aws-lc/build-in-container.sh \
   hack/envoy-aws-lc/Dockerfile \
   Makefile \
 | entr -cr bash -c 'set -o pipefail; make build-envoy-aws-lc-image 2>&1 | tee envoy-build.log'
```

Bazel state lives under `WORK_DIR` and survives between runs, so only the first
cycle pays the full build cost. `envoy-build.log` is gitignored.

Two caveats: `entr -r` SIGTERMs the child, which can leave an orphaned
`ci-envoy-build-run-*` container behind — `docker ps` and clean up if a rerun
complains. And `entr` stops watching if the file list itself changes, so restart
it after adding a file here.

The full rationale, the chart wiring, and what this costs you in maintenance is
in the [wire-ingress chart README](../../charts/wire-ingress/README.md#getting-secp256r1mlkem768--secp384r1mlkem1024).

## Before you run it

- **Linux, x86_64 / aarch64 / ppc64le.** The Envoy build container is Linux-only
  and AWS-LC builds are limited to those architectures.
- **Hours, and roughly 60G of disk.** This is a full Bazel build of Envoy.
  Override `WORK_DIR` if `/var/tmp` is small.
- **`ENVOY_VERSION` must match your Envoy Gateway.** Envoy Gateway generates
  bootstrap config for a specific Envoy version. Read it off the running proxy:

  ```bash
  kubectl -n envoy-gateway-system get deploy \
    -l gateway.envoyproxy.io/owning-gateway-name \
    -o jsonpath='{.items[0].spec.template.spec.containers[?(@.name=="envoy")].image}'
  ```

  Envoy Gateway v1.8.3 ships `envoyproxy/envoy:distroless-v1.38.3`, which is the
  script's default. You own this pin from here on: it has to be rebuilt for
  every Envoy Gateway bump.
- **This build is not covered by the Envoy project's test matrix.**
  `bazel/SSL.md` states that only the BoringSSL FIPS build on x86_64 is
  "supported and tested by the Envoy project", and that other combinations are
  the responsibility of downstream projects. HTTP/3 is also disabled in AWS-LC
  builds — irrelevant for this Gateway, which serves h2 and http/1.1.

Deliberately not a nix image: the AWS-LC genrule wants Bazel's own downloaded
LLVM toolchain plus pinned cmake/ninja/go, which is precisely what nixpkgs'
Envoy derivation patches out (it builds with `--config=gcc` and
`--repository_disable_download`).
