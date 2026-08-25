# Envoy proxy image linked against AWS-LC instead of BoringSSL.
#
# Built by ./build.sh, which produces the `envoy-static` binary in this
# directory first. See charts/wire-ingress/README.md for why.
#
# Layout mirrors what Envoy Gateway expects of the stock
# envoyproxy/envoy:distroless-* image: the binary must be reachable as `envoy`
# on PATH (Envoy Gateway sets `command: ["envoy"]`), and the container runs as
# uid/gid 65532 (`runAsNonRoot: true`, `runAsUser: 65532`).
#
# distroless `base` gives glibc (envoy-static is static apart from libc),
# ca-certificates and tzdata. The `nonroot` tag already defaults to 65532.
ARG BASE_IMAGE=gcr.io/distroless/base-debian12:nonroot
FROM ${BASE_IMAGE}

COPY --chown=65532:65532 envoy-static /usr/local/bin/envoy

# Set explicitly rather than relying on the base image's PATH: Envoy Gateway
# invokes the bare name `envoy`, so an unset PATH would be a CrashLoopBackOff
# with a confusing message.
ENV PATH=/usr/local/bin:/usr/bin:/bin

USER 65532:65532
ENTRYPOINT ["/usr/local/bin/envoy"]
