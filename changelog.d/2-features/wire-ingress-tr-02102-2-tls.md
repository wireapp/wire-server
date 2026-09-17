Default `wire-ingress` to TLS 1.3 with `X25519MLKEM768` hybrid key agreement.
Clients without this group can no longer connect unless classical fallback is
explicitly configured. TLS 1.2 requires lowering the minimum version and adding
classical groups; its default cipher list is limited to ECDHE AES-GCM.

Add opt-in `FIPS_202205_tls_profile` (previously `BSI_TR_02102_2_conformance`),
using stock Envoy's `FIPS_202205` policy for AES-GCM, P-256/P-384 and TLS 1.2–1.3,
with a restricted TLS 1.2 baseline if
the patch is absent. Apply consistent TLS/ALPN/PROXY settings to the Gateway and
federation listener. This configures TLS negotiation, not whole-system BSI
certification; see the chart README for requirements.
