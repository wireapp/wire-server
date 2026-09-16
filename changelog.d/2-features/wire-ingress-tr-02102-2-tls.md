Default wire-ingress to TLS 1.3 with X25519MLKEM768-only hybrid key agreement
(no classical-client fallback). Retain four ECDHE AES-GCM TLS 1.2 suites for
explicit compatibility configurations, which must also add classical groups.
Add a
default-off BSI_TR_02102_2_conformance listener profile using stock Envoy's
FIPS_202205 policy and an AES-GCM-only TLS 1.2 fallback.

Combine ALPN, TLS and PROXY protocol into one ClientTrafficPolicy and repeat
TLS settings for federation's section policy. BSI mode overrides the default
with P-256/P-384 and TLS 1.2–1.3.

The listener profile does not certify certificate chains or other TLS
terminators. See the chart README for requirements.
