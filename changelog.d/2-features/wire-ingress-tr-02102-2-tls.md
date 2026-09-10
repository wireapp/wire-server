Add configurable general-purpose TLS settings to wire-ingress, with a
default-off BSI_TR_02102_2_conformance listener profile using stock Envoy's
FIPS_202205 policy and an AES-GCM-only TLS 1.2 fallback.

Combine ALPN, TLS and PROXY protocol into one ClientTrafficPolicy and repeat
TLS settings for federation's section policy. Optional X25519MLKEM768 key
agreement is available through gateway.tls.ecdhCurves outside BSI mode.

The listener profile does not certify certificate chains or other TLS
terminators. See the chart README and hack/tls-conformance for requirements
and tests. Remove the unsuccessful custom AWS-LC build tooling.
