Default `wire-ingress` to TLS 1.3 with `X25519MLKEM768` hybrid key agreement and `X25519` and `P-256|384|521` as classical fallback. TLS 1.2 requires lowering the minimum version and adding
classical groups; its default cipher list is limited to ECDHE AES-GCM.

Add opt-in `FIPS_202205_tls_profile` using stock Envoy's `FIPS_202205` policy for AES-GCM, P-256/P-384 and TLS 1.2–1.3. Using this requires consistent TLS/ALPN/PROXY settings to the Gateway and federation listener. This configures TLS negotiation, not whole-system BSI certification; see the chart README for requirements on what to do to conform to BSI TR-TR-02102-2.
