The `wire-ingress` chart now constrains the TLS parameters Envoy negotiates
(`gateway.tls.*`: TLS versions, TLS 1.2 cipher suites, key agreement groups,
signature algorithms), restoring the BSI TR-02102-2 conformance the nginx
ingress provided via `ssl-protocols` / `ssl-ciphers`. Key agreement now prefers
the hybrid post-quantum group X25519MLKEM768, falling back to P-256/P-384/P-521.

ALPN, TLS and PROXY protocol settings are now rendered into a single
Gateway-wide `ClientTrafficPolicy`, because Envoy Gateway rejects a second
policy targeting the same Gateway as `Conflicted` instead of merging it.

`gateway.tls.ecdhCurves` is validated against the crypto library named in the
new `gateway.tls.sslLibrary`, so a group the proxy image cannot offer fails at
template time rather than silently breaking the listener at runtime.

See the chart README for the conformance gaps that remain: TLS 1.3 cipher suites
are fixed by the crypto library and cannot be restricted by Envoy, and the
hybrid groups TR-02102-2 intends to recommend (SecP256r1MLKEM768 /
SecP384r1MLKEM1024) need an Envoy built against AWS-LC rather than BoringSSL.
