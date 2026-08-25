The `wire-ingress` chart now constrains the TLS parameters Envoy negotiates
(`gateway.tls.*`: TLS versions, TLS 1.2 cipher suites, ECDH curves, signature
algorithms), restoring the BSI TR-02102-2 conformance the nginx ingress
provided via `ssl-protocols` / `ssl-ciphers`. ALPN, TLS and PROXY protocol
settings are now rendered into a single Gateway-wide `ClientTrafficPolicy`,
because Envoy Gateway rejects a second policy targeting the same Gateway as
`Conflicted` instead of merging it. See the chart README for the one
conformance gap that remains (TLS 1.3 cipher suites are fixed by BoringSSL and
cannot be restricted by Envoy).
