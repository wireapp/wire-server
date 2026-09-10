# Verified deployment — 2026-09-10

Endpoint: https://tr.hops.wire.link/ (46.225.37.184), namespace `joe-test`.
HTTP/2 200 with public certificate verification; HTTP redirects to HTTPS.

| Negotiated version | Exhaustively discovered suites in the testssl corpus |
|---|---|
| TLS 1.3 | TLS_AES_128_GCM_SHA256, TLS_AES_256_GCM_SHA384 |
| TLS 1.2 | TLS_ECDHE_ECDSA_WITH_AES_128_GCM_SHA256, TLS_ECDHE_ECDSA_WITH_AES_256_GCM_SHA384 |

- `openssl.json`: 110/110 probes passed, including allowed/forbidden suites,
  groups, mixed offers, older protocols, no/unknown SNI, public trust and the
  served chain's ECDSA signatures/curves. Final run used normal DNS resolution.
- `testssl.json` / `testssl.txt`: independent testssl.sh v3.2.2 scan passed
  the JSON gate. Only the suites above, only P-256/P-384, SSLv2/3 and TLS1/1.1
  disabled, h2/http1.1, no session resumption. This run explicitly used the
  DNS-verified IP because the local resolver had cached an earlier NXDOMAIN;
  SNI and certificate hostname/trust checks remained enabled. Public DNS and
  subsequent default-DNS curl/OpenSSL checks also succeeded.
- `patch-removal.txt`: temporarily deleting the compliance patch disables
  TLS 1.3, preserves the allowed TLS 1.2 suites and rejects ChaCha20. Restoring
  it restores TLS 1.3. The final patch is present and Programmed=True.
- `certificate-signatures.txt`: the final three-certificate served chain
  uses ECDSA/SHA-384 throughout; no RSA PKCS#1 v1.5 cross-signature.
- `listener.json`: selected active listener parameters from the Envoy admin
  interface. The ordinary maximum says TLS 1.2; `FIPS_202205` is applied last
  and enables restricted TLS 1.3. The on-wire results establish actual behavior.

Versions: Envoy Gateway 1.8.3, stock Envoy distroless-v1.38.3, BoringSSL
0.20260413.0; OpenSSL test client 3.6.2; testssl v3.2.2 commit
c4856bef7255fec85affb50ca112d68bfa9c28d5. Observed Envoy image digest:
`sha256:574348fada8eb1130b448132287d76626dfb07525b16668075382f8e154a45a8`.

Seven offline regression tests, shellcheck, bash syntax, Helm lint and
server-side Kubernetes manifest validation passed. No Haskell implementation
or dependency changes; no full Wire installation or integration tests.

Scope: evidence of this public endpoint's configured cipher/profile behavior,
not a complete BSI/FIPS certification or a Wire application compatibility test.
