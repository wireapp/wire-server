# Wire TLS cipher proof of concept

Use stock Envoy Gateway with stock Envoy/BoringSSL and the upstream
`FIPS_202205` TLS compliance policy. A custom Envoy build and an additional
TLS proxy are unnecessary for the requested **cipher allowlist**.

Tested with the cluster's Envoy Gateway **1.8.3**, Envoy **1.38.3** and its
pinned BoringSSL **0.20260413.0**. This is not a claim of FIPS certification
or full BSI certification. The baseline is the supplied January 2026 PDF.

Live endpoint: **https://tr.hops.wire.link/**, public IPv4 **46.225.37.184**.
The final manifests use a namespaced Let's Encrypt HTTP-01 issuer with
`preferredChain: ISRG Root X2`. The served chain uses ECDSA/SHA-384 throughout,
avoiding the RSA PKCS#1 v1.5 cross-signature in the cluster issuer's default
chain. Clients need an appropriate modern trust store. Saved evidence is in
[`results/`](results/).

## What the policy does

| Property | Effective setting |
|---|---|
| TLS versions | 1.2 and 1.3; no SSL/TLS 1.0/1.1 |
| TLS 1.3 suites | `TLS_AES_128_GCM_SHA256`, `TLS_AES_256_GCM_SHA384` |
| TLS 1.2 suites with this ECDSA certificate | `TLS_ECDHE_ECDSA_WITH_AES_128_GCM_SHA256`, `TLS_ECDHE_ECDSA_WITH_AES_256_GCM_SHA384` |
| TLS 1.2 if an RSA certificate is later installed | The corresponding two ECDHE-RSA AES-GCM suites are also policy-permitted |
| Key exchange | P-256, P-384 |
| HTTP | HTTP/2 and HTTP/1.1; port 80 redirects to HTTPS |
| Session resumption / early data | No tickets or session IDs configured; no 0-RTT enabled |
| Unknown / absent SNI | No matching TLS filter chain; connection rejected |

All four potential TLS 1.2 suites are in BSI Table 3. Both TLS 1.3 suites
are in Table 13. BSI also allows TLS_AES_128_CCM_SHA256, but supporting all
listed suites is unnecessary. ChaCha20 and CCM-8 are excluded.

The key detail is that `compliance_policies` is applied **after** ordinary
TLS parameters. It sets the protocol range to TLS 1.2–1.3 and restricts the
TLS 1.3 cipher selection. It works on the normal, non-FIPS image. Merely
setting `ciphers`, using a FIPS-labelled image, or changing the linked crypto
library is not an equivalent configuration.

The ordinary ClientTrafficPolicy deliberately sets maxVersion to **1.2**.
Only the compliance patch enables 1.3. Losing the patch therefore leaves
BSI-listed TLS 1.2 suites rather than unrestricted TLS 1.3. This behavior was
tested by removing and restoring the live patch. It also means that setting
minVersion to 1.3 cannot produce 1.3-only service while this policy is active.
The 2026 BSI document still recommends TLS 1.2 through the end of 2031.

## Files and deployment

`manifests.yaml` contains the namespaced application, certificate request,
Let's Encrypt Issuer, EnvoyProxy, Gateway, ClientTrafficPolicy, EnvoyPatchPolicy
and HTTPRoutes.
The controller provisions its proxy Deployment/Service in its own namespace;
the application and all policy resources are in `joe-test`.

Prerequisites: the `envoy` GatewayClass, Gateway API and Envoy CRDs,
`extensionApis.enableEnvoyPatchPolicy: true`, cert-manager, a working issuer,
cert-manager's `enableGatewayAPI: true`, and public DNS. The included issuer
uses HTTP-01 through the Gateway's port 80. The Hetzner annotations are specific
to this cluster. The
load balancer uses TCP forwarding; TLS terminates in Envoy.

```sh
kubectl --kubeconfig ./kubeconfig apply -f hack/tls-conformance/manifests.yaml
kubectl --kubeconfig ./kubeconfig -n joe-test get gateway,certificate,envoypatchpolicy
```

Check `Accepted=True` and `Programmed=True` on the patch and every listener,
then run the public tests. Gateway `Programmed` alone does not prove that
an Envoy patch was applied. Check active Envoy configuration when upgrading.
No certificate private keys belong in these files or test reports.

## Repeatable tests

OpenSSL 3.5+ is recommended so the client can offer the hybrid groups used
in the negative tests. Python uses argument arrays, explicit timeouts and
certificate/hostname verification. It checks every currently resolved A/AAAA
address. A local client error or network outage is a failure, not evidence
that a forbidden cipher was refused.

The checker additionally rejects non-ECDSA signatures or non-allowlisted EC
curves in the served certificate chain. `preferredChain` is a preference, so
this check detects an issuer silently falling back to an unsuitable chain.

```sh
python3 hack/tls-conformance/test_tls.py tr.hops.wire.link --json /tmp/openssl-report.json

git clone --depth 1 --branch v3.2.2 https://github.com/testssl/testssl.sh.git /tmp/testssl
bash hack/tls-conformance/run-testssl.sh /tmp/testssl/testssl.sh tr.hops.wire.link
```

The reviewed testssl v3.2.2 tag resolves to commit
`c4856bef7255fec85affb50ca112d68bfa9c28d5`. Its socket probes test a broader
cipher corpus than the host OpenSSL build. The wrapper parses the JSON and
fails on extra suites, missing expected suites, old protocols, an incomplete
scan, or an untrusted/mismatched certificate. Do not use `--ssl-native`.

The following test **briefly disables TLS 1.3 on this PoC** and restores its
patch in a `finally` block. Use it only for this disposable deployment:

```sh
python3 hack/tls-conformance/test_patch_removal.py --kubeconfig ./kubeconfig
```

`bootstrap.yaml` is a diagnostic self-signed certificate used while public
issuance is unavailable. It is not referenced by the final manifest and is
not a substitute for successful public certificate verification.

If shared AWS/DNS integration is unavailable, the separately tested
`public-fallback.yaml` overlay changes the Gateway and routes to
`tr.46.225.37.184.sslip.io` and uses a namespaced Let's Encrypt HTTP-01
Issuer. Apply it **after** manifests.yaml, and use that hostname in both
testers. Update the embedded IP-derived name if the load balancer's IP changes.
It uses the same Envoy TLS policy, with no nginx dependency. Applying the
base manifest again restores tr.hops.wire.link.

The offline scanner-gate regression tests can run in CI without a cluster:

```sh
python3 hack/tls-conformance/test_scan_validation.py
```

For a release pipeline, deploy into its test environment, run `test_tls.py`
and `run-testssl.sh`, retain their reports, and promote only when both exit
zero. The patch-removal test belongs in a disposable conformance environment,
not a live Wire deployment.

## Fronting Wire

The data path remains: TCP load balancer → Envoy Gateway proxy → Wire
Services (nginz/cannon/federator as appropriate). The hello HTTPRoute simply
stands in for the routes from `charts/wire-ingress`. TLS termination does
not depend on the backend being a static page.

For the existing chart, set `gateway.tls.enabled: true`, maxVersion/minVersion
to `"1.2"`, the four AES-GCM TLS 1.2 suites above, and ecdhCurves to
`[P-256, P-384]`. Attach the compliance patch to **every TLS filter chain**
generated for that Gateway, including federation and additional domains.
The supplied PoC patch targets one known single-host listener; do not copy its
`filter_chains/0` index blindly into a multi-host deployment. The controller's
listener naming scheme and merged-Gateway settings affect patch targets.
Do not put another unconfigured TLS terminator or CDN ahead of this listener.

Wire's existing routes, WebSockets, backend policies and federator client
certificate validation still need their normal application tests before a
release. This PoC does not deploy Wire or claim to test those behaviors.
The compliance policy also overrides signature preferences; inspect the
federation client's certificate types and authentication requirements.

Pin and regularly update the controller/proxy image pair; run these checks
on the release candidate after each upgrade. The successful test does not
establish that a particular image has no security advisories. A production
package also needs the project's normal image/CVE checks and support policy.

## Alternatives and the PQ migration

| Option | Cipher requirement | Maintenance / tradeoff |
|---|---|---|
| Stock Envoy + FIPS_202205 | Meets it; demonstrated live | Reuses current Gateway API work; policy forces TLS 1.2–1.3 and P-256/P-384 |
| Stock Envoy, TLS 1.2 only | Meets minimum with explicit AES-GCM list | Useful safe baseline; does not satisfy the preference for 1.3 |
| Envoy built with OpenSSL | Potentially configurable, needs testing | Supported build switch, but downstream runtime/build maintenance; outside Envoy's security policy; no HTTP/3 |
| Envoy built with AWS-LC | Potential path to NIST/ML-KEM groups | Existing build scripts can be reused, but changing the library alone does not prove cipher restriction; downstream test/maintenance burden |
| Stock Traefik | Normal TLS 1.3 cipher setting cannot enforce this list | Go's ordinary TLS API does not expose TLS 1.3 cipher selection; a custom runtime/FIPS route needs its own supported configuration and tests |
| HAProxy/OpenSSL termination | Explicit TLS 1.3 `ciphersuites` and version controls | A supported fallback for 1.3-only and OpenSSL PQ; adds a proxy if retaining Envoy routing, plus certificate reload/client identity handling |

Post-quantum names here are **key-exchange groups**, not TLS cipher suites.
Stock BoringSSL already supports X25519MLKEM768, so the earlier assumption
that it has no PQ support is incorrect. Cloud can enable it with normal
Envoy ecdhCurves configuration. However, FIPS_202205 overrides that list;
the strict profile demonstrated here cannot enable PQ just by adding a group.

OpenSSL 3.5+ implements SecP256r1MLKEM768 and SecP384r1MLKEM1024. AWS-LC
also provides those groups. Treat a future NIST-hybrid profile as a separate
tested change: confirm BSI's adopted recommendation and exact group IDs,
exercise clients that offer only those groups, test mixed offers and forbidden
fallbacks, and decide whether classical fallback remains permitted. TLS 1.3
AES-GCM suites can remain the same. A client with configurable allowed groups
may support cloud and strict profiles without requiring separate codebases;
the actual Wire client behavior was not tested here.

Beyond suites, a full BSI assessment must cover certificate chain signatures,
key lengths, client authentication, randomness, key handling and deployment
security. In particular, the 2026 text ends the recommendation for RSA PKCS#1
v1.5 certificate signatures in 2025; inspect the entire chosen public/private
CA chain. A browser-trusted certificate alone does not prove that requirement.
The PoC initially encountered that exact cross-signature, then obtained and
verified the ECDSA alternative before saving the final evidence.

## Primary sources

- Supplied `BSI-TR-02102-2.pdf`, version 2026-01, Tables 2, 3, 10–14.
- [Envoy 1.38.3 TLS policy definition](https://github.com/envoyproxy/envoy/blob/v1.38.3/api/envoy/extensions/transport_sockets/tls/v3/common.proto).
- [Envoy 1.38.3 application order and non-FIPS behavior](https://github.com/envoyproxy/envoy/blob/v1.38.3/source/common/tls/context_impl.cc).
- [Pinned BoringSSL policy implementation](https://github.com/google/boringssl/blob/0.20260413.0/ssl/ssl_lib.cc).
- [Envoy Gateway patch API and status checks](https://gateway.envoyproxy.io/v1.8/tasks/extensibility/envoy-patch-policy/).
- [Envoy crypto build/support boundaries](https://github.com/envoyproxy/envoy/blob/v1.38.3/bazel/SSL.md).
- [Go TLS 1.3 cipher configuration rationale](https://go.dev/blog/tls-cipher-suites).
- [OpenSSL 3.5 hybrid group support](https://docs.openssl.org/3.5/man3/SSL_CTX_set1_curves/).
- [HAProxy TLS configuration](https://www.haproxy.com/documentation/haproxy-configuration-tutorials/security/ssl-tls/global-tls-settings/).
- [HAProxy support branches](https://www.haproxy.org/).
