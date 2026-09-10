# TLS conformance investigation

## 2026-09-10 — initial inspection

Scope: deploy only a small public test application in namespace `joe-test`, using
the supplied kubeconfig. Preserve existing cluster services and repository work.
No full Wire installation or Haskell changes are needed.

The supplied BSI-TR-02102-2.pdf is version 2026-01 (2026-01-27). Table 13
allows TLS_AES_128_GCM_SHA256, TLS_AES_256_GCM_SHA384 and
TLS_AES_128_CCM_SHA256. ChaCha20 and CCM-8 are absent. TLS 1.2 recommendations
end in 2031. Table 10 allows NIST/brainpool/selected FFDHE groups; hybrid NIST
ML-KEM groups are an intended future recommendation, not a current entry.

Cluster: Kubernetes 1.36.3, existing Envoy Gateway 1.8.3, GatewayClass `envoy`,
cert-manager with production Let's Encrypt DNS-01 ClusterIssuer `letsencrypt`.
External DNS watches HTTPRoutes and Ingresses (not Services). `joe-test` does
not yet exist. Existing ingress-nginx is outside this experiment's scope.

The wire-ingress chart already sets TLS parameters and supports EnvoyProxy
customization. Its normal cipher list only controls TLS <=1.2. Existing AWS-LC
build scripts and an interrupted build log are present; leave these intact.

Promising new lead: upstream Envoy has a `TlsParameters.compliance_policies`
setting. BoringSSL FIPS_202205 restricts TLS 1.3 to AES-GCM, potentially meeting
the requirement with the stock image and no extra proxy. Verify availability
in the actual 1.38.3 release and enforce it through Envoy Gateway, then test
negative cipher offers. This is a TLS algorithm policy, not a claim of FIPS
certification or full BSI conformance.

Alternatives under review: OpenSSL/AWS-LC Envoy (custom build and security
maintenance), HAProxy/OpenSSL termination (explicit ciphersuites, extra hop),
Traefik (Go's normal TLS 1.3 API also does not expose cipher selection).

## 10:30 UTC — stock-image path and infrastructure issue

Verified the actual Envoy 1.38.3 source: `compliance_policies: [FIPS_202205]`
is implemented, including on a normal BoringSSL build (with a warning that it
is not a FIPS build). Its pinned BoringSSL is 0.20260413.0. The policy is
applied LAST and resets versions to TLS 1.2–1.3, groups to P-256/P-384, and
TLS 1.2 ciphers to four ECDHE AES-GCM suites. All those suites are in BSI
Table 3. This cannot honestly be advertised as TLS 1.3-only, even if
ClientTrafficPolicy.minVersion is set to 1.3. That limitation is acceptable
for the requested minimum, which prefers but does not require 1.3-only.

Applied `hack/tls-conformance/manifests.yaml`. Gateway has public IPv4
46.225.37.184; hello pod is healthy. TLS listener is pending its certificate.
The patch is accepted but not yet programmed because the TLS listener does
not exist until that certificate is issued.

Infrastructure issue: both existing cert-manager and external-dns pods lack
AWS web identity environment variables. Their annotated ServiceAccounts and
the pod identity webhook exist. DNS-01 and DNS publication therefore fail
with missing AWS credentials. Requested permission to restart those two
controllers, since they are outside joe-test. No local AWS credentials are
available. Continue preparing tests while awaiting the response.

Safety improvement: set the ordinary ClientTrafficPolicy maximum to TLS 1.2.
The compliance policy overrides it to TLS 1.3 and restricts its suites together.
If EnvoyPatchPolicy is missing or fails to apply, the baseline still permits
only BSI-listed TLS 1.2 AES-GCM. This prevents accidental ChaCha20 exposure
from relying on a patch over a permissive TLS 1.3 baseline. Test this by
temporarily removing ONLY our patch and restoring it after baseline checks.

## 10:33 UTC — first live result

The actual stock Envoy listener accepts TLS 1.3 AES-GCM with the compliance
policy, despite the ordinary maxVersion=1.2. Admin listener dump confirms
the patch is present; Envoy logs the expected non-FIPS-build warning.

Public load balancer initially closed connections without reaching Envoy.
Setting `load-balancer.hetzner.cloud/use-private-ip: "true"` on our EnvoyProxy
fixed backend connectivity. Workers have private 10.20.0.x addresses. This
change affects only our generated load balancer.

Because public issuance is still blocked, temporarily reference the ECDSA
certificate from bootstrap.yaml. Its public certificate is used explicitly as
a trust anchor for diagnostics; no private key is exported. Final manifests
continue to reference the production Let's Encrypt certificate.

OpenSSL tester passes **109/109 probes against public IP 46.225.37.184**:
two TLS 1.3 AES-GCM suites; two ECDSA TLS 1.2 AES-GCM suites; rejection of
other client-supported suites, ChaCha20, CCM and CCM-8; P-256/P-384 only;
old versions, no-SNI and unknown-SNI rejected; mixed-version forbidden-only
offers rejected. Initial test failures correctly exposed a client security
level preventing CCM-8 offers and a TLS 1.2 group list omitting the P-384
certificate curve. Corrected the tester, not the server, for those cases.
Negative tests can kill kubectl port-forward on a connection reset, so the
authoritative probes now use the public IP directly.

testssl.sh v3.2.2 (commit c4856bef7255fec85affb50ca112d68bfa9c28d5) is
running its independent socket-based cipher-per-protocol scan.

## 10:38 UTC — independent scan, failure-mode proof, public certificate

testssl.sh finished successfully: exactly the same four AES-GCM suites,
TLS 1.2/1.3 only, P-256/P-384, no session tickets/IDs. Its diagnostic scan
correctly reports that the bootstrap certificate is not publicly trusted.
The JSON gate rejects this scan for certificate trust, despite green ciphers.

`test_patch_removal.py` passed against the live public IP: removing the
patch rejects TLS 1.3 with an actual protocol_version alert, keeps both
ECDSA AES-GCM TLS 1.2 suites, rejects ChaCha20, and restores TLS 1.3 after
reapplying the saved policy.

Implemented and verified an independent path using only joe-test resources:
`public-fallback.yaml` uses tr.46.225.37.184.sslip.io and a namespaced
Let's Encrypt HTTP-01 Issuer with the existing Gateway API solver. Issuance
succeeded, curl verified the public certificate and fetched the page, and
all 109 OpenSSL probes passed with the normal trust store.

The user then authorized restarting cert-manager and external-dns. Both
rollouts succeeded; AWS_ROLE_ARN/AWS_WEB_IDENTITY_TOKEN_FILE are now injected,
and external-dns successfully lists the zone without credential errors.
Restored manifests.yaml to use tr.hops.wire.link for the final deployment.
The fallback remains available as an optional documented overlay.

## 10:41 UTC — final hostname restored

tr.hops.wire.link resolves to 46.225.37.184 from both Cloudflare and Google
public resolvers. The Let's Encrypt DNS-01 certificate is Ready, the Gateway
and compliance patch are Programmed, and HTTP/2 GET / returns the static page
with normal certificate/hostname verification. HTTP port 80 redirects to HTTPS.
Only TCP ports 80/443 are exposed; there is no QUIC listener bypass.

The local system resolver cached the earlier NXDOMAIN (remaining TTL about
four minutes), so initial default-DNS scans correctly failed. Final on-wire
scans explicitly target the public DNS-verified IP while still checking SNI
and public certificate trust. No hosts-file entry or TLS verification bypass
was used. The OpenSSL final scan passed 109/109 and is saved to
`hack/tls-conformance/results/openssl.json`; the testssl final scan is running.

Removed the now-unused bootstrap and sslip.io Certificate/Issuer resources
and their test-only secrets (including the namespaced ACME account). Their
manifests remain on disk and can recreate them. The primary tr-certificate
and the existing cluster issuers/accounts are untouched.

Validation so far: seven offline scanner-gate regression tests pass;
shellcheck and bash syntax pass; wire-ingress Helm lint passes with test
values; git diff --check passes. Chart edits only correct documentation and
comments. No Haskell sources, dependencies or executable chart templates
changed, so no Haskell build/unit suite or full Wire integration suite was run.

## 10:43 UTC — certificate-chain improvement

The first production scan passed suites/trust but exposed an extra BSI concern:
the cluster issuer's default chain ended in ISRG Root X2 cross-signed by X1
with sha256WithRSAEncryption (PKCS#1 v1.5). Created a namespaced HTTP-01
Issuer `tr-letsencrypt` with preferredChain=ISRG Root X2 and updated only our
Certificate. Revision 2 issued successfully. The served three-certificate
chain now has ECDSA-with-SHA384 signatures throughout (leaf → YE2 → Root YE
→ ISRG Root X2). No shared issuer settings changed.

Added certificate-chain signature/curve inspection to the OpenSSL tester,
bringing the final profile to 110 probes. This prevents a later ignored
preferredChain setting from silently reintroducing the cross-signature.
Final reports are being refreshed for this exact certificate revision.

The testssl JSON gate initially expected "Ok" for a successful trust chain;
the actual v3.2.2 JSON spells this "passed.". Updated the strict success
allowlist and regression fixture, then validated the existing complete scan.
No TLS configuration was loosened to pass a test.

## Final verification

The final all-ECDSA-chain endpoint passed **110/110 OpenSSL probes** and the
independent testssl scan plus JSON gate. Local DNS subsequently resolved too;
normal-hostname curl returned HTTP/2 200 and the full OpenSSL test was repeated
through ordinary DNS. Evidence, exact versions/digest and test scope are in
`hack/tls-conformance/results/README.md`.

Only the intended primary issuer/certificate, hello workload, Gateway and
policies remain in joe-test. Shared controllers were only restarted as
authorized. The working PoC stays online. Existing user files and the earlier
AWS-LC build work were preserved. No custom image build or additional TLS
proxy was needed. The documented limits are TLS 1.2 remaining enabled,
FIPS_202205 preventing PQ groups, and the need to apply the policy to every
TLS filter chain when adapting the single-host manifest to a Wire deployment.
