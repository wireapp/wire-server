# Chart integration checks — September 10, 2026

The chart BSI flag is default-off. The local Terraform experiment lives in
the sibling cailleach.tr-conformance checkout, with rollout instructions at
targets/wire/galaxy/tls-profiles.md. Bella/Chala are prepared for operator
planning and staged application. Native TURN/DTLS is explicitly deferred;
see cailleach.tr-conformance/coturn-todos.txt. Public admin auth is preserved,
and Bella selects the new preferred-ECDSA-chain issuer. Real certificate and
OAuth acceptance remains an operator post-apply check. No Galaxy access/apply
was performed.

## Checks completed

- Ten offline Helm regression tests in test_chart.py.
- Six companion-chart regression tests in test_companion_charts.py, including
  admin auth, SFT profiles, and legacy/Envoy inbucket rendering.
- Seven existing testssl parser regression tests.
- Helm lint: wire-ingress, SFT/auxiliary/admin companion charts, and inbucket.
- Three changed Terraform modules validate with the repository's Nix providers;
  wire-server-with-bells retains two existing redundant ignore_changes warnings.
- Hops server-side schema validation and an isolated three-listener Gateway.
- All three actual Envoy filter chains contained FIPS_202205 (JSONPath wildcard).
- Chart BSI endpoint: 110/110 probes, results/chart-openssl.json.
- The secondary SNI listener accepted TLS 1.3 AES-256-GCM/P-256 and rejected
  TLS 1.3 ChaCha20. This synthetic SNI reuses the primary certificate: the
  diagnostic explicitly verified tr.hops.wire.link, not the synthetic name.
- Patch-removal test: safe TLS 1.2-only fallback; TLS 1.3 restored with the
  policy. See results/chart-patch-removal.txt for the initial tunnel failure.
- Separate non-BSI chart profile negotiated both X25519MLKEM768 and X25519
  using verified TLS 1.3 connections: results/chart-pq.json.
- Eleven live synthetic admin checks: OAuth allow/deny/outage, redirect
  encoding, spoofed-header rejection, internal-path isolation, APR1 basic auth
  and unknown hosts; results/admin-auth.txt. Also verified the admin route
  through Envoy using TLS 1.3 AES-GCM/P-256. This does not test real Galaxy OAuth.
- Rendered the targets' pinned published SFT chart 0.148.0: its route references
  the dedicated Gateway, its certificate Secret is `sftd-<namespace>`, and its
  issued leaf is ECDSA P-384. No SFT chart modification is needed.
- After removing the temporary fixtures, the original public
  `tr.hops.wire.link` endpoint passed another 110/110 probes against its sole
  DNS address 46.225.37.184: results/public-handoff.json.

These tests reuse the existing PoC certificate and hello Service, not a Wire
installation. The two test Gateways use ClusterIP Services, so no additional
public DNS entries or load balancers are needed. Evidence uses localhost
addresses because tests ran through kubectl port-forward.
Synthetic Gateways/routes must carry
`external-dns.alpha.kubernetes.io/controller: ignored`; otherwise external-dns
can advertise the test ClusterIP alongside the real public hostname. An initial
fixture run exposed this; advertisements were removed and public DNS restored
before saving the successful results.
Temporary chart/PQ/admin/OAuth fixtures were removed after testing; they can be
recreated from the saved manifests. The original public hello PoC remains live.

## Reproduce

From the wire-server checkout (PyYAML, Helm and OpenSSL 3.5+ required):

```sh
python3 hack/tls-conformance/test_chart.py
helm template tr-chart charts/wire-ingress -n joe-test \
  -f hack/tls-conformance/chart-hops-values.yaml \
  --show-only templates/gateway.yaml \
  --show-only templates/envoyproxy.yaml \
  --show-only templates/clienttrafficpolicy-gateway.yaml \
  --show-only templates/envoypatchpolicy-bsi.yaml
```

The saved chart-hops-rendered.yaml is that selected render. Apply it with
chart-hops-backend.yaml only to the authorized Hops joe-test namespace.
Do not install the full chart using these synthetic values. The second
hostname is only a filter-chain fixture, not a publicly valid hostname.

For PQ, use the same template command without the EnvoyPatchPolicy template,
release tr-pq, and these overrides:
`--set gateway.name=tr-pq --set BSI_TR_02102_2_conformance=false
--set gateway.tls.minVersion=1.3
--set gateway.tls.ecdhCurves='{X25519MLKEM768,X25519,P-256,P-384}'`.
The saved result is chart-hops-pq-rendered.yaml.

After the user applies the Galaxy changes and DNS settles, run (all DNS
addresses are tested; do not force an address to conceal stale records):

```sh
python3 hack/tls-conformance/test_tls.py nginz-https.bella.wire.link
python3 hack/tls-conformance/test_pq.py nginz-https.chala.wire.link
python3 hack/tls-conformance/test_pq.py sft.chala.wire.link
```

Repeat the strict checks for every public Bella endpoint. The current
test_tls.py deliberately requires the tested all-ECDSA chain/profile; private
federation roots need --cafile. Native TURN/DTLS additionally needs
protocol-specific testing; an HTTPS scan cannot cover it.
