# Chart integration checks — September 10, 2026

The chart BSI flag is default-off. The local Terraform experiment lives in
the sibling cailleach.tr-conformance checkout, with rollout instructions at
targets/wire/galaxy/tls-profiles.md. Bella is not yet apply-ready: native
TURN/DTLS, authenticated admin exposure and certificate-chain configuration
remain unresolved. No Galaxy access/apply was performed.

## Checks completed

- Nine offline Helm regression tests in test_chart.py.
- Seven existing testssl parser regression tests.
- Helm lint: wire-ingress and both Cailleach companion charts.
- Both changed Terraform modules validate with the repository's Nix providers;
  wire-server-with-bells retains two existing redundant ignore_changes warnings.
- Hops server-side schema validation and an isolated two-listener Gateway.
- Both actual Envoy filter chains contained FIPS_202205 (JSONPath wildcard).
- Chart BSI endpoint: 110/110 probes, results/chart-openssl.json.
- The secondary SNI listener accepted TLS 1.3 AES-256-GCM/P-256 and rejected
  TLS 1.3 ChaCha20. This synthetic SNI reuses the primary certificate: the
  diagnostic explicitly verified tr.hops.wire.link, not the synthetic name.
- Patch-removal test: safe TLS 1.2-only fallback; TLS 1.3 restored with the
  policy. See results/chart-patch-removal.txt for the initial tunnel failure.
- Separate non-BSI chart profile negotiated both X25519MLKEM768 and X25519
  using verified TLS 1.3 connections: results/chart-pq.json.

These tests reuse the existing PoC certificate and hello Service, not a Wire
installation. The two test Gateways use ClusterIP Services, so no additional
public DNS entries or load balancers are needed. Evidence uses localhost
addresses because tests ran through kubectl port-forward.

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
