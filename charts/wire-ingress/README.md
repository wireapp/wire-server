# wire-ingress

Wire ingress using Envoy Gateway and the Kubernetes Gateway API. Install it in
**the same namespace as `wire-server`**. See [values.yaml](values.yaml) for the
complete configuration reference.

## Prerequisites

- Kubernetes **1.33+** and Envoy Gateway **1.9+**. Check the controller's
  [compatibility matrix](https://gateway.envoyproxy.io/news/releases/matrix/).
- Gateway API CRDs including `ListenerSet` (the
  [1.6.1 standard bundle](https://github.com/kubernetes-sigs/gateway-api/releases/download/v1.6.1/standard-install.yaml)),
  and Envoy Gateway extension CRDs matching the controller version.
- A `GatewayClass` using controller
  `gateway.envoyproxy.io/gatewayclass-controller`.

For federation and the FIPS profile, configure the **Envoy Gateway Helm release**:

```yaml
config:
  envoyGateway:
    extensionApis:
      enableEnvoyPatchPolicy: true
      enableLua: true
    runtimeFlags:
      enabled: [XDSNameSchemeV2]
```

V2 naming is recommended and is the default from Envoy Gateway 1.10. Enabling it
changes listener names controller-wide; review existing listener patches first.

## Install

Create a TLS Secret covering your hostnames, then supply values such as:

```yaml
gateway:
  className: envoy
  listeners:
    https:
      hostname: "*.example.com"
config:
  dns:
    https: api.example.com
    ssl: ws.example.com
    webapp: webapp.example.com
    fakeS3: assets.example.com
tls:
  secret:
    create: false
    nameOverride: wire-ingress-tls
```

```sh
helm upgrade --install wire-ingress ./charts/wire-ingress \
  --namespace <wire-namespace> -f my-values.yaml
```

The HTTPS listener hostname must cover the configured domains. For hostnames
outside that wildcard, use `gateway.extraHttpsListeners` and matching certificate
SANs, or [multiple backend domains](#multiple-backend-domains).

The chart defaults to a LoadBalancer Service and three proxy replicas. Review
these settings for your deployment:

| Setting | Purpose |
| --- | --- |
| `gateway.serviceType` | Use `ClusterIP` when no external load balancer is needed. |
| `gateway.infrastructure.annotations` | Cloud load balancer settings; `gateway.annotations` applies to the Gateway itself. |
| `gateway.proxyProtocol.enabled` | Enable only when the upstream load balancer sends PROXY protocol headers. |
| `gateway.envoyProxy.replicas` | Proxy count; one replica disables the disruption budget. |
| `gateway.envoyProxy.spec` | Additional Envoy configuration, including resource requests. |
| `gateway.zoneAwareRouting.enabled` | Same-zone routing; requires zone labels and Envoy Gateway's topology injector. |

## Certificates and federation

For cert-manager, enable `tls.useCertManager` and configure `tls.issuer`. When
using an existing issuer, set `tls.createIssuer: false`. HTTP01 validation also
requires `gateway.listeners.http.enabled: true` and
[cert-manager Gateway API support](https://cert-manager.io/docs/usage/gateway/).
Use `certManager.customSolvers` for DNS01 if port 80 cannot be exposed.

Federation requires `federator.enabled: true`, its hostname in
`config.dns.federator`, and the `federator-ca` ConfigMap supplied by `wire-server`.
Its certificate needs **both server-auth and client-auth EKUs**. Use a suitable
private CA through `federator.tls.issuer`; do not assume a public ACME certificate
supports client authentication. Alternatively, set
`federator.tls.useCertManager: false` and provide `federator.tls.secretName`.

Keep Lua and patch policies enabled for federation: they provide client-certificate
forwarding and support for federation hostnames with a trailing dot.

## TLS profiles

| Profile | TLS versions | Ciphers | Key exchange |
| --- | --- | --- | --- |
| Default | 1.3 | AES-GCM, ChaCha20 | X25519MLKEM768, X25519, P-256, P-384, P-521 |
| `FIPS_202205` | 1.2–1.3 | AES-GCM; ECDHE for TLS 1.2 | P-256, P-384 |

Customize the default profile through `gateway.tls`. To enable FIPS parameters:

```yaml
FIPS_202205_tls_profile: true
gateway:
  patchPolicies:
    xdsNameSchemeV2: true
```

**The FIPS profile overrides all other TLS parameter settings.** It cannot be
combined with TLS 1.3-only or post-quantum settings. It requires a dedicated,
unmerged Gateway, with `gateway.tls.enabled` and `gateway.patchPolicies.enabled`
left enabled. Its settings apply to all TLS listeners sharing the HTTPS socket,
including federation and companion listeners.

`gateway.patchPolicies.xdsNameSchemeV2` must match the controller's naming scheme.
Use `true` with the controller configuration above; use `false` only for legacy
naming. A mismatch can leave the patch unapplied. Verify `Programmed=True` after
installation and controller upgrades:

```sh
kubectl -n <wire-namespace> get envoypatchpolicy \
  -o jsonpath='{.items[*].status.ancestors[*].conditions[*]}' | jq
```

This setting alone establishes neither FIPS certification nor
[BSI TR-02102-2 conformance](https://www.bsi.bund.de/SharedDocs/Downloads/EN/BSI/Publications/TechGuidelines/TG02102/BSI-TR-02102-2.pdf?__blob=publicationFile).
For BSI requirements, use ECDSA P-256/P-384 server certificates and separately
assess certificate-chain signatures, federation client credentials, and other
TLS terminators. The FIPS profile still permits RSA PKCS#1 signatures. Check the
applicable guideline edition and verify every public hostname after upgrades
and certificate renewal.

## Sharing a Gateway

Set `gateway.create: false` and `gateway.name` to use an existing Gateway in the
same namespace. It must admit this release's ListenerSet. Avoid conflicting
`ClientTrafficPolicy` resources: policies targeting the same object do not merge.

Other charts can attach listeners to this chart's Gateway. Use
`gateway.allowedListenerNamespaces` to admit additional namespaces, **only if
you trust them to serve hostnames on this Gateway**. Prefer V2 naming with shared
listeners; legacy listener ordering can invalidate FIPS patches.

To reuse an EnvoyProxy, set `gateway.envoyProxy.create: false` and provide its
name, or leave the name empty to inherit the GatewayClass configuration.
For merged Gateways, federation needs
`gateway.patchPolicies.targetGatewayClass: true`; the FIPS profile does not
support merged Gateways.

## Multiple backend domains

Use `config.domains` instead of `config.dns` to serve the same backend on several
domains. **This cannot be combined with federation.** For example:

```yaml
config:
  domains:
    - name: primary
      base: example.com
      dns: {https: api.example.com, ssl: ws.example.com, webapp: webapp.example.com}
    - name: secondary
      base: example.org
      dns: {https: api.example.org, ssl: ws.example.org, webapp: webapp.example.org}
      tls: {secretName: secondary-tls}
fakeS3:
  enabled: false
```

Each domain needs a matching certificate. Additional domains receive a
team-settings CSP header unless `renderCSP: false`; webapp and account-pages
manage their own CSP.

## Migrating from nginx-ingress-services

Most domain, service, certificate and feature values carry over. Add the Gateway
configuration shown above and review [values.yaml](values.yaml).

- Replace `config.ingressClass` with `gateway.className`.
- Replace `ingressName`, `config.isAdditionalIngress`, and `config.dns.base` with
  `config.domains` entries. Use per-domain `renderCSP` instead of
  `config.renderCSPInIngress`.
- Remove `tls.enabled`, `tls.verify_depth`, and `secrets.tlsClientCA`.
  Client trust comes from `federator-ca`; there is no equivalent verify-depth setting.
- Replace `secrets.certManager.customSolversSecret` with a custom Issuer.
- HTTP is disabled by default. The S3 `/minio/` guard redirects to `/` instead of
  returning 403.

## Tests

With the Nix development environment and a configured Envoy cluster:

```sh
make kube-ingress-test
make kube-ingress-test profile=fips
make kube-ingress-test profile=tr-02102-2
```

These scan the default, FIPS and TR-02102-2 profiles without requiring a Wire backend.
The TR profile follows the 2026-01 deployment guidance: FIPS TLS parameters with
ECDSA server certificates and ECDSA signatures throughout the verified chain,
including the trust anchor. It does not assess federation partners or other TLS
terminators. `KUBECONFIG`
overrides `tmp/kubeconfig`; if neither is available, kubectl's normal context is
used. Tests assume V2 naming. For a legacy controller, set
`WIRE_INGRESS_XDS_NAME_SCHEME_V2=false`. Non-default installations can set
`WIRE_INGRESS_GATEWAY_CLASS` and `ENVOY_GATEWAY_NAMESPACE`.

Runs are isolated and clean up their cluster resources. Results and failure
diagnostics remain under `tmp/ingress-tests/`. The checks cover HTTPS TLS
configuration, not application routing, federation mTLS, or compliance certification.

The existing `kube-integration-setup`, `kube-integration-test`, and
`kube-integration-teardown` flow includes these checks for Envoy setups
(`WIRE_INGRESS_MODE=envoy` during setup). Standalone and CI runs use the same tests.
See [test/](test/) for the profiles.
