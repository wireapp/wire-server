# wire-ingress

A Helm chart for Wire server ingress using the **Kubernetes Gateway API**.

The chart targets **Envoy Gateway** as the Gateway API controller.

---

## Status

**This chart is in development. Dont use it in production yet!**

---

## Prerequisites

### Envoy Gateway

[Envoy Gateway](https://gateway.envoyproxy.io/) must be installed in the cluster before deploying
this chart. The `EnvoyPatchPolicy` extension API must be enabled (required for federation — see
[EnvoyPatchPolicy](#envoypatchpolicy)):

```yaml
config:
  envoyGateway:
    extensionApis:
      enableEnvoyPatchPolicy: true
```

---

## Backwards compatibility

The chart preserves the `values.yaml` structure of the `nginx-ingress-services` chart wherever possible.
Operators should be able to reuse most of their existing values files with minimal changes.

### Behaviour changes

* non-tls ingress disabled by default. If you want to make use of automated certificate validation via http01, you need `gateway.listeners.http.enabled: true`
* s3 ingress b`/minio/` path blocking. Returns 301 redirect to "/" (was 403).

### New values (no equivalent in nginx-ingress-services)

Only values that require explanation are listed. Trivial or self-explanatory values (ports,
name overrides, etc.) can be found in `values.yaml`.

| Key | Default | Description |
|---|---|---|
| `gateway.create` | `true` | If `false`, no `Gateway` resource is created — set `gateway.name` to reference an existing one. Useful when sharing a Gateway across multiple releases. |
| `gateway.controllerNamespace` | `envoy-gateway-system` | Namespace where Envoy Gateway runs its proxy pods. Change only if Envoy Gateway was installed into a non-default namespace. |
| `gateway.envoyProxy.create` | `true` | If `false`, no `EnvoyProxy` resource is created. Set `gateway.envoyProxy.name` to reference an existing one, or leave it empty to inherit the GatewayClass-level `EnvoyProxy`. |
| `gateway.envoyProxy.name` | _(derived)_ | When `create: true` — name of the created resource. When `create: false` — name of an existing `EnvoyProxy` to reference via `infrastructure.parametersRef`. |
| `gateway.envoyProxy.spec` | `{}` | Free-form [EnvoyProxySpec](https://gateway.envoyproxy.io/docs/api/extension_types/#envoyproxyspec) merged verbatim. Use to set `mergeGateways`, custom service annotations, etc. |
| `gateway.serviceType` | `LoadBalancer` | Service type for the Envoy proxy service. Only used when `gateway.manageServiceType: true`. |
| `gateway.manageServiceType` | `true` | Shorthand that sets `envoyService.type` to `gateway.serviceType`. Disable when managing the service type via `gateway.envoyProxy.spec` directly. |
| `gateway.infrastructure.annotations` | `{}` | Annotations forwarded to the LoadBalancer Service provisioned by Envoy Gateway — see [Gateway API docs](https://gateway-api.sigs.k8s.io/reference/spec/#gateway.networking.k8s.io/v1.GatewayInfrastructure). Use for cloud-specific LB settings (e.g. AWS NLB). |
| `gateway.proxyProtocol.enabled` | `false` | Creates a `ClientTrafficPolicy` enabling PROXY protocol on all listeners. Required when the upstream load balancer is configured to send PROXY protocol headers. |
| `gateway.patchPolicies.enabled` | `true` | Controls whether `EnvoyPatchPolicy` resources are created — see [EnvoyPatchPolicy](#envoypatchpolicy). |
| `gateway.listeners.https.hostname` | `""` | **Required when `federator.enabled: true`.** Restricts the HTTPS listener to a specific hostname (e.g. `*.example.com`). Without this, both the HTTPS and federator listeners are catch-all on the same port, causing Envoy to degrade ALPN to HTTP/1.1-only (`OverlappingTLSConfig`). |
| `gateway.listeners.http.enabled` | `false` | Enables the HTTP listener on port 80. Required for HTTP01 ACME challenges via cert-manager's `gatewayHTTPRoute` solver — see [HTTP01 certificate challenges](#http01-certificate-challenges). |
| `tls.secret.create` | `true` | If `false`, the TLS Secret is not created by this chart. Use when the secret is managed externally (e.g. by another operator). |
| `federator.tls.useCertManager` | `true` | Controls cert-manager for the federator TLS secret independently of `tls.useCertManager`. Requires a private CA — see [Federator TLS certificate](#federator-tls-certificate-federatortlsusecertmanager). |

### Dropped values

| Old key | Reason |
|---|---|
| `config.ingressClass` | Replaced by `gateway.className` — different concept (GatewayClass name, not IngressClass) |
| `ingressName` | Multi-ingress out of scope |
| `config.isAdditionalIngress` | Multi-ingress out of scope |
| `config.renderCSPInIngress` | Multi-ingress out of scope |
| `config.dns.base` | Only used for CSP header rendering, which is a multi-ingress feature |
| `tls.verify_depth` | Envoy Gateway `ClientTrafficPolicy` does not expose a direct verify-depth knob; the CA chain itself controls this |
| `tls.enabled` | Removed — had no effect; all routes are always TLS-terminated |
| `secrets.tlsClientCA` | No longer supplied via values. The `federator-ca` ConfigMap is created by the wire-server chart and referenced directly. |
| `secrets.certManager.customSolversSecret` | No longer supported. Create a custom Issuer instead. |

### Fully backwards compatible values

All keys below are accepted unchanged. Their names, types, and semantics are identical to
`nginx-ingress-services`.

| Key |
|---|
| `nameOverride` |
| `teamSettings.enabled` |
| `accountPages.enabled` |
| `websockets.enabled` |
| `webapp.enabled` |
| `fakeS3.enabled` |
| `federator.enabled` |
| `federator.integrationTestHelper` |
| `federator.tls.duration` |
| `federator.tls.renewBefore` |
| `federator.tls.privateKey.rotationPolicy` |
| `federator.tls.issuer.name` |
| `federator.tls.issuer.kind` |
| `federator.tls.issuer.group` |
| `tls.useCertManager` |
| `tls.createIssuer` |
| `tls.privateKey.rotationPolicy` |
| `tls.privateKey.algorithm` |
| `tls.privateKey.size` |
| `tls.issuer.name` |
| `tls.issuer.kind` |
| `tls.caNamespace` |
| `certManager.inTestMode` |
| `certManager.certmasterEmail` |
| `certManager.customSolvers` |
| `service.webapp.externalPort` |
| `service.s3.externalPort` |
| `service.s3.serviceName` |
| `service.useFakeS3` |
| `service.teamSettings.externalPort` |
| `service.accountPages.externalPort` |
| `config.dns.https` |
| `config.dns.ssl` |
| `config.dns.webapp` |
| `config.dns.fakeS3` |
| `config.dns.federator` |
| `config.dns.certificateDomain` |
| `config.dns.teamSettings` |
| `config.dns.accountPages` |
| `secrets.tlsWildcardCert` |
| `secrets.tlsWildcardKey` |


## Design decisions

### Gateway API controller: Envoy Gateway

The chart targets [Envoy Gateway](https://gateway.envoyproxy.io/). Implementation-specific
resources (`ClientTrafficPolicy`, `SecurityPolicy`, `HTTPRouteFilter` with `directResponse`) are
used where the standard Gateway API has gaps. These resources are clearly marked in each template.

### Gateway creation is optional

The chart can optionally create a `Gateway` resource (controlled by `gateway.create: true`).
When `gateway.create: false`, all `HTTPRoute` and policy resources still reference the gateway by
name (`gateway.name`). This allows operators to share a Gateway across multiple charts or manage it
separately.

The default values create the Gateway. The default `gateway.name` is derived from the release name,
so that self-referencing is consistent by default.

### EnvoyProxy resource

The chart creates an `EnvoyProxy` resource (when `gateway.envoyProxy.create: true`) and wires it
to the `Gateway` via `infrastructure.parametersRef`. Use `gateway.envoyProxy.spec` to pass
arbitrary fields from the [EnvoyProxySpec](https://gateway.envoyproxy.io/docs/api/extension_types/#envoyproxyspec).

Set `gateway.envoyProxy.create: false` when a shared `EnvoyProxy` is managed at the
`GatewayClass` level (e.g. shared load balancer across deployments) — leave `gateway.envoyProxy.name`
empty and the Gateway will have no `infrastructure.parametersRef`, letting the `GatewayClass`-level
`EnvoyProxy` take effect automatically.

Set `gateway.envoyProxy.name` (with `create: false`) to reference an existing `EnvoyProxy` in the
**same namespace** via `infrastructure.parametersRef`.

`gateway.manageServiceType: true` (default) is a shorthand that sets
`provider.kubernetes.envoyService.type` to `gateway.serviceType`. Disable it when managing
the service type via `envoyProxy.spec` or a cluster-level `EnvoyProxy`.

### GatewayClass is not created

`GatewayClass` is installed by the Envoy Gateway Helm chart and is cluster-scoped. This chart only
references it by name via `gateway.className`.

### EnvoyPatchPolicy

When `federator.enabled: true`, the chart creates an `EnvoyPatchPolicy` resource that adds the
FQDN variant of the federator hostname (e.g. `federator.example.com.`, with trailing dot) to the
Envoy virtual host's domain list.

**Why this is needed:** Wire federation resolves remote backends via DNS SRV records. Per the DNS
specification, SRV record targets are always FQDNs — they include a trailing dot
(e.g. `peer.example.com.`). The federator passes this FQDN directly as the HTTP/2 `:authority`
header. Envoy's virtual-host matching is exact, so the trailing dot causes a `route_not_found`
error. Adding the FQDN as an additional domain in the route configuration allows Envoy to match
both the bare hostname and the FQDN.

The policy targets `kind: GatewayClass` (using `gateway.className`) and patches the
`RouteConfiguration` named `<namespace>/<gateway>/federator`. This approach works correctly with
`mergeGateways: true` because route configuration names are per-namespace even when multiple
Gateways share a single Envoy proxy.

`EnvoyPatchPolicy` is an Envoy Gateway extension API. It must be explicitly enabled in the
EnvoyGateway ConfigMap before deploying this chart with `federator.enabled: true`:

```yaml
apiVersion: gateway.envoyproxy.io/v1alpha1
kind: EnvoyGateway
metadata:
  name: envoy-gateway
  namespace: envoy-gateway-system
spec:
  extensionApis:
    enableEnvoyPatchPolicy: true
```

> **Future note:** If future versions of the Wire federator stop sending FQDNs in the
> `:authority` header, this patch policy will no longer be needed. `gateway.patchPolicies.enabled`
> exists so it can be disabled at that point without a chart change.

---

### Multi-ingress is out of scope

Single-domain deployments are the only supported topology. Multi-domain support can be added later.

### HTTP01 certificate challenges

cert-manager can complete ACME HTTP01 challenges through the Gateway using the `gatewayHTTPRoute`
solver (cert-manager >= 1.14). The **default solver** in this chart uses `gatewayHTTPRoute` — it
requires the HTTP listener to be enabled:

```yaml
gateway:
  listeners:
    http:
      enabled: true  # required for HTTP01 challenges
```

If you cannot or do not want to open port 80, use a DNS01 solver instead by setting

```yaml
certManager:
  customSolvers:
    - dns01:
        # .. provider-specific settings
```

DNS01 requires credentials for your DNS provider but does not need
port 80 to be open.

### Federator TLS certificate (`federator.tls.useCertManager`)

When `federator.tls.useCertManager: true`, cert-manager issues the federator TLS certificate.
The certificate requires both **server auth** and **client auth** Extended Key Usages (EKUs),
because federator connections are mutually authenticated.

**Most public CAs (including Let's Encrypt) no longer issue certificates with the client auth
EKU.** You will need a **private CA** (e.g. a cert-manager `ClusterIssuer` backed by an internal
CA) to issue the federator certificate. Using the same public ACME issuer as for the main
wildcard certificate will not work.

A typical setup uses a cert-manager `ClusterIssuer` of type `CA`, referencing a private CA
secret:

```yaml
federator:
  tls:
    useCertManager: true
    issuer:
      name: my-private-ca
      kind: ClusterIssuer
```

---

### Federator mTLS uses Envoy Gateway policies

Federator mTLS is implemented using:

- `ClientTrafficPolicy` to configure TLS settings on the federator `Gateway` listener (client
  certificate validation, verify depth)
- A separate `Gateway` listener (or dedicated `Gateway`) for the federator so that mTLS settings
  apply only to that listener
- The `X-SSL-Certificate` header forwarding is handled via Envoy Gateway's `HTTPRouteFilter` with
  request header injection from the client cert (implementation-specific)

---
---

## Testing strategy

### Rendering diff

For each implementation step, render the chart and inspect the output:

```bash
helm template release-name ./wire-ingress -f test-values.yaml
```

To verify routing coverage against the previous deployment, render both and compare (see the
migration guide for details).

### Schema validation

`values.schema.json` is maintained alongside `values.yaml`. Run `helm lint` to catch
misconfiguration early:

```bash
helm lint ./wire-ingress -f test-values.yaml
```

### Test values files to maintain

Create one values file per meaningful configuration variant to use with `helm template` and
`helm lint`:

| File | Purpose |
|---|---|
| `ci/values-minimal.yaml` | Only required fields, all optional features disabled |
| `ci/values-full.yaml` | All features enabled (webapp, teamSettings, accountPages, fakeS3, federator, certManager) |
| `ci/values-manual-tls.yaml` | `tls.useCertManager: false`, wildcard cert supplied manually |
| `ci/values-federator.yaml` | `federator.enabled: true` with all federator-specific settings |
| `ci/values-byo-gateway.yaml` | `gateway.create: false`, referencing an external gateway |

---

## Implementation plan

The work is split into small, independently reviewable steps. Each step touches one logical
concern and can be reviewed by running `helm template` / `helm lint` on the partial chart.

### Phase 1 — Chart scaffolding

#### Chart.yaml, .helmignore, empty values.yaml, _helpers.tpl

Create the chart skeleton with helpers for naming (`fullname`, `zone`, `getCertificateSecretName`,
`getIssuerName`, `getGatewayName`).

Use `helm create` for an initial scaffolding.

**Review:** `helm lint ./wire-ingress` passes with empty values.

- [x] Done

---

### Phase 2 — TLS

#### TLS Secret (manual mode)

Template: `templates/secret.yaml`
Condition: `!tls.useCertManager`

Encodes `secrets.tlsWildcardCert` and `secrets.tlsWildcardKey` into a `kubernetes.io/tls` Secret
referenced by the Gateway listener.

- [x] Done
- [x] Manually tested deployment

---

#### cert-manager Certificate + Issuer

Templates: `templates/certificate.yaml`, `templates/issuer.yaml`
Condition: `tls.useCertManager`

A cert-manager `Certificate` and `Issuer`/`ClusterIssuer` for ACME HTTP-01 certificate issuance.
The `secretName` produced by the `Certificate` is referenced by the Gateway listener.

- [x] Done
- [x] Manually tested http01 challenge, via issuer

### Phase 3 — Gateway

#### Optional Gateway resource

Template: `templates/gateway.yaml`

Creates a `gateway.networking.k8s.io/v1 Gateway` when `gateway.create: true`. The Gateway has:

- An HTTPS listener on port 443 with TLS termination, referencing the TLS secret

The federator listener is added in the federator phase (not here) to keep federator concerns separate.

New values:

```yaml
gateway:
  create: true
  name: ""          # default: derived from release name via helper
  className: envoy  # GatewayClass installed by Envoy Gateway
  listeners:
    http:
      port: 80
    https:
      port: 443
```

**Review:** `helm template` produces a valid `Gateway` manifest. Diff against old chart shows no
`Ingress` equivalent yet — that is expected.

- [x] Done

---

### Phase 4 — Core HTTPRoutes

Each step adds one `HTTPRoute` (or one group of closely related routes).

#### HTTPRoute — nginz (HTTPS endpoint)

Template: `templates/httproute-nginz.yaml`

Routes `config.dns.https` → `nginz` service port `http`.

The route attaches to the HTTPS listener of the Gateway via `parentRefs`.

**Review:** Renders correctly with and without TLS; `helm lint` passes.

- [x] Done
- [x] Manually tested

---

#### HTTPRoute — nginz websockets

Template: `templates/httproute-nginz-websockets.yaml`
Condition: `websockets.enabled`

Routes `config.dns.ssl` → `nginz` service port `ws`. WebSocket upgrades require no special
annotation in Envoy — they are transparent at the HTTP layer.

**Review:** Rendered only when `websockets.enabled: true`. Hostname differs from nginz route.

- [x] Done
- [x] Manually tested

---

#### HTTPRoute + Service — webapp

Templates: `templates/httproute-webapp.yaml`, `templates/service-webapp.yaml`
Condition: `webapp.enabled`

Routes `config.dns.webapp` → `webapp-http` ClusterIP service port `service.webapp.externalPort`.

**Review:** Absent from rendered output when `webapp.enabled: false`.

- [x] Done
- [x] Manually tested

---

#### HTTPRoute + Service — team-settings

Templates: `templates/httproute-team-settings.yaml`, `templates/service-team-settings.yaml`
Condition: `teamSettings.enabled`

Routes `config.dns.teamSettings` → `team-settings-http` ClusterIP service port `service.teamSettings.externalPort`.

- [x] Done
- [x] Manually tested

---

#### HTTPRoute + Service — account-pages

Templates: `templates/httproute-account-pages.yaml`, `templates/service-account-pages.yaml`
Condition: `accountPages.enabled`

Routes `config.dns.accountPages` → `account-pages-http` ClusterIP service port `service.accountPages.externalPort`.

- [x] Done
- [x] Manually tested

---

#### HTTPRoute + Service — fakeS3 / minio

Template: `templates/httproute-minio.yaml`
Condition: `fakeS3.enabled`

Routes `config.dns.fakeS3` → `fake-aws-s3` service directly (no intermediary ClusterIP service needed — the fake-aws-s3 chart creates its own service).

Access to `/minio/` paths is blocked by a `RequestRedirect` to `/` (301), placed as a prefix
match rule before the catch-all. This is standard Gateway API — no Envoy extensions required.

**Review:** Two rules rendered: one redirect rule for `/minio/` prefix, one catch-all for `/`.

- [x] Done
- [x] Manually tested assets work

---

### Phase 5 — Federator

#### Gateway listener for federator

Extend `templates/gateway.yaml` to add a separate TLS listener for `config.dns.federator` when
`federator.enabled: true`.

The federator requires its own listener so that `ClientTrafficPolicy` can enforce mTLS only on
that listener, not on the main HTTPS listener.

- [x] Done

---

#### HTTPRoute for federator

Template: `templates/httproute-federator.yaml`
Condition: `federator.enabled`

Routes `config.dns.federator` → `federator` service port `federator-ext`, attaching to the
federator listener.

Fails with an error if `config.isAdditionalIngress` is set, since federation and multi-ingress
cannot be combined.

- [x] Done

---

#### ClientTrafficPolicy for federator mTLS

Template: `templates/clienttrafficpolicy-federator.yaml`
Condition: `federator.enabled`

Envoy Gateway-specific (`gateway.envoyproxy.io/v1alpha1`). Configures:

- `tls.clientValidation.caCertificateRef` → references `federator-ca-secret`
- Forwards the client certificate as `X-SSL-Certificate` request header (implementation-specific
  header injection)

Enforces mTLS client certificate validation and forwards the client certificate as the
`X-SSL-Certificate` request header to the federator backend.

> **Incompatibility note:** The `tls.verify_depth` value is not directly mapped. Envoy Gateway's
> `ClientTrafficPolicy` does not expose a depth knob; validation depth is implicitly controlled by
> the CA chain provided in `federator-ca-secret`.

- [x] Done
- [x] Manually tested a federated request (see  check-federation-certs.sh script)

---

#### Federator TLS secret + X-SSL-Certificate header injection

Templates: `templates/secret-federator.yaml`, `templates/certificate-federator.yaml`, `templates/envoyextensionpolicy-federator.yaml`
Condition: `federator.enabled`

Creates `federator-certificate-secret` (manual mode) or a cert-manager `Certificate` with both
`server auth` and `client auth` EKUs (`tls.useCertManager: true`).

The `federator-ca` ConfigMap is **not** created by this chart — it is created by the wire-server
chart and must exist in the release namespace before deploying with `federator.enabled: true`.

The `EnvoyExtensionPolicy` injects the mTLS client certificate as the `X-SSL-Certificate` request
header via an inline Lua filter, matching nginx's `$ssl_client_escaped_cert` behaviour.

- [x] Done
- [x] Manually tested

---

### Phase 6 — Integration test helper

#### federation-test-helper Service

Template: `templates/service-test-fed.yaml`

A ClusterIP Service in `envoy-gateway-system` that selects the Envoy proxy pods for this
Gateway (using `gateway.envoyproxy.io/owning-gateway-*` labels). Because Envoy Gateway runs
proxy pods in its own namespace, a cross-namespace Service is needed to expose them for
DNS-based federation discovery.

The service is named `<namespace>-fed` and lives in `envoy-gateway-system`, so the SRV record
that Wire federation discovery resolves is:

```
_wire-server-federator._tcp.<namespace>-fed.envoy-gateway-system.svc.cluster.local
```

- [x] Done

---

### Phase 7 — Switch integration tests to wire-ingress

Replace all uses of `nginx-ingress-services` in `hack/` with `wire-ingress`.

The integration test setup deploys two federation namespaces (`namespace1`, `namespace2`) each
with their own ingress release. Both currently use `nginx-ingress-services`. The goal is to make
integration tests pass with `wire-ingress` in place of `nginx-ingress-services`.

- [x] Done
- [x] deal with the federation ingresses for the dynamic backends. remove them? they are not needed. or set up the same way as federation (but no test)
- [x] fix integration test setup so all test pass

---

### Phase 8 — custom solver secret

#### Custom ACME solver secret

Template: `templates/custom-solvers-secret.yaml`

An opaque Secret containing credentials for custom ACME challenge solvers, referenced by
`certManager.customSolvers`.

- [x] Drop this functionality and document this
- [x] Done

---

### Phase 9 — Documentation and CI values

#### Finalize PR

- [x] introduce a flag that switches between gateway / ingress-nginx in the tests
- [x] investigate: migrate federation-test helper to integrations chart? NO, becasue we would need to pass the relase name
- [x] replace "envoy-gateway-system" hardcoded namespace with chart var
- [x] feature flag for envoypatch policies (enabled by default, turned off in tests)
- [x] document: must deploy enovy-gateway with patches enabled
- [x] envoy patch policies: adjust docs: its not a kubernetes problem, but that SRV records have to by FQDM
- [x] for .Values.federator.tls.useCertManager document that you'll likely needa private CA, since many public CA stopped issuing client auth certs
- [x] organize the parameter listing in this README better 
- [x] rework patch policies so merged gateways work
- [x] test now if diya / elna federation works
- [ ] test now if integration test pass
- [ ] Write the migration guide section of this README
- [ ] move the phases out of README
- [ ] clean up PR: no stray files
- [ ] remove CLAUDE.md
- [ ] make a note on nginx-ingress-services

---

## Migration guide

_To be written after implementation. Will cover:_

- Which values files can be reused as-is
- Which keys need renaming (see backwards compatibility table above)
- How to verify the migration with `helm template` diff
- Envoy Gateway prerequisites (CRDs, controller install)
