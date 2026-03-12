# wire-ingress

A Helm chart for Wire server ingress using the **Kubernetes Gateway API**.

The chart targets **Envoy Gateway** as the Gateway API controller.

---

## Status

**This chart is in development. Dont use it in production yet!**

---

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

### GatewayClass is not created

`GatewayClass` is installed by the Envoy Gateway Helm chart and is cluster-scoped. This chart only
references it by name via `gateway.className`.

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

### Federator mTLS uses Envoy Gateway policies

Federator mTLS is implemented using:

- `ClientTrafficPolicy` to configure TLS settings on the federator `Gateway` listener (client
  certificate validation, verify depth)
- A separate `Gateway` listener (or dedicated `Gateway`) for the federator so that mTLS settings
  apply only to that listener
- The `X-SSL-Certificate` header forwarding is handled via Envoy Gateway's `HTTPRouteFilter` with
  request header injection from the client cert (implementation-specific)

---

## Backwards compatibility

The chart preserves the `values.yaml` structure of `nginx-ingress-services` wherever possible.
Operators should be able to reuse most of their existing values files with minimal changes.

### Renamed / restructured values

| Old key | New key | Notes |
|---|---|---|
| `config.ingressClass` | `gateway.className` | Different concept: GatewayClass name, not IngressClass |
| _(not present)_ | `gateway.create` | `true` = chart creates the Gateway; `false` = BYO |
| _(not present)_ | `gateway.name` | Name of the Gateway to attach routes to |
| _(not present)_ | `gateway.infrastructure.annotations` | Annotations forwarded to the LoadBalancer Service provisioned by Envoy Gateway — see [Gateway API docs](https://gateway-api.sigs.k8s.io/reference/spec/#gateway.networking.k8s.io/v1.GatewayInfrastructure) |
| _(not present)_ | `gateway.proxyProtocol.enabled` | Creates a `ClientTrafficPolicy` enabling PROXY protocol on all Gateway listeners — required when the load balancer is configured to send PROXY protocol headers |
| _(not present)_ | `tls.secret.create` | If `false`, the TLS Secret is not created by this chart — use when the secret is managed externally (e.g. by another chart or operator). `secrets.tlsWildcardCert` and `secrets.tlsWildcardKey` are ignored when `false`. |
| _(not present)_ | `tls.secret.nameOverride` | Override the name of the TLS Secret referenced by the Gateway listener. If not set, the name is derived from the release name. |

### Dropped values (not applicable to Gateway API)

| Old key | Reason |
|---|---|
| `ingressName` | Multi-ingress out of scope |
| `config.isAdditionalIngress` | Multi-ingress out of scope |
| `config.renderCSPInIngress` | Multi-ingress out of scope |
| `config.dns.base` | Only used for CSP header rendering, which is a multi-ingress feature |
| `kubeVersionOverride` | Deprecated; the federation-test-helper label selector no longer needs a version override |
| `tls.verify_depth` | Envoy Gateway `ClientTrafficPolicy` does not expose a direct verify-depth knob; the CA chain itself controls this |
| `tls.enabled` | This is removed since it didnt ahve any effect. All ingresses are always defined with TLS. |

### Fully backwards compatible values

All keys below are accepted unchanged. Their names, types, and semantics are identical to
`nginx-ingress-services`.

| Key | Notes |
|---|---|
| `nameOverride` | |
| `teamSettings.enabled` | |
| `accountPages.enabled` | |
| `websockets.enabled` | |
| `webapp.enabled` | |
| `fakeS3.enabled` | |
| `federator.enabled` | |
| `federator.integrationTestHelper` | |
| `federator.tls.duration` | |
| `federator.tls.renewBefore` | |
| `federator.tls.privateKey.rotationPolicy` | |
| `federator.tls.issuer.name` | |
| `federator.tls.issuer.kind` | |
| `federator.tls.issuer.group` | |
| `tls.useCertManager` | |
| `tls.createIssuer` | |
| `tls.privateKey.rotationPolicy` | |
| `tls.privateKey.algorithm` | |
| `tls.privateKey.size` | |
| `tls.issuer.name` | |
| `tls.issuer.kind` | |
| `tls.caNamespace` | |
| `certManager.inTestMode` | |
| `certManager.certmasterEmail` | |
| `certManager.customSolvers` | |
| `service.webapp.externalPort` | |
| `service.s3.externalPort` | |
| `service.s3.serviceName` | |
| `service.useFakeS3` | |
| `service.teamSettings.externalPort` | |
| `service.accountPages.externalPort` | |
| `config.dns.https` | |
| `config.dns.ssl` | |
| `config.dns.webapp` | |
| `config.dns.fakeS3` | |
| `config.dns.federator` | |
| `config.dns.certificateDomain` | |
| `config.dns.teamSettings` | |
| `config.dns.accountPages` | |
| `secrets.tlsWildcardCert` | |
| `secrets.tlsWildcardKey` | |
| `secrets.tlsClientCA` | |
| `secrets.certManager.customSolversSecret` | |

### Behaviour changes

| Feature | Old behaviour | New behaviour |
|---|---|---|
| Default ACME solver | `http01.ingress.class: nginx` | `http01.gatewayHTTPRoute` targeting this chart's Gateway — requires `gateway.listeners.http.enabled: true` |
| `/minio/` path blocking | nginx `server-snippet` returning 403 | Envoy Gateway `HTTPRouteFilter` `directResponse` (implementation-specific, non-standard) |
| Federator client cert header | `nginx.ingress.kubernetes.io/configuration-snippet` setting `X-SSL-Certificate` | Envoy Gateway `ClientTrafficPolicy` + `HTTPRouteFilter` header injection |
| Websocket routing | Separate host with port name `ws` | Same `HTTPRoute` — WebSocket upgrades are handled transparently by Envoy |

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

---

#### HTTPRoute — websockets

Template: `templates/httproute-websockets.yaml`
Condition: `websockets.enabled`

Routes `config.dns.ssl` → `nginz` service port `ws`. WebSocket upgrades require no special
annotation in Envoy — they are transparent at the HTTP layer.

**Review:** Rendered only when `websockets.enabled: true`. Hostname differs from nginz route.

- [ ] Done

---

#### HTTPRoute — webapp

Template: `templates/httproute-webapp.yaml`
Condition: `webapp.enabled`

Routes `config.dns.webapp` → `webapp-http` service port `externalPort`.

**Review:** Absent from rendered output when `webapp.enabled: false`.

- [ ] Done

---

#### HTTPRoute — team-settings

Template: `templates/httproute-team-settings.yaml`
Condition: `teamSettings.enabled`

Routes `config.dns.teamSettings` → `team-settings-http` service port `externalPort`.

- [ ] Done

---

#### HTTPRoute — account-pages

Template: `templates/httproute-account-pages.yaml`
Condition: `accountPages.enabled`

Routes `config.dns.accountPages` → `account-pages-http` service port `externalPort`.

- [ ] Done

---

#### HTTPRoute — fakeS3 / minio

Template: `templates/httproute-minio.yaml`
Condition: `fakeS3.enabled`

Routes `config.dns.fakeS3` → `fake-aws-s3` service.

Access to `/minio/` paths is blocked with a 403 using an Envoy Gateway `HTTPRouteFilter` with
`type: DirectResponse` and `statusCode: 403`, placed as a prefix match rule before the catch-all.

> **Incompatibility note:** `DirectResponse` is an Envoy Gateway extension
> (`gateway.envoyproxy.io/v1alpha1`). It is not part of the standard Gateway API spec.

**Review:** Two rules rendered: one 403 rule for `/minio/` prefix, one catch-all for `/`.

- [ ] Done


---

### Phase 5 — Services

#### ClusterIP Services

Template: `templates/service.yaml`

Creates `webapp-http`, `s3-http`, `team-settings-http`, and `account-pages-http` ClusterIP
services with their respective selectors and ports.

**Review:** `helm template` produces the expected set of ClusterIP services for the enabled features.

- [ ] Done

---

### Phase 6 — Federator

#### Gateway listener for federator

Extend `templates/gateway.yaml` to add a separate TLS listener for `config.dns.federator` when
`federator.enabled: true`.

The federator requires its own listener so that `ClientTrafficPolicy` can enforce mTLS only on
that listener, not on the main HTTPS listener.

- [ ] Done

---

#### HTTPRoute for federator

Template: `templates/httproute-federator.yaml`
Condition: `federator.enabled`

Routes `config.dns.federator` → `federator` service port `federator-ext`, attaching to the
federator listener.

Fails with an error if `config.isAdditionalIngress` is set, since federation and multi-ingress
cannot be combined.

- [ ] Done

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

- [ ] Done

---

#### Federator TLS secrets + CA secret

Templates: `templates/secret-federator.yaml`, `templates/ca-federator.yaml`
Condition: `federator.enabled`

Creates `federator-certificate-secret` and `federator-ca-secret`. When `tls.useCertManager` is
enabled, a cert-manager `Certificate` with both `server auth` and `client auth` EKUs is created
in `templates/certificate-federator.yaml`.

- [ ] Done

---

### Phase 7 — Integration test helper

#### federation-test-helper Service

Template: `templates/federation-test-helper.yaml`

A ClusterIP service targeting the ingress controller pod, used for SRV-based discovery in
integration tests. Uses `app.kubernetes.io/` labels on Kubernetes >= 1.23, legacy labels
otherwise.

- [ ] Done

---

### Phase 8 — custom solver secret

#### Custom ACME solver secret

Template: `templates/custom-solvers-secret.yaml`

An opaque Secret containing credentials for custom ACME challenge solvers, referenced by
`certManager.customSolvers`.

- [ ] Done

---

### Phase 9 — Documentation and CI values

#### Finalize README, migration guide, and ci/ values files

- Write the migration guide section of this README
- Create `ci/values-minimal.yaml`, `ci/values-full.yaml`, etc.
- Ensure `helm lint` and `helm template` pass for all CI values files

- [ ] Done

---

## Migration guide

_To be written after implementation. Will cover:_

- Which values files can be reused as-is
- Which keys need renaming (see backwards compatibility table above)
- How to verify the migration with `helm template` diff
- Envoy Gateway prerequisites (CRDs, controller install)
