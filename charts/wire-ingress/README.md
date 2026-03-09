# wire-ingress

A replacement for [`nginx-ingress-services`](../nginx-ingress-services/README.md) that uses the
**Kubernetes Gateway API** instead of the classic `networking.k8s.io/v1 Ingress` API.

The chart targets **Envoy Gateway** as the Gateway API controller.

---

## Status

**This chart is in planning. No implementation exists yet.**

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

The `ingressName` / `isAdditionalIngress` pattern from `nginx-ingress-services` is not implemented.
Single-domain deployments are the only supported topology. Multi-domain support can be added later.

### Federator mTLS uses Envoy Gateway policies

The old chart used nginx annotations (`auth-tls-verify-client`, `auth-tls-verify-depth`, and a
`configuration-snippet` to forward `X-SSL-Certificate`). The new chart uses:

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

### Dropped values (not applicable to Gateway API)

| Old key | Reason |
|---|---|
| `ingressName` | Multi-ingress out of scope |
| `config.isAdditionalIngress` | Multi-ingress out of scope |
| `config.renderCSPInIngress` | Multi-ingress out of scope |
| `config.dns.base` | Only used for CSP header rendering, which is a multi-ingress feature |
| `tls.verify_depth` | Envoy Gateway `ClientTrafficPolicy` does not expose a direct verify-depth knob; the CA chain itself controls this |

### Fully backwards compatible values

All keys below are accepted unchanged. Their names, types, and semantics are identical to
`nginx-ingress-services`.

| Key | Notes |
|---|---|
| `nameOverride` | |
| `kubeVersionOverride` | |
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
| `tls.enabled` | |
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
| `/minio/` path blocking | nginx `server-snippet` returning 403 | Envoy Gateway `HTTPRouteFilter` `directResponse` (implementation-specific, non-standard) |
| Federator client cert header | `nginx.ingress.kubernetes.io/configuration-snippet` setting `X-SSL-Certificate` | Envoy Gateway `ClientTrafficPolicy` + `HTTPRouteFilter` header injection |
| Websocket routing | Separate host with port name `ws` | Same `HTTPRoute` — WebSocket upgrades are handled transparently by Envoy |

---

## Testing strategy

### Rendering diff

For each implementation step, render both charts with the same values and compare:

```bash
helm template release-name ../nginx-ingress-services -f test-values.yaml > /tmp/old.yaml
helm template release-name ./wire-ingress           -f test-values.yaml > /tmp/new.yaml
diff /tmp/old.yaml /tmp/new.yaml
```

The goal is not an identical diff (the API objects are different) but to confirm that every
resource in the old chart has a counterpart and that no routing rule is silently dropped.

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

#### Step 1: Chart.yaml, .helmignore, empty values.yaml, _helpers.tpl

Create the chart skeleton. Helpers replicate the naming helpers from `nginx-ingress-services`
(`fullname`, `zone`, `getCertificateSecretName`, `getIssuerName`) plus new helpers for Gateway
naming (`getGatewayName`).

**Review:** `helm lint ./wire-ingress` passes with empty values.

---

#### Step 2: values.schema.json

Define a JSON Schema that validates the same top-level structure as `nginx-ingress-services` plus
the new `gateway.*` keys. This is written before any templates so that schema gaps are caught
early.

**Review:** `helm lint` with each `ci/values-*.yaml` file passes or produces only expected
validation errors.

---

### Phase 2 — Gateway

#### Step 3: Optional Gateway resource

Template: `templates/gateway.yaml`

Creates a `gateway.networking.k8s.io/v1 Gateway` when `gateway.create: true`. The Gateway has:

- An HTTP listener on port 80 (for ACME HTTP-01 challenges and optional redirect)
- An HTTPS listener on port 443 with TLS termination, referencing the TLS secret

The federator listener is added in Step 12 (not here) to keep federator concerns separate.

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

---

### Phase 3 — Core HTTPRoutes

Each step adds one `HTTPRoute` (or one group of closely related routes).

#### Step 4: HTTPRoute — nginz (HTTPS endpoint)

Template: `templates/httproute-nginz.yaml`

Routes `config.dns.https` → `nginz` service port `http`.

Gateway API equivalent of the `nginz-https` rule in the old `Ingress`. The route attaches to the
HTTPS listener of the Gateway via `parentRefs`.

**Review:** Renders correctly with and without TLS; `helm lint` passes.

---

#### Step 5: HTTPRoute — websockets

Template: `templates/httproute-websockets.yaml`
Condition: `websockets.enabled`

Routes `config.dns.ssl` → `nginz` service port `ws`. WebSocket upgrades require no special
annotation in Envoy — they are transparent at the HTTP layer.

**Review:** Rendered only when `websockets.enabled: true`. Hostname differs from nginz route.

---

#### Step 6: HTTPRoute — webapp

Template: `templates/httproute-webapp.yaml`
Condition: `webapp.enabled`

Routes `config.dns.webapp` → `webapp-http` service port `externalPort`.

**Review:** Absent from rendered output when `webapp.enabled: false`.

---

#### Step 7: HTTPRoute — team-settings

Template: `templates/httproute-team-settings.yaml`
Condition: `teamSettings.enabled`

Routes `config.dns.teamSettings` → `team-settings-http` service port `externalPort`.

---

#### Step 8: HTTPRoute — account-pages

Template: `templates/httproute-account-pages.yaml`
Condition: `accountPages.enabled`

Routes `config.dns.accountPages` → `account-pages-http` service port `externalPort`.

---

#### Step 9: HTTPRoute — fakeS3 / minio

Template: `templates/httproute-minio.yaml`
Condition: `fakeS3.enabled`

Routes `config.dns.fakeS3` → `fake-aws-s3` service.

The old chart used a nginx `server-snippet` to return 403 for `/minio/` paths. This chart uses an
Envoy Gateway `HTTPRouteFilter` with `type: DirectResponse` and `statusCode: 403` for a
`/minio/` prefix match rule, placed before the catch-all rule.

> **Incompatibility note:** `DirectResponse` is an Envoy Gateway extension
> (`gateway.envoyproxy.io/v1alpha1`). It is not part of the standard Gateway API spec.

**Review:** Two rules rendered: one 403 rule for `/minio/` prefix, one catch-all for `/`.

---

### Phase 4 — TLS

#### Step 10: TLS Secret (manual mode)

Template: `templates/secret.yaml`
Condition: `!tls.useCertManager`

Identical in structure to the old chart. Encodes `secrets.tlsWildcardCert` and
`secrets.tlsWildcardKey` into a `kubernetes.io/tls` Secret referenced by the Gateway listener.

---

#### Step 11: cert-manager Certificate + Issuer

Templates: `templates/certificate.yaml`, `templates/issuer.yaml`
Condition: `tls.useCertManager`

Identical in structure and values to the old chart. The cert-manager `Certificate` spec is
unchanged; the `secretName` it produces is referenced by the Gateway listener (Step 3).

---

### Phase 5 — Services

#### Step 12: ClusterIP Services

Template: `templates/service.yaml`

Identical to the old chart. Creates `webapp-http`, `s3-http`, `team-settings-http`,
`account-pages-http` ClusterIP services. The selectors and ports are unchanged.

**Review:** `helm template` output for this file should be byte-for-byte identical to the old
chart's `service.yaml` output (same values → same services).

---

### Phase 6 — Federator

#### Step 13: Gateway listener for federator

Extend `templates/gateway.yaml` to add a separate TLS listener for `config.dns.federator` when
`federator.enabled: true`.

The federator requires its own listener so that `ClientTrafficPolicy` can enforce mTLS only on
that listener, not on the main HTTPS listener.

---

#### Step 14: HTTPRoute for federator

Template: `templates/httproute-federator.yaml`
Condition: `federator.enabled`

Routes `config.dns.federator` → `federator` service port `federator-ext`, attaching to the
federator listener.

Blocks combination with `config.isAdditionalIngress` (same guard as the old chart, kept for
documentation purposes even though multi-ingress is out of scope).

---

#### Step 15: ClientTrafficPolicy for federator mTLS

Template: `templates/clienttrafficpolicy-federator.yaml`
Condition: `federator.enabled`

Envoy Gateway-specific (`gateway.envoyproxy.io/v1alpha1`). Configures:

- `tls.clientValidation.caCertificateRef` → references `federator-ca-secret`
- Forwards the client certificate as `X-SSL-Certificate` request header (implementation-specific
  header injection)

Maps to the old nginx annotations:
- `nginx.ingress.kubernetes.io/auth-tls-verify-client: "on"`
- `nginx.ingress.kubernetes.io/auth-tls-secret`
- `nginx.ingress.kubernetes.io/configuration-snippet` (X-SSL-Certificate)

> **Incompatibility note:** The `tls.verify_depth` value is not directly mapped. Envoy Gateway's
> `ClientTrafficPolicy` does not expose a depth knob; validation depth is implicitly controlled by
> the CA chain provided in `federator-ca-secret`.

---

#### Step 16: Federator TLS secrets + CA secret

Templates: `templates/secret-federator.yaml`, `templates/ca-federator.yaml`
Condition: `federator.enabled`

Identical in structure to the old chart (`federator-certificate-secret`,
`federator-ca-secret`). cert-manager `Certificate` for federator is also kept in
`templates/certificate-federator.yaml` with the same spec as the old chart.

---

### Phase 7 — Integration test helper

#### Step 17: federation-test-helper Service

Template: `templates/federation-test-helper.yaml`

Identical to the old chart. The Kubernetes version label selector switch (>= 1.23) is kept.

---

### Phase 8 — custom solver secret

#### Step 18: Custom ACME solver secret

Template: `templates/custom-solvers-secret.yaml`

Identical to the old chart.

---

### Phase 9 — Documentation and CI values

#### Step 19: Finalize README, migration guide, and ci/ values files

- Write the migration guide section of this README
- Create `ci/values-minimal.yaml`, `ci/values-full.yaml`, etc.
- Ensure `helm lint` and `helm template` pass for all CI values files

---

## Migration guide

_To be written after implementation. Will cover:_

- Which values files can be reused as-is
- Which keys need renaming (see backwards compatibility table above)
- How to verify the migration with `helm template` diff
- Envoy Gateway prerequisites (CRDs, controller install)
