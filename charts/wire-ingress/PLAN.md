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
- [x] test now if integration test pass
- [x] Write the migration guide section of this README
- [x] move the phases out of README
- [x] clean up PR: no stray files
- [x] remove CLAUDE.md

---

## Migration guide

_To be written after implementation. Will cover:_

- Which values files can be reused as-is
- Which keys need renaming (see backwards compatibility table above)
- How to verify the migration with `helm template` diff
- Envoy Gateway prerequisites (CRDs, controller install)
