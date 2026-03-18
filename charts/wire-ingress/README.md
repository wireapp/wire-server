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
| `secrets.tlsClientCA` | No longer supplied via values. The `federator-ca` ConfigMap is created by the wire-server chart and referenced directly. |

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
| _(not present)_ | `federator.tls.useCertManager` | Controls cert-manager for the federator TLS secret independently of `tls.useCertManager` |
| _(not present)_ | `federator.tls.secretName` | Name of the TLS Secret for the federator listener. Default: `federator-certificate-secret`. When `useCertManager: true`, cert-manager writes the issued certificate into this secret. When `useCertManager: false`, the secret must exist before deploying. |
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
| `secrets.certManager.customSolversSecret` | |

### Behaviour changes

| Feature | Old behaviour | New behaviour |
|---|---|---|
| Default ACME solver | `http01.ingress.class: nginx` | `http01.gatewayHTTPRoute` targeting this chart's Gateway — requires `gateway.listeners.http.enabled: true` |
| `/minio/` path blocking | nginx `server-snippet` returning 403 | `RequestRedirect` to `/` (301) — standard Gateway API |
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

Template: `templates/federation-test-helper.yaml`

A ClusterIP service targeting the ingress controller pod, used for SRV-based discovery in
integration tests. Uses `app.kubernetes.io/` labels on Kubernetes >= 1.23, legacy labels
otherwise.

- [ ] Done

Notes:

This helper ties into the integration test setup.
Here's how the integration tests are set up with nginx-ingress-service. My goal is to make integration tests pass
when we deploy wire-ingress instead in place of nginx-ingress-servicess.

The integration test are deployed via helmfile hack/helmfile.yaml.gotmpl

The cluster in which the integration tests run has ClusterIssuer named "federation" that will sign any certs.

From /home/stefan/repos/wire-server/hack/helm_vars/wire-server/values.yaml.gotmpl
```
federator:
  tls:
    useCertManager: true
    useSharedFederatorSecret: true
```
we can see that the federator will assume that the cert used for both client and server auth is externally provide at secret "federator-certificate-secret"

In the integration test setup 2 domains federationDomain1 and federationDomain2 are configured with a namespace .
The federator will use the "federation" ClusterIssuer to obtain a cert.

Wire uses a SRV records to look up the federator domain of a domain. This is what federation-test-helper is used for. The SRV record on the service targets the ingress deployment pods which are ingressing the federator.



---

### Phase 7 — Switch integration tests to wire-ingress

Replace all uses of `nginx-ingress-services` in `hack/` with `wire-ingress`.

The integration test setup deploys two federation namespaces (`namespace1`, `namespace2`) each
with their own ingress release. Both currently use `nginx-ingress-services`. The goal is to make
integration tests pass with `wire-ingress` in place of `nginx-ingress-services`.

#### Overview of changes

Files to update:

| File | Change |
|---|---|
| `hack/helmfile.yaml.gotmpl` | Replace two `ingress-svc` release blocks |
| `hack/helmfile-federation-v0.yaml.gotmpl` | Replace one `ingress-svc` release block |
| `hack/bin/integration-setup-federation.sh` | Replace `nginx-ingress-services` in `charts` array with `wire-ingress` |
| `hack/helm_vars/nginx-ingress-services/values.yaml.gotmpl` | Keep for reference; create `hack/helm_vars/wire-ingress/values.yaml.gotmpl` |

#### Step 1 — Create `hack/helm_vars/wire-ingress/values.yaml.gotmpl`

Adapt from the existing nginx-ingress-services values. Key changes:

- Remove `config.ingressClass` → replace with `gateway.className` pointing to the Envoy GatewayClass
- Remove `secrets.tlsClientCA` (dropped — `federator-ca` ConfigMap is created by the wire-server chart)
- Add `federator.tls.useCertManager: true` with `federator.tls.issuer.name: federation` /
  `federator.tls.issuer.kind: ClusterIssuer`

The wire-ingress chart creates the cert at `federator-certificate-secret` (our default
`federator.tls.secretName`) via cert-manager using the `federation` ClusterIssuer. The federator
chart reads the same secret (via `useSharedFederatorSecret: true`). This is how both charts share
the same mTLS cert.

Expected values:
```yaml
teamSettings:
  enabled: true
accountPages:
  enabled: true
federator:
  enabled: true
  integrationTestHelper: true
  tls:
    useCertManager: true
    issuer:
      name: federation
      kind: ClusterIssuer
tls:
  useCertManager: true
  issuer:
    name: federation
    kind: ClusterIssuer
  createIssuer: false
  caNamespace: wire-federation-v0
gateway:
  className: "envoy"
config:
  dns:
    https: "nginz-https.{{ .Release.Namespace }}-integration.example.com"
    ssl: "nginz-ssl.{{ .Release.Namespace }}-integration.example.com"
    webapp: "webapp.{{ .Release.Namespace }}-integration.example.com"
    fakeS3: "assets.{{ .Release.Namespace }}-integration.example.com"
    teamSettings: "teams.{{ .Release.Namespace }}-integration.example.com"
    accountPages: "account.{{ .Release.Namespace }}-integration.example.com"
    # federator: dynamically set by helmfile
    # certificateDomain: dynamically set by helmfile
```

#### Step 2 — Update `hack/helmfile.yaml.gotmpl`

In both `ingress-svc` release blocks (namespace1 and namespace2):
- Change `chart` from `../.local/charts/nginx-ingress-services` to `../.local/charts/wire-ingress`
- Change `values` from `./helm_vars/nginx-ingress-services/values.yaml.gotmpl` to `./helm_vars/wire-ingress/values.yaml.gotmpl`
- Review the `needs: ['ingress']` dependency — `ingress` refers to the nginx-ingress-controller
  release. With wire-ingress there is no nginx controller; this dependency should be removed or
  replaced with the Envoy Gateway release if it is managed by this helmfile.

#### Step 3 — Update `hack/helmfile-federation-v0.yaml.gotmpl`

The `ingress-svc` release currently pins `wire/nginx-ingress-services` at a specific version.
Replace with a local chart reference (same as helmfile.yaml.gotmpl):
- Change `chart` to `../.local/charts/wire-ingress`
- Change `values` to `./helm_vars/wire-ingress/values.yaml.gotmpl`

#### Step 4 — Update `hack/bin/integration-setup-federation.sh`

Line 25: replace `nginx-ingress-services` with `wire-ingress` in the `charts` array.

#### Step 5 — Verify federation-test-helper pod selector

After the first deployment, check the actual labels on the Envoy Gateway proxy pods:

```bash
kubectl get pods -n <namespace> --show-labels | grep envoy
```

The template currently uses:
```yaml
selector:
  gateway.envoyproxy.io/owning-gateway-name: <gateway-name>
  gateway.envoyproxy.io/owning-gateway-namespace: <namespace>
```

Adjust `templates/federation-test-helper.yaml` if the actual labels differ.

- [ ] Done
- [ ] deal with the federation ingresses for the dynamic backends. remove them? they are not needed. or set up the same way as federation (but no test)

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
