# wire-ingress

A Helm chart for Wire server ingress using the **Kubernetes Gateway API**.

The chart targets **Envoy Gateway** as the Gateway API controller.

---

## Status

**This chart is in development. Dont use it in production yet!**

---

## Prerequisites

### Gateway API

Install the [Gateway API](https://gateway-api.sigs.k8s.io/) into your cluster.
This chart makes use of the of the kinds defined in the `gateway.networking.k8s.io/v1` API.

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

Also make sure also you've created a `GatewayClass` object with 
```
spec:
  controllerName: gateway.envoyproxy.io/gatewayclass-controller
```

You need to refer to this object in the `gateway.className` paramter.

---

## Backwards compatibility


### Migrating from the `nginx-ingress-services` chart

The chart preserves the `values.yaml` structure of the `nginx-ingress-services` chart wherever
possible. Most existing values files should work with minimal changes.

Add a `gateway` block to your values and review at least the following keys:

- `gateway.className` — set to the `GatewayClass` name created during installation (see above).
- `gateway.create` — if `false`, you must create a `Gateway` object yourself and set `gateway.name` to its name.
- `gateway.listeners.https.hostname` — set to `*.<your-domain>`. This assumes all domains under
  `config.dns.*` are subdomains of `<your-domain>`. If that is not the case, create your own
  `Gateway` and set `gateway.create: false`.
- `gateway.proxyProtocol.enabled` — set to `true` if your load balancer sends PROXY protocol headers.
- `gateway.patchPolicies.targetGatewayClass` — depends on your setup; see [EnvoyPatchPolicy](#envoypatchpolicy).
- `gateway.envoyProxy.create` and `gateway.manageServiceType` — depend on your setup; see the parameter table below.

`secrets.tlsClientCA` is no longer needed and can be removed.

### Behaviour changes

* non-tls ingress disabled by default. If you want to make use of automated certificate validation via http01, you need `gateway.listeners.http.enabled: true`
* s3 ingress b`/minio/` path blocking. Returns 301 redirect to "/" (was 403).

### New values (no equivalent in nginx-ingress-services)

Only values that require explanation are listed. Trivial or self-explanatory values (ports,
name overrides, etc.) can be found in `values.yaml`.

| Key | Default | Description |
|---|---|---|
| `gateway.create` | `true` | If `false`, no `Gateway` resource is created — set `gateway.name` to reference an existing one. Useful when sharing a Gateway across multiple releases. |
| `gateway.className` | `""` | **Required.** Name of the `GatewayClass` installed by the Envoy Gateway controller (e.g. `envoy`). Must match the `GatewayClass` object whose `spec.controllerName` is `gateway.envoyproxy.io/gatewayclass-controller`. |
| `gateway.listeners.https.hostname` | `""` | **Required when `federator.enabled: true`.** Restricts the HTTPS listener to a specific hostname (e.g. `*.example.com`). Without this, both the HTTPS and federator listeners are catch-all on the same port, causing Envoy to degrade ALPN to HTTP/1.1-only (`OverlappingTLSConfig`). |
| `gateway.listeners.http.enabled` | `false` | Enables the HTTP listener on port 80. Required for HTTP01 ACME challenges via cert-manager's `gatewayHTTPRoute` solver — see [HTTP01 certificate challenges](#http01-certificate-challenges). |
| `gateway.envoyProxy.create` | `true` | If `false`, no `EnvoyProxy` resource is created. Set `gateway.envoyProxy.name` to reference an existing one, or leave it empty to inherit the GatewayClass-level `EnvoyProxy`. |
| `gateway.envoyProxy.name` | _(derived)_ | When `create: true` — name of the created resource. When `create: false` — name of an existing `EnvoyProxy` to reference via `infrastructure.parametersRef`. |
| `gateway.envoyProxy.spec` | `{}` | Free-form [EnvoyProxySpec](https://gateway.envoyproxy.io/docs/api/extension_types/#envoyproxyspec) merged verbatim. Use to set `mergeGateways`, custom service annotations, etc. |
| `gateway.manageServiceType` | `true` | Shorthand that sets `envoyService.type` to `gateway.serviceType`. Disable when managing the service type via `gateway.envoyProxy.spec` directly. |
| `gateway.serviceType` | `LoadBalancer` | Service type for the Envoy proxy service. Only used when `gateway.manageServiceType: true`. |
| `gateway.infrastructure.annotations` | `{}` | Annotations forwarded to the LoadBalancer Service provisioned by Envoy Gateway — see [Gateway API docs](https://gateway-api.sigs.k8s.io/reference/spec/#gateway.networking.k8s.io/v1.GatewayInfrastructure). Use for cloud-specific LB settings (e.g. AWS NLB). |
| `gateway.proxyProtocol.enabled` | `false` | Creates a `ClientTrafficPolicy` enabling PROXY protocol on all listeners. Required when the upstream load balancer is configured to send PROXY protocol headers. |
| `gateway.patchPolicies.enabled` | `true` | Controls whether `` resources are created — see [EnvoyPatchPolicy](#envoypatchpolicy). |
| `gateway.patchPolicies.targetGatewayClass` | `false` | When `true`, `EnvoyPatchPolicy` targets the `GatewayClass` instead of the `Gateway`. **Required when `gateway.envoyProxy.spec.mergeGateways: true`**: with merged Gateways, policies targeting a `Gateway` are not applied — they must target the `GatewayClass`. Leave `false` for single-Gateway deployments (e.g. integration tests). |
| `gateway.controllerNamespace` | `envoy-gateway-system` | Can be ignored, relevant only for integration tests. Namespace where Envoy Gateway runs its proxy pods. Change only if Envoy Gateway was installed into a non-default namespace. |
| `tls.secret.create` | `true` | If `false`, the TLS Secret is not created by this chart. Use when the secret is managed externally (e.g. by another operator). |
| `federator.tls.useCertManager` | `true` | Controls cert-manager for the federator TLS secret independently of `tls.useCertManager`. Requires a private CA — see [Federator TLS certificate](#federator-tls-certificate-federatortlsusecertmanager). |

### Dropped values

| Old key | Reason |
|---|---|
| `config.ingressClass` | |
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

The policy patches the `RouteConfiguration` named `<namespace>/<gateway>/federator`. Route
configuration names are per-namespace even when multiple Gateways share a single Envoy proxy, so
the name is predictable from chart values.

**`gateway.patchPolicies.targetGatewayClass`** controls what the policy targets:

- **`false` (default)** — targets `kind: Gateway` by name. Use for standard single-Gateway
  deployments, including integration tests.
- **`true`** — targets `kind: GatewayClass` (using `gateway.className`). **Required when
  `gateway.envoyProxy.spec.mergeGateways: true`.** With merged Gateways, all Gateways of the same
  GatewayClass share one Envoy proxy.

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
