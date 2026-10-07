# wire-ingress

A Helm chart for Wire server ingress using the **Kubernetes Gateway API**.

The chart targets **Envoy Gateway** as the Gateway API controller.

---

## Status

**This chart is in development. Don't use it in production yet! See FUTUREWORK below**

---

## Prerequisites

### Gateway API

Install the [Gateway API](https://gateway-api.sigs.k8s.io/) into your cluster.
This chart makes use of the kinds defined in the `gateway.networking.k8s.io/v1` API.

You must use install it in the same namespace as the `wire-server` helm chart, otherwise references will not work.
FUTUREWORK: Make this helm chart a subchart of `wire-server` before releasing it and remove this paragraph.

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

Also make sure you've created a `GatewayClass` object with 
```
spec:
  controllerName: gateway.envoyproxy.io/gatewayclass-controller
```

You need to refer to this object in the `gateway.className` parameter.

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

### Behavior changes

* non-tls ingress disabled by default. If you want to make use of automated certificate validation via http01, you need `gateway.listeners.http.enabled: true`
* s3 ingress `/minio/` path blocking. Returns 301 redirect to "/" (was 403).

### New values (no equivalent in nginx-ingress-services)

Only values that require explanation are listed. Trivial or self-explanatory values (ports,
name overrides, etc.) can be found in `values.yaml`.

| Key | Default | Description |
|---|---|---|
| `gateway.create` | `true` | If `false`, no `Gateway` resource is created — set `gateway.name` to reference an existing one. Useful when sharing a Gateway across multiple releases. |
| `gateway.className` | `""` | **Required.** Name of the `GatewayClass` installed by the Envoy Gateway controller (e.g. `envoy`). Must match the `GatewayClass` object whose `spec.controllerName` is `gateway.envoyproxy.io/gatewayclass-controller`. |
| `gateway.alpn.enabled` | `true` | Enables ALPN configuration via `ClientTrafficPolicy` to support HTTP/2 despite overlapping certificate SANs across multiple service listeners. When disabled, ALPN defaults to HTTP/1.1 only. |
| `gateway.alpn.protocols` | `[h2, http/1.1]` | List of ALPN protocols to advertise to clients. Defaults to HTTP/2 with HTTP/1.1 fallback. |
| `FIPS_202205_tls_profile` | `false` | Override all TLS settings to use FIPS_202205 compliance. Before setting this to true, read the section [TLS profiles](#tls-profiles). |
| `gateway.tls.enabled` | `true` | Configure TLS parameters on all HTTPS listeners. Must remain enabled with the FIPS profile. |
| `gateway.tls.minVersion` | `"1.3"` | Minimum TLS version. |
| `gateway.tls.maxVersion` | `"1.3"` | Maximum TLS version. |
| `gateway.tls.ciphers` | Four ECDHE ECDSA/RSA AES-GCM suites (see values) | TLS <=1.2 only. Omitted when minVersion is 1.3; does not constrain TLS 1.3 suites. |
| `gateway.tls.ecdhCurves` | `["X25519MLKEM768", "X25519", "P-256", "P-384", "P-521"]` | Clients and the Envoy crypto library must support this group. |
| `gateway.tls.signatureAlgorithms` | `[]` | Optional signature preferences; also affects federation client authentication. |
| `gateway.patchPolicies.xdsNameSchemeV2` | _(unset)_ | Must match the controller's `XDSNameSchemeV2` runtime flag. Required only when `FIPS_202205_tls_profile` is on — see [xDS name scheme](#xds-name-scheme). |
| `gateway.patchPolicies.xdsListenerName` | `""` | Legacy scheme only. Overrides the xDS listener name the FIPS patch targets, for an externally created Gateway that declares another listener first on the HTTPS port. |
| `gateway.listeners.https.sectionName` | `https` | Name of the HTTPS listener section. Change only to match an externally created Gateway — see [External Gateways](#external-gateways). |
| `gateway.listeners.federator.sectionName` | `federator` | Name of the federation listener section. Same reason as above. |
| `config.domains[].sectionName` | _(derived)_ | Per-domain listener section override for multi-ingress. Defaults to `https-<name>`; the first entry uses `gateway.listeners.https.sectionName`. |
| `gateway.extraHttpsListeners` | `[]` | Extra named HTTPS listeners on the same port, with `hostname` and optional `certificateSecretName`; useful for admin hostnames outside the API wildcard. Attach routes explicitly and issue a matching certificate. The FIPS patch covers these listeners too. |
| `tls.extraDnsNames` | `[]` | Additional certificate SANs for companion routes on this Gateway. |
| `gateway.listeners.http.enabled` | `false` | Enables the HTTP listener on port 80. Required for HTTP01 ACME challenges via cert-manager's `gatewayHTTPRoute` solver — see [HTTP01 certificate challenges](#http01-certificate-challenges). |
| `gateway.envoyProxy.create` | `true` | If `false`, no `EnvoyProxy` resource is created. Set `gateway.envoyProxy.name` to reference an existing one, or leave it empty to inherit the GatewayClass-level `EnvoyProxy`. |
| `gateway.envoyProxy.name` | _(derived)_ | When `create: true` — name of the created resource. When `create: false` — name of an existing `EnvoyProxy` to reference via `infrastructure.parametersRef`. |
| `gateway.envoyProxy.spec` | `{}` | Free-form [EnvoyProxySpec](https://gateway.envoyproxy.io/docs/api/extension_types/#envoyproxyspec) merged verbatim. Use to set resource requests, custom service annotations, etc. |
| `gateway.manageServiceType` | `true` | Shorthand that sets `envoyService.type` to `gateway.serviceType`. Disable when managing the service type via `gateway.envoyProxy.spec` directly. |
| `gateway.serviceType` | `LoadBalancer` | Service type for the Envoy proxy service. Only used when `gateway.manageServiceType: true`. |
| `gateway.envoyProxy.replicas` | `3` | Proxy pod count for three AZs — see [Availability of the proxy fleet](#availability-of-the-proxy-fleet). |
| `gateway.envoyProxy.topologySpreadKeys` | node and zone | Topology keys to spread the proxy pods over. Advisory (`ScheduleAnyway`). `[]` for none. |
| `gateway.zoneAwareRouting.enabled` | `true` | Prefer same-zone backend endpoints to cut inter-AZ traffic cost — see [Zone-aware routing](#zone-aware-routing). |
| `gateway.zoneAwareRouting.minEndpointsThreshold` | `3` | Below this many backend endpoints across all zones, Envoy balances normally. |
| `gateway.annotations` | `{}` | Annotations on the `Gateway` object itself, e.g. for external-dns' `gateway-httproute` source. Not propagated to the proxy Service. |
| `gateway.infrastructure.labels` | `{}` | Labels forwarded to the resources Envoy Gateway generates. Gateway API >= v1.1. |
| `gateway.infrastructure.annotations` | `{}` | Annotations forwarded to the LoadBalancer Service provisioned by Envoy Gateway — see [Gateway API docs](https://gateway-api.sigs.k8s.io/reference/spec/#gateway.networking.k8s.io/v1.GatewayInfrastructure). Use for cloud-specific LB settings (e.g. AWS NLB). |
| `gateway.proxyProtocol.enabled` | `false` | Enables PROXY protocol on all listeners (via the Gateway-wide `ClientTrafficPolicy`). Required when the upstream load balancer is configured to send PROXY protocol headers. |
| `gateway.patchPolicies.enabled` | `true` | Controls whether `EnvoyPatchPolicy` resources are created — see [EnvoyPatchPolicy](#envoypatchpolicy). |
| `gateway.patchPolicies.targetGatewayClass` | `false` | When `true`, `EnvoyPatchPolicy` targets the `GatewayClass` instead of the `Gateway`. **Required when `gateway.envoyProxy.spec.mergeGateways: true`**: with merged Gateways, policies targeting a `Gateway` are not applied — they must target the `GatewayClass`. Leave `false` for single-Gateway deployments (e.g. integration tests). |
| `gateway.controllerNamespace` | `envoy-gateway-system` | Can be ignored, relevant only for integration tests. Namespace where Envoy Gateway runs its proxy pods. Change only if Envoy Gateway was installed into a non-default namespace. |
| `tls.secret.create` | `true` | If `false`, the TLS Secret is not created by this chart. Use when the secret is managed externally (e.g. by another operator). |
| `federator.tls.useCertManager` | `true` | Controls cert-manager for the federator TLS secret independently of `tls.useCertManager`. Requires a private CA — see [Federator TLS certificate](#federator-tls-certificate-federatortlsusecertmanager). |

### Dropped values

| Old key | Reason |
|---|---|
| `config.ingressClass` | |
| `ingressName` | Replaced by `config.domains[].name` — see [Multi-ingress (multiple backend domains)](#multi-ingress-multiple-backend-domains) |
| `config.isAdditionalIngress` | Implicit — every `config.domains` entry after the first is an additional ingress |
| `config.renderCSPInIngress` | CSP is injected automatically on additional domains (team-settings route only); opt out per-domain with `config.domains[].renderCSP: false` |
| `config.dns.base` | Replaced by `config.domains[].base` (used for the per-domain CSP wildcard) |
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

### Gateway creation is optional

The chart can optionally create a `Gateway` resource (controlled by `gateway.create: true`).
When `gateway.create: false`, all `HTTPRoute` and policy resources still reference the gateway by
name (`gateway.name`). This allows operators to share a Gateway across multiple charts or manage it
separately.

The default values create the Gateway. The default `gateway.name` is derived from the release name,
so that self-referencing is consistent by default.

An externally created Gateway must be in the release namespace and its listener sections must match
`gateway.listeners.https.sectionName` and `gateway.listeners.federator.sectionName`, because
`HTTPRoute` resources attach by section name and policies cannot cross namespaces. See
[External Gateways](#external-gateways) for the full list, including the `FIPS_202205_tls_profile`
case.

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

### Availability of the proxy fleet

When both `gateway.create` and `gateway.envoyProxy.create` are true, the chart defaults to
three proxy replicas, a PDB with `minAvailable: 1`, and advisory node/zone spread constraints.
This targets a three-AZ deployment; advisory spreading does not guarantee one pod per zone.

The PDB is derived from the final replica count, including `envoyProxy.spec` overrides:
one replica disables it; otherwise it uses `minAvailable: 1`. There is no separate PDB setting.

### Zone-aware routing

Same-zone backends are preferred by default (`gateway.zoneAwareRouting.enabled: true`). Requires nodes
labelled `topology.kubernetes.io/zone` and Envoy Gateway's topology injector. The WebSocket
policy inherits this setting while retaining its disabled idle timeout.

`minEndpointsThreshold: 3` enables zone preference for backends with one replica in each of
three AZs. It counts endpoints per upstream cluster across all zones, not per zone. Backend
placement and capacity during an AZ failure must still be managed separately.

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

The policy patches the `RouteConfiguration` named
`<namespace>/<gateway>/<gateway.listeners.federator.sectionName>`. Route configuration names are
per-namespace even when multiple Gateways share a single Envoy proxy, so the name is predictable
from chart values. It is also independent of the `XDSNameSchemeV2` runtime flag — for TLS
listeners the route config keeps the section-based name — so this policy needs no changes for
Envoy Gateway 1.10. See [xDS name scheme](#xds-name-scheme).

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

### Multi-ingress (multiple backend domains)

Set `config.domains` **instead of** `config.dns` to serve several domains from one release:

```yaml
config:
  domains:
    - name: blueberry
      base: blueberry.example.com
      dns: { https: nginz-https.blueberry.example.com, ssl: nginz-ssl.blueberry.example.com, webapp: webapp.blueberry.example.com }
    - name: red
      base: red.example.org
      dns: { https: nginz-https.red.example.org, ssl: nginz-ssl.red.example.org, webapp: webapp.red.example.org }
      tls: { issuer: { name: letsencrypt-red, kind: ClusterIssuer } }  # optional per-domain issuer
```

First entry = primary (listener `https`, un-suffixed names, no injected CSP — apps set their own).
Each additional entry gets its own listener `https-<name>`, cert/secret, suffixed routes, and an
injected per-domain CSP header on the team-settings route (opt out with `renderCSP: false`).

The webapp and account-pages routes never get an injected CSP, on any domain: both apps emit
correct per-domain headers themselves, and the injected header would replace them with a weaker
approximation. This matches the hosts the legacy `nginx-ingress-services` chart skips in its CSP
snippet. Team-settings does not yet support this, hence the approximation there.

Multi-ingress is mutually exclusive with federation: `config.domains` cannot be
combined with `federator.enabled: true`. Use federation with a single backend
domain (`config.dns`), or multi-ingress (`config.domains`) with the federator
disabled — setting both fails template rendering with a clear error.

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

### One Gateway-wide ClientTrafficPolicy

ALPN, TLS parameters and PROXY protocol are all rendered into a *single*
`ClientTrafficPolicy` (`<gateway>-client-traffic`): policies for the same target
conflict rather than merge. Federation's section policy replaces the Gateway
policy, so it repeats these settings alongside client-certificate validation.

By default, explicit ALPN `[h2, http/1.1]` allows HTTP/2 even with overlapping
certificate SANs across listeners, while retaining HTTP/1.1 support.

### TLS profiles

**By default**, this chart requires TLS 1.3 and uses all three TLS 1.3 ciphers. The X25519MLKEM768 Post-Quantum Traditional (PQ/T) Hybrid Key Exchange is also allowed.

You can specify different TLS settings, like:

```yaml
gateway:
  tls:
    minVersion: "1.2"
    ecdhCurves: [X25519MLKEM768, P-256, P-384]
```

OR you can also make use of the top-level `FIPS_202205_tls_profile` variable:

#### FIPS_202205 profile and BSI TR-02102-2 limitations

```yaml
FIPS_202205_tls_profile: true
gateway:
  className: envoy
  patchPolicies:
    enabled: true
    targetGatewayClass: false
    # must match the controller: (usually false for envoy gateay < 1.9 and true for envoy gateway >= 1.10 unless set explicitly). See xDS name scheme below.
    xdsNameSchemeV2: false
```

The `FIPS_202205` patch overrides `gateway.tls` with AES-GCM, P-256/P-384 and
TLS 1.2–1.3; it cannot be combined with TLS 1.3-only or PQ settings. 

The policy enables these six suites. 

| TLS version | Cipher suite | Available with ECDSA-only server certificates |
|---|---|---|
| 1.2 | `ECDHE-ECDSA-AES128-GCM-SHA256` | Yes |
| 1.2 | `ECDHE-ECDSA-AES256-GCM-SHA384` | Yes |
| 1.2 | `ECDHE-RSA-AES128-GCM-SHA256` | No |
| 1.2 | `ECDHE-RSA-AES256-GCM-SHA384` | No |
| 1.3 | `TLS_AES_128_GCM_SHA256` | Yes |
| 1.3 | `TLS_AES_256_GCM_SHA384` | Yes |

An ECDSA-only server certificate (recommended) leaves four: the two ECDSA TLS 1.2 suites and both TLS 1.3 suites.

Also read the cipher allowlist in
[BSI TR-02102-2](https://www.bsi.bund.de/SharedDocs/Downloads/EN/BSI/Publications/TechGuidelines/TG02102/BSI-TR-02102-2.pdf?__blob=publicationFile),

Requires (on the Envoy Proxy) a`extensionApis.enableEnvoyPatchPolicy: true` and a dedicated, unmerged
Gateway — which this chart may or may not have created, see
[External Gateways](#external-gateways). `mergeGateways` and
`patchPolicies.targetGatewayClass` are rejected: both change the generated
listener names this patch targets. Tested with EG 1.8.3 / Envoy 1.38.3. The patch
covers all TLS filter chains on the HTTPS socket, including federation and extra
listeners. 

#### What else is needed for BSI TR-02102-2 conformance?

For **BSI TR-02102-2 conformance through the end of 2031** under edition 2026-01
enabling this profile is only the TLS-parameter step. Operators must also:

- Use ECDSA P-256/P-384 server certificates on every listener, including externally
  supplied certificates. The chart checks main certificates it issues, not external
  secrets. The [BoringSSL policy](https://boringssl.googlesource.com/boringssl/+/HEAD/include/openssl/ssl.h)
  still permits RSA PKCS#1 v1.5 handshake signatures and overrides signature
  preferences; using an RSA server key would leave that unwanted option available.
- Verify certificate chains use recommended signatures and key sizes (Sections
  3.3.3, 3.4.3 and 3.6). **RSA PKCS#1 v1.5 ceased to be recommended after 2025**
  for both TLS 1.2 handshake signatures and certificate signatures (Tables 7 and
  12). This does not exclude RSA-PSS, which remains recommended. An ECDSA leaf
  alone does not fix a PKCS#1-signed chain; do not assume the default Let's Encrypt
  chain is suitable. The tested ECDSA chain was anchored at ISRG Root X2.
- For federation/mTLS, also enforce approved client certificate chains and client
  handshake signatures. An ECDSA server does not prevent RSA PKCS#1 client
  authentication. Restrict client credentials to an approved ECDSA profile or
  independently enforce the permitted signature schemes; the FIPS flag does not
  enforce this restriction.
- Audit every other public TLS terminator and the remaining TR requirements,
  including authentication, key handling and random-number generation. This chart
  does not establish whole-system conformance.

The 2031 horizon applies to TLS 1.2 and classical-only P-256/P-384 key agreement
(Tables 6 and 10), not just cipher suites. Plan migration before 2032 and review
newer BSI editions; this profile is not a guarantee against future guideline changes.

For operator acceptance, scan every public hostname for both allowed and
forbidden suites, protocols, groups and signatures; verify the served certificate
chain too. Repeat after proxy upgrades and certificate renewal. A rendered Helm
policy or a Wire-client-only test is not evidence of server-side enforcement.

See the [Envoy Gateway patch documentation](https://gateway.envoyproxy.io/v1.8/tasks/extensibility/envoy-patch-policy/).

##### xDS name scheme

The patch targets a generated xDS listener by name, and Envoy Gateway has two
naming schemes selected by the controller-wide `XDSNameSchemeV2` runtime flag:

| Scheme | Listener name | Default in |
|---|---|---|
| Legacy | `<gateway-namespace>/<gateway-name>/<listener-section>` | EG <= 1.9 |
| V2 | `<protocol>-<port>`, e.g. `tcp-443` | EG >= 1.10 |

`gateway.patchPolicies.xdsNameSchemeV2` must state which one the controller uses.
It is deliberately unset by default and rendering **fails** until you set it,
because there is no safe default: EG flips it in 1.10, and either stale value
produces a name that matches nothing. That failure is quiet — the patch lands in
the policy's `ResourceNotFound` condition, the rest of the policy still applies,
and the listener keeps the restrictive TLS 1.2-only baseline. The manifest looks
correct while the profile is not in effect. Check both:

```sh
kubectl get envoypatchpolicy -n <ns> <release>-gateway-bsi \
  -o jsonpath='{.status.ancestors[*].conditions[*]}' | jq
egctl config envoy-proxy listener -n envoy-gateway-system <pod> | jq '.. | .name? // empty'
```

Only the FIPS **listener** patch is affected. The federation patch targets a
`RouteConfiguration`, and `routeConfigName()` delegates TLS listeners to
`httpsListenerFilterChainName()`, which ignores the flag — so federation needs no
migration for EG 1.10.

Under the legacy scheme the socket is named after the **first** listener on the
port, not the one you might expect: EG builds one xDS listener per address+port
and the first Gateway listener it encounters supplies the name. For a Gateway
created by this chart that is `gateway.listeners.https.sectionName`. If an
external Gateway declares a different listener first, set
`gateway.patchPolicies.xdsListenerName` to the name from the `egctl` dump above.

##### External Gateways

`FIPS_202205_tls_profile` works with `gateway.create: false`. Nothing about the
patch requires this chart to own the Gateway; it attaches by name. The
prerequisites are:

- **Same namespace.** `EnvoyPatchPolicy.spec.targetRef` is a
  `LocalPolicyTargetReference` and `ClientTrafficPolicy.spec.targetRefs` a
  `LocalPolicyTargetReferenceWithSectionName` — neither has a `namespace` field
  (Gateway API dropped it in v1.1). A policy only affects a target in its own
  namespace. This is not FIPS-specific: every HTTPRoute in this chart pins
  `parentRefs[].namespace` to the release namespace, so a Gateway elsewhere
  receives no routes at all.
- **Matching listener section names.** HTTPRoutes attach by section name. Either
  name the external listeners `https` and `federator`, or point
  `gateway.listeners.https.sectionName` / `gateway.listeners.federator.sectionName`
  at whatever they are called.
- **`allowedRoutes` admitting this namespace** on those listeners.
- **This chart's `ClientTrafficPolicy` must win.** The patch adds a key inside
  `common_tls_context.tls_params`, which EG emits only when a
  `ClientTrafficPolicy` sets TLS parameters. Policies targeting the same object
  do not merge — the oldest wins and the other is `Conflicted`. If the cluster
  operator already attached one to the shared Gateway, ours is ignored,
  `tls_params` is absent, and the patch matches nothing. Verify with
  `kubectl get clienttrafficpolicy -n <ns> -o wide`.

Because of the routing constraint above, "externally created" in practice means
the same Gateway shape this chart would have rendered, provisioned by Terraform
or a cluster operator instead of Helm.


### Federator mTLS uses Envoy Gateway policies

Federator mTLS is implemented using:

- `ClientTrafficPolicy` to configure TLS settings on the federator `Gateway` listener (client
  certificate validation, verify depth)
- A separate `Gateway` listener for the federator so that mTLS settings apply only to that listener
- `X-SSL-Certificate` header forwarding is handled via an `EnvoyExtensionPolicy` with an inline
  Lua filter that reads the URL-encoded PEM client certificate from the connection and injects it
  as a request header, matching nginx's `$ssl_client_escaped_cert` behaviour
