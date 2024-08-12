# smallstep-acomp - Helm chart accompanying smallstep

This Helm chart is meant to be installed alongside the [step-certificates Helm
chart](https://smallstep.github.io/helm-charts) in the same namespace. It has been tested with Helm
chart version `1.25.0` and image

```
image:
    repository: cr.step.sm/smallstep/step-ca
    tag: "0.25.3-rc7"
```

This Helm chart provides:

- A reverse-proxy for Certificate Revocation List (CR) distribution endpoints to federating smallstep
  servers
- Smallstep server configuration for the End-to-End Identity setup


## Reverse-proxy for CRL distribution points

This Helm chart installs a reverse proxy that proxies the Certificate Revocation List (CRL)
Distribution Point of the Smallstep servers CRL Certificate Authority (CA) from federating domains
and the own domain. This reverse proxy is required for a working End-to-End Identity setup.

The Helm chart deploys a nginx server that reverse-proxies
`https://<nginx.ingress.hostname>/proxyCrl/<other_acme_domain>` to `https://{other_acme_domain}/crl`
as well as an ingress for the `/proxyCrl` endpoint. For example if `upstreams.proxiedHosts` is set
to `[acme.alpha.example.com, acme.beta.example.com]` and the host for the Smallstep server on the
own domain is `acme.alpha.example.com` this helm chart will forward requests

- `https://acme.alpha.example.com/proxyCrl/acme.alpha.example.com` to `https://acme.alpha.example.com/crl`
- `https://acme.alpha.example.com/proxyCrl/acme.beta.example.com` to `https://acme.beta.example.com/crl`

| Name                       | Description                                                                                               |
| -------------------------- | --------------------------------------------------------------------------------------------------------- |
| `upstreams.enable`         | Set to `false` in case you want to write custom nginx server block for the upstream rules                 |
| `upstreams.dnsResolver`    | DNS server that nginx uses to resolve the proxied hostnames                                               |
| `upstreams.proxiedHosts`   | List of remote (federated) step-ca hostnames to proxy. Also include the own step-ca host here.            |
| `nginx.ingress.enable`     | Set to `false` if you need to define a custom ingress for the /proxyCrl endpoint. Make sure CORS is set.  |
| `nginx.ingress.hostname`   | Hostname of the step-ca server                                                                            |
| `nginx.ingress.extraTls`   | The TLS configuration                                                                                     |
| `nginx.ingress.annotations`| CORS config for the ingress, set the hostname of the step-ca server here                                  |

For more details on `nginx.*` parameters see README.md documentation in the `nginx` dependency chart.

## Smallstep server configuration for the End-to-End Identity setup

This Helm chart helps to create configuration file for step-ca. If `stepConfig.enabled` is `true` a
configmap that contains a `ca.json` will be created. The name of that configmap is compatible with the
step-certificates Helm chart, so that it can be directly used. However since step-ca is deployed
from a seperate Helm release updating and deploying a configuration won't cause an automatic reload
of the step-ca server. It is therefore recommended to manually restart step-ca after configuartion
changes if this Helm chart is used for these purposes.

For references see:

- [[1] Configuring `step-ca`](https://smallstep.com/docs/step-ca/configuration/)
- [[2] Configuring `step-ca` Provisioners - ACME for Wire messenger clients ](https://smallstep.com/docs/step-ca/provisioners/#acme-for-wire-messenger-clients)

| Parameter                                            | Description                                                                                                                       |
|------------------------------------------------------|-----------------------------------------------------------------------------------------------------------------------------------|
| `stepConfig.enabled`                                 | Create a configmap with configuration file for `step-certificates` Helm chart.                                                    |
|                                                      | If `true` then almost all `stepConfig.*` parameters are required.                                                                 |
| `stepConfig.configTemplate`                          | Template for the configuration file. Overwrite this if the default value is not generic enough for your use case.                 |
| `stepConfig.address`                                 | See [1]                                                                                                                           |
| `stepConfig.dnsName`                                 | Used in `dnsNames` config entry (See [1]) and to define the CRL URL.                                                              |
| `stepConfig.additionalDNSNames`                      | Optional. Additional entries to `dnsNames` configuration field                                                                    |
| `stepConfig.root`                                    | See [1]. Public key of the Root CA                                                                                                |
| `stepConfig.crt`                                     | See [1]. Public key of the Intermediate CA                                                                                        |
| `stepConfig.key`                                     | See [1]. Private key of the Intermediate CA                                                                                       |
| `stepConfig.federatedRoots`                          | See [1]. Add all cross-signed Intermediate CA certs from federating domains here.                                                 |
| `stepConfig.db`                                      | See [1]                                                                                                                           |
| `stepConfig.tls`                                     | See [1]                                                                                                                           |
| `stepConfig.logger`                                  | See [1]                                                                                                                           |
| `stepConfig.authority.claims`                        | See [1]                                                                                                                           |
| `stepConfig.authority.jwk`                           | JSON string of the JWK provisioner to use. A JWK provisioner can be created                                                       |
|                                                      | by running `step ca init` then copying it out of the generated `ca.json`, discarding the `ca.json`.                               |
| `stepConfig.authority.acme.name`                     | Name of the ACME provisioner. Default: `"keycloakteams"`                                                                          |
| `stepConfig.authority.acme.claims`                   | See [1]                                                                                                                           |
| `stepConfig.authority.acme.dpop.key`                 | See [2]. Public half of the DPoP signature key bundle configured of the Wire deployment.                                          |
|                                                      | Use the same value as `brig.secrets.dpopSigKeyBundle` value of the `wire-server` Helm chart.                                       |
|                                                      | Base64 encoded string of the PEM encoded public key.                                                                              |
| `stepConfig.authority.acme.dpop.wireDomain`          | Set this to the federation domain of the backend                                                                                  |
| `stepConfig.authority.acme.oidc.clientId`            | Name of the OIDC client. Default: "wireapp".                                                                                      |
| `stepConfig.authority.acme.oidc.discoveryBaseUrl`    | OpenID Connect Discovery endpoint. The OIDC provider must respond with its configuration when `/.well-known/openid-configuration` |
|                                                      | is appended to the URL. For Keycloak this URL is of format `https://<keycloak-host>/auth/realms/<realm-name>`.                    |
| `stepConfig.authority.acme.oidc.issuerUrl`           | For Keycloak this must be of the format `https://<keycloak-host>/auth/realms/<realm-name>?client_id=wireapp`                      |
| `stepConfig.authority.acme.oidc.signatureAlgorithms` | See [2]                                                                                                                           |
| `stepConfig.authority.acme.oidc.transform`           | See [2]. Has sensible default. There shouldn't be any need to customize this setting.                                             |
| `stepConfig.authority.acme.x509.organization`        | Set this to the federation domain of the backend                                                                                  |
| `stepConfig.authority.acme.x509.template`            | See [2]. Go template for a client certificate which is evaluated by step-ca.                                                      |
|                                                      | This string is evaluated as template of the Helm chart first.                                                                     |
|                                                      | Has a sensible default. There shouldn't be a need to customize this setting.                                                      |

| Parameter             | Description                                                                                           |
|-----------------------|-------------------------------------------------------------------------------------------------------|
| `caPassword.enabled`  | If `true` generate Secret with a name that the `step-certificates` Helm chart will automatically use. |
|                       | The Helm chart will mount this at `/home/step/secrets/passwords/password`.                            |
| `caPassword.password` | Password that decrypts the intermediate CA private key                                                |

| Parameter                 | Description                                                                                           |
|---------------------------|-------------------------------------------------------------------------------------------------------|
| `existingSecrets.enabled` | If `true` generate Secret with a name that the `step-certificates` Helm chart will automatically use. |
| `existingSecrets.data`    | Map from filename to content. Each entry will be mounted as file `/home/step/secrets/<filename>`      |
|                           | Add the private key of the Intermediate CA here.                                                      |

| Parameter               | Description                                                                                         |
|-------------------------|-----------------------------------------------------------------------------------------------------|
| `existingCerts.enabled` | If `true` generate ConfigMap with a name that the Helm chart will automatically use.               |
| `existingCerts.data`    | Map from filename to content. Each entry will be mounted as file `/home/step/certs/<filename>`      |
| `existingCerts.data`    | Use it to make public keys of the Root, intermediate CA as well as the cross-signed certs available |
|                         | to step-ca. Each entry will be mounted as file `/home/step/certs/<filename>`                        |
