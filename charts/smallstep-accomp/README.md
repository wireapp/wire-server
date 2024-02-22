This Helm chart installs a reverse proxy that proxies the Certificate Revocation List (CRL) Distribution Point of the Smallstep servers CRL Certificate Authority (CA) from federating domains and the own domain. It is required to be installed alongside the Smallstep helm chart.

The Helm chart deploys a nginx server that reverse-proxies `https://<nginx.ingress.hostname>/proxyCrl/<other_acme_domain>` to `http://{other_acme_domain}/crl` (note: http, not https) as well as an ingress for the `/proxyCrl` endpoint. For example if `upstreams.proxiedHosts` is set to `[acme.alpha.example.com, acme.beta.example.com]` and the host for the Smallstep server on the own domain is `acme.alpha.example.com` this helm chart will forward requests

- `https://acme.alpha.example.com/acme.alpha.example.com` to `http://acme.alpha.example.com/crl`
- `https://acme.alpha.example.com/acme.beta.example.com` to `http://acme.beta.example.com/crl`

## Parameters

| Name                      | Description                                                                               |
| ------------------------- | ----------------------------------------------------------------------------------------- |
| `upstreams.enable`        | Set to `false` in case you want to write custom nginx server block for the upstream rules |
| `upstreams.dnsResolver`   | DNS server that nginx uses to resolve the proxied hostnames                               |
| `upstreams.proxiedHosts`  | List of smallstep hostnames to proxy. Please also include the own smallstep host here     |
| `nginx.ingress.enable`    | Set to `false` in case you'd like to define a custom ingress for the /proxyCtrl endpoint  |
| `nginx.ingress.hostname`  | Hostname of the smallstep server                                                          |

For more details on `nginx.*` parameters see README.md documentation in the `nginx` dependency chart.
