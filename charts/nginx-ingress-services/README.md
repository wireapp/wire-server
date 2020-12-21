This helm chart is a helper to set up needed services, ingresses and (likely) secrets to access your cluster.
It will _NOT_ deploy an ingress controller! Ensure you already have one on your cluster - or have a look at our [nginx-ingress-controller](../nginx-ingress-controller/README.md)

If tls.enabled == true, then you need to supply 2 variables, `tlsWildcardCert` and `tlsWildcardKey` that could either be supplied as plain text in the form of a `-f path/to/secrets.yaml`, like this:

```
secrets:
  tlsWildcardCert: |
    -----BEGIN CERTIFICATE-----
    ... (Your Primary SSL certificate) ...
    -----END CERTIFICATE-----
    -----BEGIN CERTIFICATE-----
    ... (Your Intermediate certificate) ...
    -----END CERTIFICATE-----
  tlsWildcardKey: |
    -----BEGIN PRIVATE KEY-----
    ...
    -----END PRIVATE KEY-----
```

or encrypted with `sops` and then use `helm-wrapper`. As an alternative, it is possible to use `cert-manager` (see further down below).

Have a look at the [values file](values.yaml) for different configuration options.

### Common issues

Q: My ingress keeps serving "Kubernetes Ingress Controller Fake Certificate"!!

A: Ensure that your certificate is _valid_ and has _not expired_; trying to serve expired certificates will silently fail and the nginx ingress will simply fallback to the default certificate.


## About cert-manager

### Prerequisites

* `cert-manager` and its CRDs have to be installed upfront, 
   e.g. `helm upgrade --install -n cert-manager-ns --set 'installCRDs=true' cert-manager jetstack/cert-manager`,
   because upstream decided that this is the way (https://github.com/jetstack/cert-manager/pull/2964)


### What does this chart do?

* define `Ingress` for various services and their corresponding FQDNS
* do TLS termination either by explicitly providing a wildcard certificate or letting
  *cert-manager* take care of this
* [optional] configure an *Issuer* to issue ACME HTTP01 certificates provided by Letsencrypt
* [optional] define a *Certificate* representation that causes *cert-manager* to issue a
  certificate that is then used by `Ingress` 


### Todo when introducing support for K8s >= 1.15 

* the `apiVersion` of all resources based on cert-manager's CRDs, namely `./templates/issuer.yaml` and 
  `./templates/certificate.yaml`, has to be changed to `cert-manager.io/v1alpha3`


### Monitoring

__FUTUREWORK:__ When `wire-server-metrics` is ready, expiration & renewal should be integrated into monitoring.
