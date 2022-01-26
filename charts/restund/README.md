This chart deploys [Restund](https://docs.wire.com/understand/restund.html), a
STUN and TURN server.

You need to supply the zrestSecret at key `secrets.zrestSecret`. Make sure this
matches `secrets.turn.secret` of the brig chart.

Restund pods are deployed with `hostNetwork: true`, because restund needs to
listen on a wide range of udp ports. See `values.yaml` for additional tcp ports
that need to be exposed on the hosting node.

The Restund server might also expose the internal network to which the hosting
node is connected to. It is therefore recommended to run restund on a separate
network (cluster) than the rest of wire's services. See
[details](https://docs.wire.com/understand/restund.html#network).


## Parameters

## TLS

To enable TLS set `tls.enabled: true` and `tls.listenPort` (e.g. `5349`). For the certificates you have two options:

### Bring your own certificate
| Parameter       | Description                                                                                 |
|-----------------|---------------------------------------------------------------------------------------------|
| `tls.key`       | Private key of the TLS certificate for `host`                                               |
| `tls.crt`       | TLS certificate for `host`                                                                  |

### Cert-manager certificate

| Parameter       | Description                                                                                                                                        |
|-----------------|----------------------------------------------------------------------------------------------------------------------------------------------------|
| `tls.issuerRef` | describes what [Issuer](https://cert-manager.io/docs/reference/api-docs/#meta.cert-manager.io/v1.ObjectReference)  to use to request a certificate |



  
