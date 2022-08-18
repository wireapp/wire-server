**Warning**: this chart is currently considered alpha. Use at your own risk!

This chart deploys [coturn](https://github.com/wireapp/coturn), a STUN and TURN
server.

You need to supply a list of one or more zrest secrets at the key
`secrets.zrestSecrets`. The secret provided to the brig chart in
`secrets.turn.secret` must be included in this list.

Note that coturn pods are deployed with `hostNetwork: true`, as they need to
listen on a wide range of UDP ports. Additionally, some TCP ports need to be
exposed on the hosting node, which are listed in `values.yaml`.

Due to the nature of TURN, this service might also expose the
internal network to which the hosting node is connected. It is
therefore recommended to run coturn on a separate Kubernetes cluster
from the rest of the Wire services. Further details may be found in
Wire's documentation for Restund, another TURN implementation, on
[this](https://docs.wire.com/understand/restund.html#network) page.

coturn can optionally be configured to expose a TLS control
port. The TLS private key and certificate to use for this port can
be supplied manually in `secrets.tls.cert` and `secrets.tls.key`, or
[cert-manager](https://cert-manager.io) can be used to provision certificates
automatically. If cert-manager is in use, and no Issuer or ClusterIssuer is
provided in the cluster already, then this chart can create an issuer using
Let's Encrypt, though the configuration of ACME challenge solvers must be
provided by the operator manually.
