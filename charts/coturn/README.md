**Warning**: this chart is currently considered alpha. Use at your own risk!

This chart deploys [coturn](https://github.com/coturn/coturn), a STUN and TURN
server.

You need to supply the zrestSecret at key `secrets.zrestSecret`. Make sure this
matches `secrets.turn.secret` of the brig chart.

Note that coturn pods are deployed with `hostNetwork: true`, as they need to
listen on a wide range of UDP ports. Additionally, some TCP ports need to be
exposed on the hosting node, which are listed in `values.yaml`.

Due to the nature of TURN, this service might also expose the
internal network to which the hosting node is connected. It is
therefore recommended to run coturn on a separate Kubernetes cluster
from the rest of the Wire services. Further details may be found in
Wire's documentation for Restund, another TURN implementation, on
[this](https://docs.wire.com/understand/restund.html#network) page.
