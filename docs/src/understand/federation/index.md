(federation-understand)=

# Wire Federation

Wire Federation aims to allow multiple Wire-server
{ref}`backends <glossary_backend>` to federate with each other: Users on on
different backends are be able to interact with each other as if they
are on the the same backend.

Federated backends are be able to identify, discover and authenticate
one-another using the domain names under which they are reachable via the
network. To enable federation, administrators of a Wire backend can decide to
either specifically list the backends that they want to federate with, or to
allow federation with all Wire backends reachable from the network. See
{ref}`configure-federation`.

```{note}
The Federation development is work in progress.
```

```{toctree}
---
maxdepth: 2
numbered: true
glob: true
---
architecture
backend-communication
*
```
