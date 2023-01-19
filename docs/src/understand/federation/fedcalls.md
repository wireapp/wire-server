# Federated API calls by client API end-point (generated)

**Updated manually using using [the fedcalls tool](https://github.com/wireapp/wire-server/blob/8760b4978ccb039b229d458b7a08136a05e12ff9/tools/fedcalls/README.md); last change: 2023-01-16.**

This is most likely only interesting for backend developers.

This graph and csv file describe which public (client) API end-points trigger calls to which end-points at backends federating with the one that is called.  The data is correct by construction (see [the fedcalls tool](https://github.com/wireapp/wire-server/blob/8760b4978ccb039b229d458b7a08136a05e12ff9/tools/fedcalls/README.md) for more details).

The target can only be understood in the context of the [backend code base](https://github.com/wireapp/wire-server/).  It is described by component (sub-directory in `/services`) and end-point name (use grep to find it).

links:

- [dot](img/wire-fedcalls.dot)
- [png](img/wire-fedcalls.png)
- [csv](img/wire-fedcalls.csv)

```{image} img/wire-fedcalls.png
```
