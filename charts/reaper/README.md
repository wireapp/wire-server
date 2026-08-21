Reaper
------

This pod is useful in the following scenario: You run wire-server alongside a single
redis-ephemeral (part of databases-ephemeral). If you have a different setup for redis,
do not use this chart.

Due to the nature of pods and their ephemerality, there might be situations where a
redis-ephemeral pod is restarted. In such cases, wire clients will have stale
connections (they will have an active websocket connection, but gundeck (responsible for
sending messages) will be unaware of this (as the record of who is connected where is
gone with a redis-ephemeral restart). So these stale clients will not receive any
messages. Here, this reaper will check that the `redis-ephemeral` pod is older than any
other `cannon`; if that is not the case, it kills the `cannon`s forcing clients to
reconnect.

Image
-----

The reaper runs `scripts/reaper.sh` through `kubectl`, so `image` must point at a
kubectl image that **contains a POSIX shell** at `/bin/sh`. Distroless kubectl images
do not ship one and the pod will fail to start. The script itself is POSIX sh, so
busybox `ash` is enough, bash not required.

The image is fully configurable:

```yaml
image:
  registry: docker.io      # set to "" for an unqualified repository
  repository: alpine/kubectl
  tag: 1.36.3
  digest: ""               # e.g. "sha256:..."; takes precedence over tag
  pullPolicy: IfNotPresent
imagePullSecrets:
  - name: my-pull-secret
```

RBAC
----

The chart creates a namespaced `Role`/`RoleBinding` granting `get`, `list`, `watch` and
`delete` on pods, bound to a `<release>-reaper` ServiceAccount.

`watch` is required even though the script never watches anything explicitly:
`kubectl delete pod` blocks until the pod is gone and opens a watch to do so. Without it
the reaper deletes the first cannon and then hangs, without crashing.

Earlier versions bound the ServiceAccount to `cluster-admin` through a fixed-name
`ClusterRoleBinding`, which gave the pod read access to every Secret in the cluster.
`helm upgrade` removes that binding and the old `reaper-role` ServiceAccount. Because
nothing is cluster-scoped any more and all names are release-scoped, several reaper
releases can now coexist in one cluster; previously a second release failed to install
with a `ClusterRoleBinding` ownership conflict.

Runtime
-------

The container runs as uid/gid 65534 with a read-only root filesystem and has resource
requests and limits. `nodeSelector`, `tolerations` and `affinity` are honoured.

`checkIntervalSeconds` (default `15`) controls how long the script waits between checks.
Earlier versions listed pods once per second.

Logs distinguish a failure to reach the API from "there are no matching pods", and
include the underlying error:

    Failed to list pods: Error from server (Forbidden): ... Skipping this iteration...
    No cannon pods found. Doing nothing...

Both cases previously printed `Failed to list pods. Skipping this iteration...`, so a
reaper that could not list pods at all looked exactly like an idle one.
