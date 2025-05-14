# inconsistencies

This tool checks and can repair certain inconsistencies between the `user`, `user_keys` and `user_handle` tables in brig.

More context on how this was/is useful under these issues:

- https://wearezeta.atlassian.net/browse/SQSERVICES-1798
- https://wearezeta.atlassian.net/browse/SQSERVICES-1797

(A precursor tool to check inconsistencies between spar and brig tables, if deemed useful, could be exhumed from git history of [PR #2840](https://github.com/wireapp/wire-server/pull/2840) or [one commit](https://github.com/wireapp/wire-server/pull/2840/commits/2e06428d10508328bcf2d829b16a7cc75ee72386) and incorporated here)

This tool writes findings into an output file as JSON lines, so it can be more easily analysed. The tool should run on a cluster (as opposted to through port-forwarding from a local machine) for speed. Though please do watch metrics when running this as a few thousand parallelized table scan pagination requests per second can have a performance impact on the whole database.

## How to run and make sense of data

1. Build image

```
make build-image-inconsistencies
```

2. Push image

```
docker push <image-including-tag-from-above-output>
```

3. Run it in K8s using this pod yaml **Update image field and args appropriately**:

Inside the affected cluster's context (e.g. `targets/wire/staging/app`), open a PR with a pod manifest file that can be created using `kubectl apply -f <filename>`

```yaml
apiVersion: v1
kind: Pod
metadata:
  name: inconsistencies
  labels:
    app: inconsistencies
spec:
  restartPolicy: Never
  containers:
  - name: inconsistencies
    image: <image-with-tag>
    imagePullPolicy: Always
    args:
    - handle-less-users # adjust to the command you need, see Options.hs
    - --cassandra-host-brig
    - brig-brig-eks-service.databases
    - --cassandra-keyspace-brig
    - brig
    - --inconsistencies-file
    - /tmp/inconsistencies.log
```

4. Wait for the process to finish. Watch logs, it will say something like "sleeping for 4 hours" and then close all connections to cassandra.

5. Copy the logs using `kubectl cp`

```
kubectl cp inconsistencies:/tmp/inconsistencies.log inconsistencies.log
```

6. **IMPORTANT:** Delete the pod. The easiest way to do this is with `kubectl delete -f <filename>` (which also deletes any configmap)

7. Convert logs into CSV:

```bash
cat inconsistencies.log |
    jq -r '[.userId, .status.value, .status.writetime, .userHandle.value, .userHandle.writetime, .handleClaimUser.value, .handleClaimUser.writetime] | @csv' >! inconsistencies.csv
```

You can look at this data using any tool comfortable.

8. From a CSV file, you may extract only handles/emails/keys to feed into repair using awk/grep:

```bash
cat inconsistencies.csv | awk -F ',' '{print $1}' | grep -v '^"+' | xargs -n 1 echo > dangling-email-keys.txt
```

## How to repair some data

First, you need to extract a list of emails/handles/UUIDs you wish to repair. The code will still perform checks on whether these inputs actually need any kind of repairing (backfilling into tables or removing from tables).

You can run the same container with additional flags of the command, a configmap with values (for simplicity called `input`), and the `--repair-data` flag. See source code under `Options.hs`.

At least the following are supported:

- `missing-email-keys` (and a mounted configmap containing newline-separated UUIDs)
- `dangling-handles` (and a mounted configmap containing newline-separated handles)
- `dangling-keys` (and a mounted configmap containing newline-separated emails)

Example:

```yaml
apiVersion: v1
kind: Pod
metadata:
  name: inconsistencies
  labels:
    app: inconsistencies
spec:
  restartPolicy: Never
  containers:
  - name: inconsistencies
    image: quay.io/wire/inconsistencies:<tag>
    imagePullPolicy: Always
    args:
    - missing-email-keys
    - --input-file
    - /input/input
    - --repair-data
    - --cassandra-host-brig
    - brig-brig-eks-service.databases
    - --cassandra-keyspace-brig
    - brig
    - --inconsistencies-file
    - /inconsistencies.log
    volumeMounts:
     - name: input
       mountPath: "/input"
       readOnly: true
  volumes:
  - name: input
    configMap:
      name: input
---
apiVersion: v1
kind: ConfigMap
metadata:
  name: input
data:
  input: |
    2a7de2ba-754c-11ed-b14d-00163e5e6c00
    3049c812-754c-11ed-b56e-00163e5e6c00
    ...
```

Apply as usual, should execute quickly, and make sure to export inconsistencies.log and check actual logs, then delete the resources created (`kubectl delete -f ...`)
