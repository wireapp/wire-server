# Isolated test on backend-integration-tests

This deploys the current fan-in prototype to the cluster described by
`cailleach/targets/ops/backend-integration-tests` (Talos on Hetzner), without
Terraform or access to its state. It creates
resources only in a dedicated `fan-in-perf-*` namespace. It does not provision,
repair, scale, or reconfigure the cluster. The checked-in configuration defines
14 worker nodes; live capacity must be checked before choosing three nodes. This
cluster also runs CI: coordinate the test window and node selection with its users.
A separate namespace does not isolate CPU, memory, disk or network contention.

The first target is a reproducible distributed baseline, not production readiness:

- One Gundeck and one Cannon on the services node, each with 2 CPU/2 GiB and `-N2`.
- PostgreSQL 17 + pg_partman, Cassandra, Redis, RabbitMQ, ElasticMQ and Prometheus
  on the dependencies node (4.5 CPU and roughly 10 GiB requested in total).
- A separate generator node, with a 2 CPU/2 GiB Job and `-N2`.

Choose three distinct Ready, schedulable nodes with enough **unallocated** capacity;
8-vCPU service/dependency nodes and a 4-vCPU generator node are sensible starting
sizes. Match the images' architecture. The generator's separation is enforced by
node selectors. Dependency contention remains a measurement limitation; move
PostgreSQL to a fourth node before attributing a dependency-node bottleneck to
fan-in. Requests equal limits to keep the resource budget explicit. Record CPU
throttling when interpreting results.

Only synthetic data is used. No Brig, Galley, ingress, external database, or real
SNS is needed. ElasticMQ provides the SQS queue required at Gundeck startup; SNS
points to a closed loopback port. Internal HTTP/DB traffic is unencrypted inside
this test namespace. NetworkPolicy restricts traffic to the namespace plus DNS;
verify that the cluster CNI enforces it (including DNS configuration). Do not run
this unauthenticated benchmark API on a cluster without namespace isolation.
Pods receive no Kubernetes service-account token. Never use production secrets.

## Build the images

Use this checkout for all four Haskell images, so the services and generator match.
The existing Nix image builds include runtime libraries; copying local executables
into an arbitrary image is not sufficient. From the repository root, for each of
`cannon`, `gundeck`, `gundeck-schema`, and `fan-in-perf`:

```bash
set -o pipefail
exe=cannon  # repeat for the other three executables
nix build ".#wireServer.imagesNoDocs.$exe" --out-link "/tmp/fan-in-image-$exe" 2>&1 | grep -vE 'Compiling|Linking|Preprocessing|Configuring|Building'
"/tmp/fan-in-image-$exe" | docker load
```

`docker load` prints the loaded image reference. Tag that reference for a registry
the cluster nodes can pull from, and push it. Use a unique tag for the exact source
revision (include a suffix for uncommitted changes); record its digest. Build and
push the PostgreSQL image too:

```bash
docker build -t YOUR_REGISTRY/fan-in-perf-postgres:17.11-partman tools/fan-in-perf/docker/postgres
docker push YOUR_REGISTRY/fan-in-perf-postgres:17.11-partman
```

Use your organization's approved/scanned dependency images and preferably pin all
images by digest. The example dependencies follow the local test environment,
except for the standalone Prometheus server. This setup does not certify images
as vulnerability-free.

## Configure and review

Use Cailleach's existing kubeconfig below. Run these commands from the wire-server
repository root with Cailleach's tools and Python dependencies available. Its
authentication helper may require AWS credentials or an interactive OIDC login;
Terraform state is not required. Do not use `aws eks update-kubeconfig`: this
cluster is on Hetzner, not EKS.

```bash
export KUBECONFIG="$PWD/cailleach/targets/ops/backend-integration-tests/2_kubeaccess/kubeconfig"
export PATH="$PWD/cailleach/bin:$PWD/cailleach/.env/bin:$PATH"
export PERF_CONTEXT=backend-integration-tests
export PERF_NAMESPACE=fan-in-perf-ci
export PERF_CONFIG=/tmp/fan-in-ci.json
export PERF_DIR=tools/fan-in-perf/kubernetes
kubectl --context "$PERF_CONTEXT" cluster-info
kubectl --context "$PERF_CONTEXT" get nodes -o wide
kubectl --context "$PERF_CONTEXT" top nodes
kubectl --context "$PERF_CONTEXT" describe nodes
kubectl --context "$PERF_CONTEXT" get storageclass
cp "$PERF_DIR/config.example.json" "$PERF_CONFIG"
```

`top nodes` shows current utilization; `describe nodes` shows allocated requests,
limits and taints. Check both, since low utilization does not imply unreserved
capacity. If metrics-server is unavailable, use the cluster monitoring instead.
Do not remove node taints or evict CI workloads to make the benchmark fit.

Edit the config: namespace (must equal `$PERF_NAMESPACE`), three node hostname
labels, five built image references, optional storage class and image-pull Secret
names. The default storage class must support dynamically provisioned volumes on
these nodes. Create any registry pull Secrets in this namespace before launching
pods. This renderer deliberately rejects unfilled example values.

```bash
python3 "$PERF_DIR/render.py" "$PERF_CONFIG" infra > /tmp/fan-in-infra.json
python3 "$PERF_DIR/render.py" "$PERF_CONFIG" schema > /tmp/fan-in-schema.json
python3 "$PERF_DIR/render.py" "$PERF_CONFIG" services > /tmp/fan-in-services.json
```

Inspect these files before applying. All Services are ClusterIP. No existing Wire
resources are modified. Use a fresh namespace for an independent experiment.

## Start dependencies and initialize once

Create the namespace and synthetic credentials without writing passwords to the
repository or passing them as command-line arguments:

```bash
kubectl --context "$PERF_CONTEXT" create namespace "$PERF_NAMESPACE"
python3 - <<'PY' | kubectl --context "$PERF_CONTEXT" -n "$PERF_NAMESPACE" create -f -
import json, secrets
print(json.dumps({"apiVersion":"v1", "kind":"Secret",
  "metadata":{"name":"benchmark-passwords"}, "type":"Opaque",
  "stringData":{"postgres-password":secrets.token_hex(32),
                "rabbit-user":"benchmark", "rabbit-password":secrets.token_hex(32)}}))
PY
kubectl --context "$PERF_CONTEXT" apply -f /tmp/fan-in-infra.json
kubectl --context "$PERF_CONTEXT" -n "$PERF_NAMESPACE" wait --for=condition=Available deployment --all --timeout=600s
kubectl --context "$PERF_CONTEXT" apply -f /tmp/fan-in-schema.json
kubectl --context "$PERF_CONTEXT" -n "$PERF_NAMESPACE" wait --for=condition=Complete job/gundeck-schema job/postgres-schema --timeout=900s
kubectl --context "$PERF_CONTEXT" apply -f /tmp/fan-in-services.json
kubectl --context "$PERF_CONTEXT" -n "$PERF_NAMESPACE" wait --for=condition=Available deployment/gundeck deployment/cannon --timeout=300s
```

Do not proceed past a failed command. Inspect `kubectl ... describe pod` and
`kubectl ... logs job/postgres-schema` (or the failing service). Readiness probes
check listening ports; the smoke test checks the complete delivery path. Schema
Jobs have no automatic retry. The PostgreSQL Job refuses to reset a database with
existing public tables. Do not delete/recreate schema Jobs to reset a running test.

## Smoke, load and results

```bash
python3 "$PERF_DIR/render.py" "$PERF_CONFIG" smoke --run-id first > /tmp/fan-in-smoke.json
kubectl --context "$PERF_CONTEXT" create -f /tmp/fan-in-smoke.json
kubectl --context "$PERF_CONTEXT" -n "$PERF_NAMESPACE" logs -f job/smoke-first
kubectl --context "$PERF_CONTEXT" -n "$PERF_NAMESPACE" wait --for=condition=Complete job/smoke-first --timeout=600s

python3 "$PERF_DIR/render.py" "$PERF_CONFIG" load --run-id baseline-1 --devices 1000 --rate 10 --duration 300 > /tmp/fan-in-load.json
kubectl --context "$PERF_CONTEXT" create -f /tmp/fan-in-load.json
kubectl --context "$PERF_CONTEXT" -n "$PERF_NAMESPACE" logs -f job/load-baseline-1 | tee /tmp/fan-in-baseline-1.txt
kubectl --context "$PERF_CONTEXT" -n "$PERF_NAMESPACE" wait --for=condition=Complete job/load-baseline-1 --timeout=1200s
```

If logs are requested while a pod is still Pending, wait until it starts and repeat
the logs command. The Job's final status, not `tee`'s exit code, determines success.
Jobs are retained so logs and the JSON summary can be retrieved afterward. Use a
new run ID each time. Run only one load Job at a time: all Jobs share one fixture.
Previous participants/events persist, so use a fresh namespace/database for clean
comparisons. Do not reset the database while workers are running.

Prometheus scrapes Cannon and Gundeck every five seconds. In another terminal:

```bash
kubectl --context "$PERF_CONTEXT" -n "$PERF_NAMESPACE" port-forward service/prometheus 9090:9090
```

Open `http://localhost:9090` and query:

```promql
histogram_quantile(0.95, sum by (stage, le) (rate(fanin_perf_server_ack_seconds_bucket[1m])))
```

Check `up` first; stage histograms appear after traffic. You can connect an existing
Grafana to this Prometheus through a suitable private route. No cluster-wide
monitoring configuration is changed. Export query results via Prometheus's HTTP
API before tearing down; save test config, image digests, node types, resource
budgets, Job logs and timestamps alongside them. Use `kubectl top pods/nodes` if
metrics-server is installed. This first setup does not yet collect PostgreSQL
exporter data, per-container CPU throttling, or GHC eventlogs; capture those before
diagnosing a saturation point.

Start with the previous 1,000-device / 10-events-per-second / five-minute baseline.
Only then increase to 5,000 and 10,000 devices, keeping the other parameters fixed.
Record delivery completeness, achieved publishing rate, latency, backlog and drain
time. This still tests online delivery for one fixed group/epoch, not SNS, offline
populations, group changes, multiple Cannons, recovery or concurrent publishers.
**Do not scale Cannon replicas:** its advertised `cannon` address currently assumes
one instance. RabbitMQ and Redis are ephemeral; restarting them invalidates an
in-flight run. PostgreSQL, Cassandra and Prometheus use PVCs.

## Cleanup and local validation

After saving results, remove only the namespace you created. This deletes its PVCs;
the StorageClass reclaim policy determines whether underlying disks are deleted:

```bash
kubectl --context "$PERF_CONTEXT" delete namespace "$PERF_NAMESPACE"
```

Validate the renderer without accessing a cluster:

```bash
python3 -m unittest discover -s tools/fan-in-perf/kubernetes -p 'test_*.py'
```

Server-side validation (`kubectl --context ... apply --dry-run=server -f ...`) is
also recommended before applying each stage, once the namespace exists. Rendering
and unit tests do not verify cluster capacity, admission policies, CNI enforcement,
image pull access, or service startup; the first cluster smoke run must verify those.
