#!/usr/bin/env python3
"""Render isolated benchmark Kubernetes resources; never contacts a cluster."""
import argparse
import json
import re


def render(c, stage, run_id="smoke", devices=1000, rate=10, duration=300):
    ns = c["namespace"]
    if (
        not re.fullmatch(r"fan-in-perf-[a-z0-9](?:[-a-z0-9]*[a-z0-9])?", ns)
        or len(ns) > 63
    ):
        raise ValueError("namespace must start with fan-in-perf- and be a DNS label")
    if not re.fullmatch(r"[a-z0-9](?:[-a-z0-9]*[a-z0-9])?", run_id) or len(run_id) > 40:
        raise ValueError("run-id must be a DNS label of at most 40 characters")
    if min(devices, rate, duration) < 1:
        raise ValueError("load parameters must be positive")
    if len(set(c["nodes"].values())) != 3:
        raise ValueError(
            "use three distinct nodes for services, dependencies and generator"
        )
    if any("REPLACE" in x for x in [*c["nodes"].values(), *c["images"].values()]):
        raise ValueError("replace example node names and image references first")
    out = []

    def obj(kind, name, spec=None, **extra):
        api = {
            "Deployment": "apps/v1",
            "Job": "batch/v1",
            "NetworkPolicy": "networking.k8s.io/v1",
        }.get(kind, "v1")
        value = dict(
            apiVersion=api,
            kind=kind,
            metadata=dict(
                name=name,
                namespace=ns,
                labels={"app.kubernetes.io/part-of": "fan-in-perf"},
            ),
            **extra
        )
        if spec is not None:
            value["spec"] = spec
        out.append(value)
        return value

    def secret_env(name, key):
        return dict(
            name=name,
            valueFrom=dict(secretKeyRef=dict(name="benchmark-passwords", key=key)),
        )

    def env(name, value):
        return dict(name=name, value=value)

    def config(name, data):
        obj("ConfigMap", name, data=data)

    def mount_config(pod, container, name, path):
        pod.setdefault("volumes", []).append(dict(name=name, configMap=dict(name=name)))
        container.setdefault("volumeMounts", []).append(
            dict(name=name, mountPath=path, readOnly=True)
        )

    def workload(name, image, role, cpu, memory, args=None, port=None, job=False):
        container = dict(
            name=name,
            image=c["images"][image],
            resources=dict(
                requests=dict(cpu=cpu, memory=memory),
                limits=dict(cpu=cpu, memory=memory),
            ),
        )
        if args:
            container["args"] = args
        pod = dict(
            automountServiceAccountToken=False,
            nodeSelector={"kubernetes.io/hostname": c["nodes"][role]},
            containers=[container],
            restartPolicy="Never" if job else "Always",
        )
        if c.get("imagePullSecrets"):
            pod["imagePullSecrets"] = [dict(name=x) for x in c["imagePullSecrets"]]
        template = dict(
            metadata=dict(
                labels={"app": name, "app.kubernetes.io/part-of": "fan-in-perf"}
            ),
            spec=pod,
        )
        if job:
            obj(
                "Job",
                name,
                dict(
                    backoffLimit=0,
                    activeDeadlineSeconds=max(1800, duration + 900),
                    template=template,
                ),
            )
        else:
            obj(
                "Deployment",
                name,
                dict(
                    replicas=1,
                    strategy=dict(type="Recreate"),
                    selector=dict(matchLabels={"app": name}),
                    template=template,
                ),
            )
        if port:
            container["ports"] = [dict(containerPort=port)]
            container["readinessProbe"] = dict(
                tcpSocket=dict(port=port), periodSeconds=5
            )
            obj(
                "Service",
                name,
                dict(selector={"app": name}, ports=[dict(port=port, targetPort=port)]),
            )
        return pod, container

    def disk(pod, container, name, path, size):
        spec = dict(
            accessModes=["ReadWriteOnce"], resources=dict(requests=dict(storage=size))
        )
        if c.get("storageClass"):
            spec["storageClassName"] = c["storageClass"]
        obj("PersistentVolumeClaim", name, spec)
        pod.setdefault("volumes", []).append(
            dict(name="data", persistentVolumeClaim=dict(claimName=name))
        )
        container.setdefault("volumeMounts", []).append(
            dict(name="data", mountPath=path)
        )

    rabbit_env = [
        secret_env("RABBITMQ_USERNAME", "rabbit-user"),
        secret_env("RABBITMQ_PASSWORD", "rabbit-password"),
    ]
    pg_env = [secret_env("PGPASSWORD", "postgres-password")]
    if stage == "infra":
        obj("Namespace", ns)["metadata"].pop("namespace")
        # No public services, no real AWS egress, and no access from other namespaces.
        obj(
            "NetworkPolicy",
            "benchmark-only",
            dict(podSelector={}, policyTypes=["Ingress", "Egress"]),
        )
        out[-1]["spec"]["ingress"] = [{"from": [{"podSelector": {}}]}]
        out[-1]["spec"]["egress"] = [
            {"to": [{"podSelector": {}}]},
            {
                "to": [
                    {
                        "namespaceSelector": {
                            "matchLabels": {
                                "kubernetes.io/metadata.name": "kube-system"
                            }
                        }
                    }
                ],
                "ports": [{"protocol": p, "port": 53} for p in ["UDP", "TCP"]],
            },
        ]
        p, x = workload(
            "postgres",
            "postgres",
            "dependencies",
            "2",
            "4Gi",
            [
                "postgres",
                "-c",
                "max_connections=200",
                "-c",
                "shared_preload_libraries=pg_stat_statements,pg_partman_bgw",
                "-c",
                "pg_stat_statements.track=all",
            ],
            5432,
        )
        x["env"] = [
            secret_env("POSTGRES_PASSWORD", "postgres-password"),
            env("POSTGRES_USER", "wire-server"),
            env("POSTGRES_DB", "fan_in_perf"),
            env("PGDATA", "/var/lib/postgresql/data/pgdata"),
        ]
        disk(p, x, "postgres", "/var/lib/postgresql/data", "30Gi")
        p, x = workload("cassandra", "cassandra", "dependencies", "1", "3Gi", port=9042)
        x["env"] = [env("MAX_HEAP_SIZE", "1G"), env("HEAP_NEWSIZE", "256M")]
        disk(p, x, "cassandra", "/var/lib/cassandra", "10Gi")
        p, x = workload(
            "rabbitmq", "rabbitmq", "dependencies", "500m", "1Gi", port=5672
        )
        x["env"] = [
            secret_env("RABBITMQ_DEFAULT_USER", "rabbit-user"),
            secret_env("RABBITMQ_DEFAULT_PASS", "rabbit-password"),
        ]
        # RabbitMQ state is intentionally ephemeral; stop services before recreating it.
        workload(
            "redis",
            "redis",
            "dependencies",
            "250m",
            "256Mi",
            ["redis-server", "--save", "", "--appendonly", "no"],
            6379,
        )
        config(
            "elasticmq",
            {
                "elasticmq.conf": """include classpath("application.conf")
node-address { protocol = http, host = elasticmq, port = 4568, context-path = "" }
rest-sqs { enabled = true, bind-port = 4568, bind-hostname = "0.0.0.0" }
queues { integration-gundeck-events {} }
"""
            },
        )
        p, x = workload(
            "elasticmq", "elasticmq", "dependencies", "250m", "256Mi", port=4568
        )
        mount_config(p, x, "elasticmq", "/opt/config")
        x["volumeMounts"][0].update(
            mountPath="/opt/elasticmq.conf", subPath="elasticmq.conf"
        )
        config("prometheus", {})
        out[-1]["data"]["prometheus.yml"] = json.dumps(
            {
                "global": {"scrape_interval": "5s"},
                "scrape_configs": [
                    {
                        "job_name": "cannon",
                        "metrics_path": "/i/metrics",
                        "static_configs": [{"targets": ["cannon:8083"]}],
                    },
                    {
                        "job_name": "gundeck",
                        "metrics_path": "/i/metrics",
                        "static_configs": [{"targets": ["gundeck:8086"]}],
                    },
                ],
            }
        )
        p, x = workload(
            "prometheus",
            "prometheus",
            "dependencies",
            "500m",
            "1Gi",
            [
                "--config.file=/etc/benchmark/prometheus.yml",
                "--storage.tsdb.path=/prometheus",
                "--storage.tsdb.retention.time=7d",
            ],
            9090,
        )
        mount_config(p, x, "prometheus", "/etc/benchmark")
        p["securityContext"] = dict(fsGroup=65534)
        disk(p, x, "prometheus", "/prometheus", "10Gi")
    elif stage == "schema":
        workload(
            "gundeck-schema",
            "gundeck-schema",
            "services",
            "1",
            "1Gi",
            [
                "--host",
                "cassandra",
                "--port",
                "9042",
                "--keyspace",
                "gundeck_perf",
                "--replication-factor",
                "1",
            ],
            job=True,
        )
        p, x = workload(
            "postgres-schema",
            "fan-in-perf",
            "services",
            "1",
            "1Gi",
            [
                "--db",
                "host=postgres port=5432 user=wire-server password=$(PGPASSWORD) dbname=fan_in_perf",
                "reset-db",
                "--partitioned",
            ],
            job=True,
        )
        x["env"] = pg_env
        # Refuse to invoke the destructive reset command against an existing fixture.
        p["initContainers"] = [
            dict(
                name="require-empty-database",
                image=c["images"]["postgres"],
                env=pg_env,
                command=[
                    "sh",
                    "-ec",
                    """n=$(psql -h postgres -U wire-server -d fan_in_perf -Atc "SELECT count(*) FROM pg_tables WHERE schemaname = 'public'"); test "$n" = 0""",
                ],
            )
        ]
    elif stage == "services":
        common = dict(
            fanInPerfPostgresql=dict(
                host="postgres", port="5432", user="wire-server", dbname="fan_in_perf"
            ),
            fanInPerfPostgresqlPassword="/etc/benchmark-passwords/postgres-password",
            fanInPerfPostgresqlPool=dict(
                size=32, acquisitionTimeout="5s", idlenessTimeout="600s"
            ),
            cassandra=dict(
                endpoint=dict(host="cassandra", port=9042), keyspace="gundeck_perf"
            ),
            rabbitmq=dict(host="rabbitmq", port=5672, vHost="/", enableTls=False),
            gundeck=dict(host="gundeck", port=8086),
            logLevel="Warn",
            logNetStrings=False,
        )
        cannon = dict(
            common,
            cannon=dict(host="0.0.0.0", port=8083, externalHost="cannon"),
            drainOpts=dict(
                gracePeriodSeconds=5, millisecondsBetweenBatches=500, minBatchSize=5
            ),
            notificationTTL=24192200,
            disabledAPIVersions=[],
        )
        gundeck = dict(
            common,
            gundeck=dict(host="0.0.0.0", port=8086),
            brig=dict(host="unused-brig", port=8082),
            redis=dict(
                host="redis", port=6379, connectionMode="master", enableTls=False
            ),
            aws=dict(
                queueName="integration-gundeck-events",
                region="eu-central-1",
                account="123456789012",
                arnEnv="integration",
                sqsEndpoint="http://elasticmq:4568",
                snsEndpoint="http://127.0.0.1:9",
            ),
            settings=dict(
                httpPoolSize=1024,
                notificationTTL=24192200,
                perNativePushConcurrency=32,
                sqsThrottleMillis=1000,
                maxConcurrentNativePushes=dict(hard=30, soft=10),
                disabledAPIVersions=[],
                cellsEventQueue="cells_events",
                consumableNotifications=False,
            ),
        )
        for name, cfg, port in [("gundeck", gundeck, 8086), ("cannon", cannon, 8083)]:
            config(name, {"config.yaml": json.dumps(cfg)})
            p, x = workload(
                name,
                name,
                "services",
                "2",
                "2Gi",
                ["-c", "/etc/benchmark/config.yaml", "+RTS", "-N2", "-RTS"],
                port,
            )
            x["env"] = rabbit_env + [
                env("AWS_ACCESS_KEY_ID", "benchmark-only"),
                env("AWS_SECRET_ACCESS_KEY", "benchmark-only"),
                env("AWS_EC2_METADATA_DISABLED", "true"),
            ]
            mount_config(p, x, name, "/etc/benchmark")
            p.setdefault("volumes", []).append(
                dict(
                    name="passwords",
                    secret=dict(
                        secretName="benchmark-passwords",
                        items=[dict(key="postgres-password", path="postgres-password")],
                    ),
                )
            )
            x["volumeMounts"].append(
                dict(
                    name="passwords",
                    mountPath="/etc/benchmark-passwords",
                    readOnly=True,
                )
            )
    else:
        args = [
            stage,
            "--gundeck-url",
            "http://gundeck:8086",
            "--cannon-url",
            "ws://cannon:8083",
        ]
        if stage == "load":
            args += [
                "--devices",
                str(devices),
                "--rate",
                str(rate),
                "--duration",
                str(duration),
                "--drain-seconds",
                "60",
            ]
        args += ["+RTS", "-N2", "-RTS"]
        workload(
            stage + "-" + run_id, "fan-in-perf", "generator", "2", "2Gi", args, job=True
        )
    return dict(apiVersion="v1", kind="List", items=out)


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("config")
    parser.add_argument(
        "stage", choices=["infra", "schema", "services", "smoke", "load"]
    )
    parser.add_argument("--run-id", default="baseline")
    for name, default in [("devices", 1000), ("rate", 10), ("duration", 300)]:
        parser.add_argument("--" + name, type=int, default=default)
    args = parser.parse_args()
    with open(args.config) as f:
        conf = json.load(f)
    print(
        json.dumps(
            render(
                conf, args.stage, args.run_id, args.devices, args.rate, args.duration
            ),
            indent=2,
        )
    )
