#!/usr/bin/env python3
"""Exercise the real wire-ingress chart without a Wire or database deployment."""

import argparse
import contextlib
import json
import os
from pathlib import Path
import re
import subprocess
import tempfile
import time
from urllib.parse import unquote


ROOT = Path(__file__).resolve().parents[2]
CHART = ROOT / "charts/wire-ingress"
HOST = "api.ingress.example.test"
FEDERATION = "federation.example.test"
WEBAPP = "webapp.ingress.example.test"
SECONDARY_API = "api.other.example.test"
SECONDARY_WEBAPP = "webapp.other.example.test"
EXTRA = "extra.example.test"
COMPANION = "companion.example.test"
BODY = "wire-ingress-regression-ok"
GATEWAY = "regression"
LISTENERSET = "regression-wire-ingress"
AES128 = "ECDHE-ECDSA-AES128-GCM-SHA256"
AES256 = "ECDHE-ECDSA-AES256-GCM-SHA384"


def run(*args, input=None, check=True, timeout=60):
    result = subprocess.run(
        args, input=input, text=True, capture_output=True, timeout=timeout
    )
    if check and result.returncode:
        raise RuntimeError(
            f"{' '.join(map(str, args))}:\n{result.stdout}{result.stderr}"
        )
    return result


def resource(kind, name, spec, namespace, api="gateway.networking.k8s.io/v1"):
    return dict(
        apiVersion=api,
        kind=kind,
        metadata=dict(name=name, namespace=namespace),
        spec=spec,
    )


def values(namespace, external, profile, gateway_class, v2, multi=False):
    result = {
        "gateway": {
            "name": GATEWAY,
            "className": gateway_class,
            "create": not external,
            "serviceType": "ClusterIP",
            "envoyProxy": {"replicas": 1},
            "listeners": {"https": {"hostname": "*.ingress.example.test"}},
            "allowedListenerNamespaces": [namespace + "-companion"],
            "extraHttpsListeners": [{"name": "extra", "hostname": EXTRA}],
            "patchPolicies": {"xdsNameSchemeV2": v2},
        },
        "config": {"dns": {"https": HOST, "federator": FEDERATION, "webapp": WEBAPP}},
        "tls": {"secret": {"create": False, "nameOverride": "server-tls"}},
        "federator": {
            "enabled": True,
            "tls": {"useCertManager": False, "secretName": "server-tls"},
        },
        "webapp": {"enabled": True},
        "websockets": {"enabled": False},
        "fakeS3": {"enabled": False},
    }
    if profile == "tls12":
        result["gateway"]["tls"] = {
            "minVersion": "1.2",
            "maxVersion": "1.2",
            "ciphers": [AES128],
            "ecdhCurves": ["P-256"],
        }
    if profile == "fips":
        result["FIPS_202205_tls_profile"] = True
    if multi:
        result["federator"]["enabled"] = False
        result["config"] = {
            "domains": [
                {
                    "name": "primary",
                    "base": "ingress.example.test",
                    "dns": {"https": HOST, "webapp": WEBAPP},
                },
                {
                    "name": "secondary",
                    "base": "other.example.test",
                    "sectionName": "secondary-https",
                    "dns": {"https": SECONDARY_API, "webapp": SECONDARY_WEBAPP},
                    "tls": {"secretName": "secondary-tls"},
                },
            ]
        }
    return result


def assert_probe(result, accepted, protocol=None, cipher=None):
    output = result.stdout + result.stderr
    if accepted:
        assert result.returncode == 0, output
        assert "CONNECTION ESTABLISHED" in output, output
        assert "Verification: OK" in output, output
        assert "HTTP/1.1 200" in output and BODY in output, output
        if protocol:
            assert f"Protocol version: {protocol}" in output, output
        if cipher:
            assert f"Ciphersuite: {cipher}" in output, output
    else:
        # A timeout, refused connection, or local OpenSSL error is NOT a TLS rejection.
        assert result.returncode != 0 and "CONNECTION ESTABLISHED" not in output, output
        assert re.search(
            r"alert (handshake failure|protocol version|insufficient security)", output
        ), output


class Suite:
    def __init__(self, args, directory):
        self.args = args
        self.directory = Path(directory)
        self.namespaces = []

    def kubectl(self, *args, **kwargs):
        return run("kubectl", "--request-timeout=20s", *args, **kwargs)

    def apply(self, obj):
        self.kubectl("apply", "-f", "-", input=json.dumps(obj))

    def create_namespace(self, namespace):
        # Refuse existing namespaces: cleanup must never delete somebody else's work.
        self.kubectl("create", "namespace", namespace)
        self.namespaces.append(namespace)

    def certificate(self, secondary=False):
        prefix = "secondary-" if secondary else ""
        hosts = (
            [SECONDARY_API, SECONDARY_WEBAPP]
            if secondary
            else [HOST, EXTRA, COMPANION, FEDERATION, WEBAPP]
        )
        run(
            "openssl",
            "req",
            "-x509",
            "-newkey",
            "ec",
            "-pkeyopt",
            "ec_paramgen_curve:P-256",
            "-nodes",
            "-days",
            "1",
            "-subj",
            "/CN=wire-ingress-regression",
            "-addext",
            "subjectAltName=" + ",".join("DNS:" + host for host in hosts),
            "-keyout",
            str(self.directory / (prefix + "key.pem")),
            "-out",
            str(self.directory / (prefix + "cert.pem")),
        )

    def secret(self, namespace, secondary=False):
        prefix = "secondary-" if secondary else ""
        self.kubectl(
            "-n",
            namespace,
            "create",
            "secret",
            "tls",
            "secondary-tls" if secondary else "server-tls",
            "--cert",
            str(self.directory / (prefix + "cert.pem")),
            "--key",
            str(self.directory / (prefix + "key.pem")),
        )

    def backend(self, namespace):
        self.kubectl("-n", namespace, "apply", "-f", str(CHART / "tests/backend.yaml"))
        self.kubectl(
            "-n",
            namespace,
            "rollout",
            "status",
            "deployment",
            "--timeout=180s",
            timeout=190,
        )

    def route(self, namespace, name, listener, section, host):
        self.apply(
            resource(
                "HTTPRoute",
                name,
                {
                    "parentRefs": [
                        {
                            "name": listener,
                            "kind": "ListenerSet",
                            "sectionName": section,
                        }
                    ],
                    "hostnames": [host],
                    "rules": [{"backendRefs": [{"name": "nginz", "port": 8080}]}],
                },
                namespace,
            )
        )

    def wait(self, namespace, resource_name, condition):
        self.kubectl(
            "-n",
            namespace,
            "wait",
            resource_name,
            f"--for=condition={condition}",
            "--timeout=180s",
            timeout=190,
        )

    @contextlib.contextmanager
    def forward(self, namespace):
        selector = f"gateway.envoyproxy.io/owning-gateway-namespace={namespace},gateway.envoyproxy.io/owning-gateway-name={GATEWAY}"
        data = json.loads(
            self.kubectl(
                "-n",
                self.args.controller_namespace,
                "get",
                "service",
                "-l",
                selector,
                "-o",
                "json",
            ).stdout
        )
        assert len(data["items"]) == 1, data
        name = data["items"][0]["metadata"]["name"]
        with tempfile.TemporaryFile(mode="w+") as log:
            process = subprocess.Popen(
                [
                    "kubectl",
                    "-n",
                    self.args.controller_namespace,
                    "port-forward",
                    "service/" + name,
                    ":443",
                    "--address=127.0.0.1",
                ],
                stdout=log,
                stderr=log,
                text=True,
            )
            try:
                deadline = time.monotonic() + 30
                while time.monotonic() < deadline:
                    log.seek(0)
                    output = log.read()
                    match = re.search(r"Forwarding from 127.0.0.1:(\d+)", output)
                    if match:
                        yield int(match[1])
                        return
                    if process.poll() is not None:
                        raise RuntimeError(output)
                    time.sleep(0.2)
                raise RuntimeError("Timed out starting port-forward: " + output)
            finally:
                process.terminate()
                process.wait(timeout=10)

    def probe(
        self,
        port,
        host,
        accepted,
        flags,
        protocol=None,
        cipher=None,
        retry=False,
        client_cert=False,
        authority=None,
    ):
        if client_cert:
            flags = flags + [
                "-cert",
                str(self.directory / "cert.pem"),
                "-key",
                str(self.directory / "key.pem"),
            ]
        deadline = time.monotonic() + (30 if retry else 0)
        while True:
            result = run(
                "openssl",
                "s_client",
                "-connect",
                f"127.0.0.1:{port}",
                "-servername",
                host,
                "-verify_hostname",
                host,
                "-verify_return_error",
                "-CAfile",
                str(
                    self.directory
                    / (
                        "secondary-cert.pem"
                        if host in [SECONDARY_API, SECONDARY_WEBAPP]
                        else "cert.pem"
                    )
                ),
                "-brief",
                "-ign_eof",
                "-ignore_unexpected_eof",
                *flags,
                input=f"GET / HTTP/1.1\r\nHost: {authority or host}\r\nX-SSL-Certificate: spoofed\r\nConnection: close\r\n\r\n",
                check=False,
                timeout=10,
            )
            try:
                assert_probe(result, accepted, protocol, cipher)
                if accepted:
                    body = json.loads(
                        result.stdout.split(
                            "\r\n\r\n" if "\r\n\r\n" in result.stdout else "\n\n", 1
                        )[1]
                    )
                    expected = (
                        "federation"
                        if host == FEDERATION
                        else "webapp" if host in [WEBAPP, SECONDARY_WEBAPP] else "api"
                    )
                    assert body["upstream"] == expected, body
                    if host == FEDERATION:
                        certificate = unquote(body["certificate"])
                        assert certificate == (
                            self.directory.joinpath("cert.pem").read_text()
                            if client_cert
                            else ""
                        ), body
                return
            except AssertionError:
                if time.monotonic() >= deadline:
                    raise
                time.sleep(1)

    def scenario(self, external, profile, multi=False):
        name = f"{'external' if external else 'managed'}-{profile}"
        if multi:
            name += "-multi"
        namespace = f"{self.args.namespace}-{name}"
        companion = namespace + "-companion"
        print(f"RUN {name}", flush=True)
        self.create_namespace(namespace)
        self.secret(namespace)
        if multi:
            self.secret(namespace, secondary=True)
        self.kubectl(
            "-n",
            namespace,
            "create",
            "configmap",
            "federator-ca",
            "--from-file=ca.crt=" + str(self.directory / "cert.pem"),
        )
        self.backend(namespace)
        config = values(
            namespace,
            external,
            profile,
            self.args.gateway_class,
            self.args.xds_name_scheme_v2,
            multi,
        )
        if external:
            # Install a genuinely external Gateway and EnvoyProxy before the chart.
            self.apply(
                resource(
                    "EnvoyProxy",
                    GATEWAY,
                    {
                        "provider": {
                            "type": "Kubernetes",
                            "kubernetes": {
                                "envoyService": {"type": "ClusterIP"},
                                "envoyDeployment": {"replicas": 1},
                            },
                        }
                    },
                    namespace,
                    "gateway.envoyproxy.io/v1alpha1",
                )
            )
            self.apply(
                resource(
                    "Gateway",
                    GATEWAY,
                    {
                        "gatewayClassName": self.args.gateway_class,
                        "infrastructure": {
                            "parametersRef": {
                                "group": "gateway.envoyproxy.io",
                                "kind": "EnvoyProxy",
                                "name": GATEWAY,
                            }
                        },
                        "allowedListeners": {
                            "namespaces": {
                                "from": "Selector",
                                "selector": {
                                    "matchExpressions": [
                                        {
                                            "key": "kubernetes.io/metadata.name",
                                            "operator": "In",
                                            "values": [namespace, companion],
                                        }
                                    ],
                                },
                            }
                        },
                        "listeners": [
                            {
                                "name": "placeholder",
                                "port": 65535,
                                "protocol": "HTTP",
                                "allowedRoutes": {
                                    "namespaces": {
                                        "from": "Selector",
                                        "selector": {
                                            "matchExpressions": [
                                                {
                                                    "key": "kubernetes.io/metadata.name",
                                                    "operator": "DoesNotExist",
                                                }
                                            ],
                                        },
                                    }
                                },
                            }
                        ],
                    },
                    namespace,
                )
            )
        path = self.directory / "values.json"
        path.write_text(json.dumps(config))
        run(
            "helm",
            "install",
            "regression",
            str(CHART),
            "-n",
            namespace,
            "-f",
            str(path),
            timeout=120,
        )
        self.route(namespace, "extra", LISTENERSET, "extra", EXTRA)
        self.wait(namespace, "gateway/" + GATEWAY, "Programmed")
        self.wait(namespace, "listenerset/" + LISTENERSET, "Programmed")
        hosts = (
            [HOST, WEBAPP, SECONDARY_API, SECONDARY_WEBAPP, EXTRA]
            if multi
            else [HOST, WEBAPP, FEDERATION, EXTRA]
        )
        if profile == "fips":
            # Deliberately no ClientTrafficPolicy: the Gateway-wide patch must cover this chain.
            self.create_namespace(companion)
            self.secret(companion)
            self.backend(companion)
            self.apply(
                resource(
                    "ListenerSet",
                    "companion",
                    {
                        "parentRef": {
                            "name": GATEWAY,
                            "namespace": namespace,
                            "kind": "Gateway",
                            "group": "gateway.networking.k8s.io",
                        },
                        "listeners": [
                            {
                                "name": "companion",
                                "hostname": COMPANION,
                                "port": 443,
                                "protocol": "HTTPS",
                                "tls": {
                                    "mode": "Terminate",
                                    "certificateRefs": [{"name": "server-tls"}],
                                },
                            }
                        ],
                    },
                    companion,
                )
            )
            self.route(companion, "companion", "companion", "companion", COMPANION)
            self.wait(companion, "listenerset/companion", "Programmed")
            hosts.append(COMPANION)
        with self.forward(namespace) as port:
            for host in hosts:
                if profile == "tls12":
                    self.probe(
                        port,
                        host,
                        True,
                        ["-tls1_2", "-cipher", AES128],
                        "TLSv1.2",
                        AES128,
                        retry=True,
                    )
                    self.probe(port, host, False, ["-tls1_2", "-cipher", AES256])
                    self.probe(port, host, False, ["-tls1_3"])
                else:
                    self.probe(
                        port,
                        host,
                        True,
                        [
                            "-tls1_3",
                            "-groups",
                            "P-256",
                            "-ciphersuites",
                            "TLS_AES_128_GCM_SHA256",
                        ],
                        "TLSv1.3",
                        "TLS_AES_128_GCM_SHA256",
                        retry=True,
                    )
                    if profile == "default":
                        self.probe(port, host, False, ["-tls1_2", "-cipher", AES128])
                    else:
                        self.probe(
                            port,
                            host,
                            True,
                            [
                                "-tls1_3",
                                "-groups",
                                "P-384",
                                "-ciphersuites",
                                "TLS_AES_128_GCM_SHA256",
                            ],
                            "TLSv1.3",
                            "TLS_AES_128_GCM_SHA256",
                        )
                        for cipher in [AES128, AES256]:
                            self.probe(
                                port,
                                host,
                                True,
                                ["-tls1_2", "-groups", "P-256", "-cipher", cipher],
                                "TLSv1.2",
                                cipher,
                            )
                        self.probe(
                            port,
                            host,
                            False,
                            [
                                "-tls1_3",
                                "-groups",
                                "P-256",
                                "-ciphersuites",
                                "TLS_CHACHA20_POLY1305_SHA256",
                            ],
                        )
                        self.probe(port, host, False, ["-tls1_3", "-groups", "X25519"])
                if host == FEDERATION:
                    flags = (
                        ["-tls1_2", "-cipher", AES128]
                        if profile == "tls12"
                        else ["-tls1_3", "-groups", "P-256"]
                    )
                    self.probe(
                        port, host, True, flags, client_cert=True, authority=host + "."
                    )
                print(f"  PASS {host}: {profile}", flush=True)
        print(f"PASS {name}", flush=True)

    def cleanup(self):
        for namespace in reversed(self.namespaces):
            result = self.kubectl(
                "delete", "namespace", namespace, "--wait=false", check=False
            )
            if result.returncode:
                print(f"Cleanup failed for {namespace}: {result.stderr}", flush=True)

    def diagnostics(self):
        for namespace in self.namespaces:
            for kind in [
                "gateway",
                "listenerset",
                "httproute",
                "clienttrafficpolicy",
                "envoypatchpolicy",
                "pods",
                "events",
            ]:
                result = self.kubectl(
                    "-n", namespace, "get", kind, "-o", "yaml", check=False
                )
                print(result.stdout + result.stderr, flush=True)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--namespace",
        default=os.environ.get("WIRE_INGRESS_TEST_NAMESPACE", "test-wire-ingress"),
    )
    parser.add_argument(
        "--gateway-class", default=os.environ.get("WIRE_INGRESS_GATEWAY_CLASS", "envoy")
    )
    parser.add_argument(
        "--controller-namespace",
        default=os.environ.get("ENVOY_GATEWAY_NAMESPACE", "envoy-gateway-system"),
    )
    parser.add_argument(
        "--xds-name-scheme-v2",
        action="store_true",
        default=os.environ.get("WIRE_INGRESS_XDS_NAME_SCHEME_V2", "false") == "true",
    )
    parser.add_argument(
        "--profile", choices=["default", "tls12", "fips"], action="append"
    )
    args = parser.parse_args()
    if not __debug__:
        parser.error("Run without Python optimization: assertions are required")
    if os.environ.get("WIRE_INGRESS_XDS_NAME_SCHEME_V2", "false") not in {
        "true",
        "false",
    }:
        parser.error("WIRE_INGRESS_XDS_NAME_SCHEME_V2 must be true or false")
    if not re.fullmatch(r"[a-z0-9](?:[-a-z0-9]{0,26}[a-z0-9])?", args.namespace):
        parser.error("--namespace must be a DNS label of at most 28 characters")
    with tempfile.TemporaryDirectory(prefix="wire-ingress-test-") as directory:
        suite = Suite(args, directory)
        try:
            suite.kubectl("get", "gatewayclass", args.gateway_class)
            suite.certificate()
            suite.certificate(secondary=True)
            for external in [False, True]:
                for profile in args.profile or ["default", "tls12", "fips"]:
                    for multi in [False, True]:
                        try:
                            suite.scenario(external, profile, multi)
                        except Exception:
                            suite.diagnostics()
                            raise
                        finally:
                            # Bound proxy resource usage as the matrix grows.
                            suite.cleanup()
                            suite.namespaces.clear()
        except Exception:
            suite.diagnostics()
            raise
        finally:
            suite.cleanup()


if __name__ == "__main__":
    main()
