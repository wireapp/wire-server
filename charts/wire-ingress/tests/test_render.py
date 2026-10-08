"""Run: python3 -m unittest discover -s charts/wire-ingress/tests (Helm + PyYAML)."""

import subprocess
import tempfile
import unittest
from pathlib import Path

import yaml

CHART = Path(__file__).resolve().parents[1]
NAME = "test-wire-ingress"
XDS_PATH = f"test/{NAME}-gateway/test/{NAME}"
VALUES = """
gateway:
  className: envoy
  listeners:
    https: {hostname: '*.example.com'}
tls:
  secret: {create: false}
teamSettings: {enabled: true}
accountPages: {enabled: true}
config:
  dns:
    https: api.example.com
    ssl: ws.example.com
    webapp: web.example.com
    fakeS3: s3.example.com
    teamSettings: teams.example.com
    accountPages: accounts.example.com
    federator: fed.example.com
"""


def render(overrides=None, kube_version="1.36.0"):
    with tempfile.NamedTemporaryFile(mode="w") as values:
        values.write(VALUES)
        values.flush()
        command = ["helm", "template", "test", str(CHART), "--namespace=test"]
        command += [f"--kube-version={kube_version}", "-f", values.name, "-f", "-"]
        result = subprocess.run(
            command,
            input=yaml.safe_dump(overrides or {}),
            text=True,
            capture_output=True,
            check=True,
        )
    return [doc for doc in yaml.safe_load_all(result.stdout) if doc]


def spec(docs, kind, suffix=""):
    return next(
        doc["spec"]
        for doc in docs
        if doc["kind"] == kind and doc["metadata"]["name"].endswith(suffix)
    )


class ListenerSetRendering(unittest.TestCase):
    def test_default_routes_and_placeholder(self):
        docs = render()
        gateway = spec(docs, "Gateway")
        self.assertEqual(gateway["allowedListeners"]["namespaces"], {"from": "Same"})
        placeholder = gateway["listeners"][0]
        self.assertEqual(placeholder["protocol"], "HTTP")
        self.assertEqual(
            placeholder["allowedRoutes"]["namespaces"]["selector"]["matchExpressions"],
            [{"key": "kubernetes.io/metadata.name", "operator": "DoesNotExist"}],
        )
        self.assertEqual(
            spec(docs, "ListenerSet")["parentRef"]["name"], f"{NAME}-gateway"
        )
        routes = [doc["spec"] for doc in docs if doc["kind"] == "HTTPRoute"]
        self.assertEqual(len(routes), 6)
        parent = dict(
            kind="ListenerSet", name=NAME, namespace="test", sectionName="https"
        )
        for route in routes:
            self.assertEqual(route["parentRefs"], [parent])

    def test_websocket_policy_inherits_listener_set_settings(self):
        docs = render()
        policy = spec(docs, "BackendTrafficPolicy", "-nginz-websockets")
        self.assertEqual(policy["mergeType"], "StrategicMerge")
        self.assertEqual(policy["timeout"]["http"]["streamIdleTimeout"], "0s")
        self.assertNotIn("loadBalancer", policy)
        for kind in ("ClientTrafficPolicy", "BackendTrafficPolicy"):
            target = spec(docs, kind)["targetRefs"][0]
            self.assertEqual((target["kind"], target["name"]), ("ListenerSet", NAME))

    def test_external_gateway(self):
        docs = render({"gateway": {"create": False, "name": "external"}})
        self.assertFalse(any(doc["kind"] in ("Gateway", "EnvoyProxy") for doc in docs))
        self.assertEqual(spec(docs, "ListenerSet")["parentRef"]["name"], "external")

    def test_federation_and_fips(self):
        for enabled in (False, True):
            with self.subTest(federation=enabled):
                docs = render(
                    {
                        "federator": {
                            "enabled": enabled,
                            "tls": {"useCertManager": False},
                        },
                        "FIPS_202205_tls_profile": True,
                        "gateway": {"patchPolicies": {"xdsNameSchemeV2": False}},
                    }
                )
                patch = spec(docs, "EnvoyPatchPolicy", "-bsi")
                self.assertEqual(patch["jsonPatches"][0]["name"], f"{XDS_PATH}/https")
                if not enabled:
                    continue
                self.assertEqual(
                    spec(docs, "Gateway")["listeners"][0]["name"], "placeholder"
                )
                self.assertIn(
                    "federator",
                    [l["name"] for l in spec(docs, "ListenerSet")["listeners"]],
                )
                route = spec(docs, "HTTPRoute", "-federator")
                self.assertEqual(route["parentRefs"][0]["kind"], "ListenerSet")
                mtls = spec(docs, "ClientTrafficPolicy", "-federator-mtls")
                self.assertEqual(
                    mtls["targetRefs"],
                    [
                        dict(
                            kind="ListenerSet",
                            group="gateway.networking.k8s.io",
                            name=NAME,
                            sectionName="federator",
                        )
                    ],
                )
                self.assertEqual(
                    mtls["tls"]["clientValidation"]["mode"], "VerifyIfGiven"
                )
                fqdn = spec(docs, "EnvoyPatchPolicy", "-fqdn-domain")
                self.assertEqual(
                    fqdn["jsonPatches"][0]["name"], f"{XDS_PATH}/federator"
                )

    def test_multi_domain_and_extra_listeners(self):
        domains = [
            dict(
                name=name,
                base=f"{name}.example.com",
                dns=dict(https=f"api.{name}.example.com", ssl=f"ws.{name}.example.com"),
                tls=dict(secretName=f"{name}-tls"),
            )
            for name in ("one", "two")
        ]
        docs = render(
            {
                "config": {"domains": domains},
                "gateway": {
                    "extraHttpsListeners": [
                        {"name": "admin", "hostname": "admin.example.com"}
                    ]
                },
                **{
                    app: {"enabled": False}
                    for app in ("webapp", "fakeS3", "teamSettings", "accountPages")
                },
            }
        )
        listeners = spec(docs, "ListenerSet")["listeners"]
        self.assertEqual(
            [l["name"] for l in listeners], ["https", "https-two", "admin"]
        )
        self.assertEqual(listeners[1]["tls"]["certificateRefs"][0]["name"], "two-tls")
        self.assertEqual(
            [
                d["spec"]["parentRefs"][0]["sectionName"]
                for d in docs
                if d["kind"] == "HTTPRoute"
            ],
            ["https", "https-two", "https", "https-two"],
        )
        policy = spec(docs, "BackendTrafficPolicy", "-nginz-websockets")
        self.assertEqual(
            [ref["name"] for ref in policy["targetRefs"]],
            [f"{NAME}-nginz-websockets", f"{NAME}-nginz-websockets-two"],
        )

    def test_http01(self):
        docs = render(
            {
                "tls": {"useCertManager": True},
                "certManager": {"certmasterEmail": "test@example.com"},
                "gateway": {"listeners": {"http": {"enabled": True}}},
            }
        )
        placeholder = spec(docs, "Gateway")["listeners"][0]
        self.assertEqual(placeholder["port"], 65535)
        self.assertIn("allowedRoutes", placeholder)
        self.assertEqual(spec(docs, "ListenerSet")["listeners"][-1]["name"], "http")
        solver = spec(docs, "Issuer")["acme"]["solvers"][0]["http01"]
        self.assertEqual(
            solver["gatewayHTTPRoute"]["parentRefs"],
            [
                dict(
                    kind="ListenerSet",
                    name=NAME,
                    namespace="test",
                    group="gateway.networking.k8s.io",
                    sectionName="http",
                )
            ],
        )

    def test_unsupported_kubernetes_and_reserved_port(self):
        with self.assertRaises(subprocess.CalledProcessError) as error:
            render(kube_version="1.32.0")
        self.assertIn("kubeVersion", error.exception.stderr)
        for listener in ("https", "http"):
            with self.subTest(listener=listener):
                with self.assertRaises(subprocess.CalledProcessError) as error:
                    render(
                        {
                            "gateway": {
                                "listeners": {
                                    listener: {"enabled": True, "port": 65535}
                                }
                            }
                        }
                    )
                self.assertIn("65535 is reserved", error.exception.stderr)
