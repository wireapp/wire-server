"""Run with python3 -m unittest discover -s charts/wire-ingress/tests (Helm + PyYAML)."""

import subprocess
import tempfile
import unittest
from pathlib import Path

import yaml

CHART = Path(__file__).resolve().parents[1]


def render(overrides=None, kube_version="1.36.0"):
    values = {
        "gateway": {
            "className": "envoy",
            "listeners": {"https": {"hostname": "*.example.com"}},
        },
        "config": {
            "dns": {
                "https": "api.example.com",
                "ssl": "ws.example.com",
                "webapp": "web.example.com",
                "fakeS3": "s3.example.com",
                "teamSettings": "teams.example.com",
                "accountPages": "accounts.example.com",
                "federator": "fed.example.com",
            }
        },
        "tls": {"secret": {"create": False}},
        "teamSettings": {"enabled": True},
        "accountPages": {"enabled": True},
    }

    def merge(target, source):
        for key, value in source.items():
            if isinstance(value, dict):
                merge(target.setdefault(key, {}), value)
            else:
                target[key] = value

    merge(values, overrides or {})
    with tempfile.NamedTemporaryFile(mode="w", suffix=".yaml") as f:
        yaml.safe_dump(values, f)
        f.flush()
        output = subprocess.check_output(
            [
                "helm",
                "template",
                "test",
                str(CHART),
                "--kube-version",
                kube_version,
                "-n",
                "test",
                "-f",
                f.name,
            ],
            text=True,
            stderr=subprocess.PIPE,
        )
    return [doc for doc in yaml.safe_load_all(output) if doc]


def resource(docs, kind):
    return next(doc for doc in docs if doc["kind"] == kind)


class ListenerSetRendering(unittest.TestCase):
    def test_default_routes_and_placeholder(self):
        docs = render()
        gateway = resource(docs, "Gateway")["spec"]
        self.assertEqual(gateway["allowedListeners"]["namespaces"], {"from": "Same"})
        placeholder = gateway["listeners"][0]
        self.assertEqual(placeholder["protocol"], "HTTP")
        self.assertEqual(
            placeholder["allowedRoutes"]["namespaces"]["selector"]["matchExpressions"],
            [{"key": "kubernetes.io/metadata.name", "operator": "DoesNotExist"}],
        )
        listeners = resource(docs, "ListenerSet")
        self.assertEqual(
            listeners["spec"]["parentRef"]["name"], "test-wire-ingress-gateway"
        )
        routes = [d for d in docs if d["kind"] == "HTTPRoute"]
        self.assertEqual(len(routes), 6)
        for route in routes:
            self.assertEqual(
                route["spec"]["parentRefs"],
                [
                    {
                        "kind": "ListenerSet",
                        "name": listeners["metadata"]["name"],
                        "namespace": "test",
                        "sectionName": "https",
                    }
                ],
            )

    def test_websocket_policy_inherits_listener_set_settings(self):
        docs = render()
        policy = next(
            d
            for d in docs
            if d["kind"] == "BackendTrafficPolicy"
            and d["metadata"]["name"].endswith("-nginz-websockets")
        )["spec"]
        self.assertEqual(policy["mergeType"], "StrategicMerge")
        self.assertEqual(policy["timeout"]["http"]["streamIdleTimeout"], "0s")
        self.assertNotIn("loadBalancer", policy)
        for kind in ("ClientTrafficPolicy", "BackendTrafficPolicy"):
            spec = resource(docs, kind)["spec"]
            self.assertEqual(spec["targetRefs"][0]["kind"], "ListenerSet")
            self.assertEqual(spec["targetRefs"][0]["name"], "test-wire-ingress")

    def test_external_gateway_still_gets_listeners(self):
        docs = render({"gateway": {"create": False, "name": "external"}})
        self.assertFalse(any(d["kind"] in ("Gateway", "EnvoyProxy") for d in docs))
        self.assertEqual(
            resource(docs, "ListenerSet")["spec"]["parentRef"]["name"], "external"
        )

    def test_federation_and_fips_names(self):
        for federation in (False, True):
            with self.subTest(federation=federation):
                docs = render(
                    {
                        "federator": {
                            "enabled": federation,
                            "tls": {"useCertManager": False},
                        },
                        "FIPS_202205_tls_profile": True,
                        "gateway": {"patchPolicies": {"xdsNameSchemeV2": False}},
                    }
                )
                patch = next(
                    d
                    for d in docs
                    if d["kind"] == "EnvoyPatchPolicy"
                    and d["metadata"]["name"].endswith("-bsi")
                )
                section = "test/test-wire-ingress/https"
                self.assertEqual(
                    patch["spec"]["jsonPatches"][0]["name"],
                    "test/test-wire-ingress-gateway/" + section,
                )
                if federation:
                    self.assertEqual(
                        resource(docs, "Gateway")["spec"]["listeners"][0]["name"],
                        "placeholder",
                    )
                    self.assertIn(
                        "federator",
                        [
                            l["name"]
                            for l in resource(docs, "ListenerSet")["spec"]["listeners"]
                        ],
                    )
                    route = next(
                        d
                        for d in docs
                        if d["kind"] == "HTTPRoute"
                        and d["metadata"]["name"].endswith("-federator")
                    )
                    self.assertEqual(
                        route["spec"]["parentRefs"][0]["kind"], "ListenerSet"
                    )
                    mtls = next(
                        d
                        for d in docs
                        if d["kind"] == "ClientTrafficPolicy"
                        and d["metadata"]["name"].endswith("-federator-mtls")
                    )
                    self.assertEqual(
                        mtls["spec"]["targetRefs"][0],
                        {
                            "kind": "ListenerSet",
                            "group": "gateway.networking.k8s.io",
                            "name": "test-wire-ingress",
                            "sectionName": "federator",
                        },
                    )
                    self.assertEqual(
                        mtls["spec"]["tls"]["clientValidation"]["mode"], "VerifyIfGiven"
                    )
                    fqdn = next(
                        d
                        for d in docs
                        if d["kind"] == "EnvoyPatchPolicy"
                        and d["metadata"]["name"].endswith("-fqdn-domain")
                    )
                    self.assertEqual(
                        fqdn["spec"]["jsonPatches"][0]["name"],
                        "test/test-wire-ingress-gateway/test/test-wire-ingress/federator",
                    )

    def test_multi_domain_and_extra_listeners(self):
        docs = render(
            {
                "config": {
                    "domains": [
                        {
                            "name": "one",
                            "base": "one.example.com",
                            "dns": {
                                "https": "api.one.example.com",
                                "ssl": "ws.one.example.com",
                            },
                        },
                        {
                            "name": "two",
                            "base": "two.example.com",
                            "dns": {
                                "https": "api.two.example.com",
                                "ssl": "ws.two.example.com",
                            },
                            "tls": {"secretName": "two-tls"},
                        },
                    ]
                },
                "gateway": {
                    "extraHttpsListeners": [
                        {"name": "admin", "hostname": "admin.example.com"}
                    ]
                },
                "webapp": {"enabled": False},
                "fakeS3": {"enabled": False},
                "teamSettings": {"enabled": False},
                "accountPages": {"enabled": False},
            }
        )
        listeners = resource(docs, "ListenerSet")["spec"]["listeners"]
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

        policy = next(
            d
            for d in docs
            if d["kind"] == "BackendTrafficPolicy"
            and d["metadata"]["name"].endswith("-nginz-websockets")
        )["spec"]
        self.assertEqual(
            [ref["name"] for ref in policy["targetRefs"]],
            [
                "test-wire-ingress-nginz-websockets",
                "test-wire-ingress-nginz-websockets-two",
            ],
        )

    def test_http01_attaches_to_listener_set(self):
        docs = render(
            {
                "tls": {"useCertManager": True},
                "certManager": {"certmasterEmail": "test@example.com"},
                "gateway": {"listeners": {"http": {"enabled": True}}},
            }
        )
        placeholder = resource(docs, "Gateway")["spec"]["listeners"][0]
        self.assertEqual(placeholder["port"], 65535)
        self.assertIn("allowedRoutes", placeholder)
        self.assertEqual(
            resource(docs, "ListenerSet")["spec"]["listeners"][-1]["name"], "http"
        )
        solver = resource(docs, "Issuer")["spec"]["acme"]["solvers"][0]["http01"]
        self.assertEqual(
            solver["gatewayHTTPRoute"]["parentRefs"][0],
            {
                "kind": "ListenerSet",
                "name": "test-wire-ingress",
                "namespace": "test",
                "group": "gateway.networking.k8s.io",
                "sectionName": "http",
            },
        )

    def test_unsupported_kubernetes_and_reserved_port_are_rejected(self):
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
