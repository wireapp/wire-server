#!/usr/bin/env python3
"""Offline regressions for companion charts in the sibling Cailleach checkout."""
import pathlib
import subprocess
import unittest

import yaml

CHARTS = pathlib.Path(__file__).resolve().parents[3] / "cailleach.tr-conformance/helm_charts"


def render(chart, values, success=True):
    result = subprocess.run(["helm", "template", "test", str(CHARTS / chart), "-n", "test", "-f", "-"],
                            input=yaml.safe_dump(values), capture_output=True, text=True)
    if not success:
        assert result.returncode, "unsafe settings rendered successfully"
        return result.stderr
    if result.returncode:
        raise AssertionError(result.stderr)
    return [doc for doc in yaml.safe_load_all(result.stdout) if doc]


class CompanionCharts(unittest.TestCase):
    def test_sft_bsi(self):
        docs = render("envoy-tls-gateway", {
            "gatewayName": "sft", "hostname": "sft.example.com", "certificateSecretName": "sft",
            "BSI_TR_02102_2_conformance": True,
        })
        policy = next(d for d in docs if d["kind"] == "ClientTrafficPolicy")
        self.assertEqual(policy["spec"]["tls"]["maxVersion"], "1.2")
        patch = next(d for d in docs if d["kind"] == "EnvoyPatchPolicy")
        self.assertIn("filter_chains[*]", patch["spec"]["jsonPatches"][0]["operation"]["jsonPath"])

    def test_sft_pq(self):
        docs = render("envoy-tls-gateway", {
            "gatewayName": "sft", "hostname": "sft.example.com", "certificateSecretName": "sft",
            "ecdhCurves": ["X25519MLKEM768", "X25519"],
        })
        self.assertNotIn("EnvoyPatchPolicy", [d["kind"] for d in docs])
        tls = next(d for d in docs if d["kind"] == "ClientTrafficPolicy")["spec"]["tls"]
        self.assertEqual(tls["minVersion"], "1.3")
        self.assertIn("X25519MLKEM768", tls["ecdhCurves"])

    def test_mixed_profiles_rejected(self):
        self.assertIn("allowlist", render("envoy-tls-gateway", {
            "gatewayName": "sft", "BSI_TR_02102_2_conformance": True,
            "ecdhCurves": ["X25519MLKEM768"],
        }, success=False))

    def test_admin_certificate_and_auth(self):
        docs = render("wire-admin-gateway", {
            "certificate": {"issuerName": "letsencrypt-bsi"},
            "backoffice": {"enabled": True, "hostname": "backoffice.ops.example.com",
                           "oauthHost": "oauth2.ops.example.com", "allowedGroupsEncoded": "Platform+team"},
            "basicAuth": {"enabled": True, "htpasswd": "synthetic-fixture-only",
                          "routes": [{"name": "inbucket", "hostname": "inbucket.example.com",
                                      "serviceName": "inbucket", "servicePort": 9000,
                                      "realm": "Authentication Required - inbucket"}]},
        })
        cert = next(d for d in docs if d["kind"] == "Certificate")["spec"]
        self.assertEqual(cert["dnsNames"], ["backoffice.ops.example.com"])
        self.assertEqual(cert["privateKey"]["algorithm"], "ECDSA")
        config = next(d for d in docs if d["kind"] == "ConfigMap")["data"]["nginx.conf"]
        for setting in ["proxy_ssl_verify on;", "auth_request /_wire_auth;", "internal;",
                        'auth_basic "Authentication Required - inbucket";',
                        "allowed_groups=Platform+team", "$escaped_request_uri"]:
            self.assertIn(setting, config)
        self.assertNotIn("synthetic-fixture-only", config)
        self.assertEqual(len([d for d in docs if d["kind"] == "HTTPRoute"]), 2)
        self.assertIn("NetworkPolicy", [d["kind"] for d in docs])

    def test_admin_config_injection_rejected(self):
        self.assertIn("must be valid", render("wire-admin-gateway", {
            "certificate": {"enabled": False},
            "backoffice": {"enabled": True, "hostname": "bad;host",
                           "oauthHost": "oauth2.example.com", "allowedGroupsEncoded": "test"},
        }, success=False))

    def test_inbucket_legacy_default_and_envoy_mode(self):
        values = {"issuerRef": {"kind": "ClusterIssuer", "name": "letsencrypt"}}
        self.assertIn("Ingress", [d["kind"] for d in render("inbucket", values)])
        docs = render("inbucket", {**values, "ingress": {"enabled": False}})
        self.assertNotIn("Ingress", [d["kind"] for d in docs])
        self.assertNotIn("Certificate", [d["kind"] for d in docs])
        service = next(d for d in docs if d["kind"] == "Service")
        self.assertIn(9000, [p["port"] for p in service["spec"]["ports"]])


if __name__ == "__main__":
    unittest.main()
