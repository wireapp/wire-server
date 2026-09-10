#!/usr/bin/env python3
"""Offline Helm profile regressions. Requires helm and PyYAML; no cluster writes."""
import pathlib
import subprocess
import unittest

import yaml

ROOT = pathlib.Path(__file__).resolve().parents[2]
CHART = ROOT / "charts/wire-ingress"
BASE = [
    "--set", "gateway.className=envoy",
    "--set", "gateway.listeners.https.hostname=*.example.com",
    "--set", "config.dns.https=api.example.com",
    "--set", "tls.useCertManager=true",
    "--set", "tls.createIssuer=false",
    "--set", "tls.issuer.name=example",
]


def render(*settings, success=True):
    args = ["helm", "template", "test", str(CHART), "-n", "test", *BASE]
    for value in settings:
        args += ["--set", value]
    result = subprocess.run(args, text=True, capture_output=True, check=False)
    if not success:
        assert result.returncode != 0, "unsafe values unexpectedly rendered"
        return result.stderr
    if result.returncode:
        raise AssertionError(result.stderr)
    return [doc for doc in yaml.safe_load_all(result.stdout) if doc]


def resources(docs, kind):
    return [doc for doc in docs if doc["kind"] == kind]


class ChartProfiles(unittest.TestCase):
    def test_default_has_no_bsi_patch(self):
        docs = render()
        self.assertFalse(resources(docs, "EnvoyPatchPolicy"))
        tls = resources(docs, "ClientTrafficPolicy")[0]["spec"]["tls"]
        self.assertEqual(tls["maxVersion"], "1.3")
        self.assertIn("X25519", tls["ecdhCurves"])
        self.assertNotIn("X25519MLKEM768", tls["ecdhCurves"])

    def test_bsi_overrides_user_tls_settings(self):
        docs = render("BSI_TR_02102_2_conformance=true",
                      "gateway.tls.minVersion=1.3", "gateway.tls.maxVersion=1.3")
        tls = resources(docs, "ClientTrafficPolicy")[0]["spec"]["tls"]
        self.assertEqual((tls["minVersion"], tls["maxVersion"]), ("1.2", "1.2"))
        self.assertEqual(tls["ecdhCurves"], ["P-256", "P-384"])
        self.assertEqual(len(tls["ciphers"]), 4)
        patch = resources(docs, "EnvoyPatchPolicy")[0]["spec"]["jsonPatches"][0]
        self.assertIn("filter_chains[*]", patch["operation"]["jsonPath"])
        self.assertEqual(patch["operation"]["value"], ["FIPS_202205"])

    def test_unsafe_bsi_combinations_fail(self):
        for setting in ["gateway.create=false", "gateway.envoyProxy.create=false",
                        "gateway.patchPolicies.enabled=false",
                        "gateway.patchPolicies.targetGatewayClass=true",
                        "gateway.envoyProxy.spec.mergeGateways=true",
                        "gateway.tls.enabled=false"]:
            with self.subTest(setting=setting):
                self.assertIn("BSI_TR_02102_2_conformance",
                              render("BSI_TR_02102_2_conformance=true", setting, success=False))

    def test_federator_repeats_safe_profile(self):
        docs = render("BSI_TR_02102_2_conformance=true", "federator.enabled=true",
                      "config.dns.federator=federator.example.com")
        policies = resources(docs, "ClientTrafficPolicy")
        self.assertEqual(len(policies), 2)
        for policy in policies:
            self.assertEqual(policy["spec"]["tls"]["maxVersion"], "1.2")
            self.assertEqual(policy["spec"]["tls"]["ecdhCurves"], ["P-256", "P-384"])

    def test_proxy_and_alpn_have_one_policy(self):
        docs = render("gateway.proxyProtocol.enabled=true")
        policies = resources(docs, "ClientTrafficPolicy")
        self.assertEqual(len(policies), 1)
        self.assertIn("proxyProtocol", policies[0]["spec"])
        self.assertIn("alpnProtocols", policies[0]["spec"]["tls"])

    def test_pq_is_opt_in(self):
        docs = render("gateway.tls.ecdhCurves={X25519MLKEM768,X25519,P-256,P-384}")
        self.assertFalse(resources(docs, "EnvoyPatchPolicy"))
        self.assertEqual(resources(docs, "ClientTrafficPolicy")[0]["spec"]["tls"]["ecdhCurves"][0],
                         "X25519MLKEM768")

    def test_v2_listener_name(self):
        docs = render("BSI_TR_02102_2_conformance=true", "gateway.patchPolicies.xdsNameSchemeV2=true")
        self.assertEqual(resources(docs, "EnvoyPatchPolicy")[0]["spec"]["jsonPatches"][0]["name"],
                         "tcp-443")

    def test_extra_certificate_san(self):
        docs = render("tls.extraDnsNames={disallowed-clients.example.com}")
        self.assertIn("disallowed-clients.example.com",
                      resources(docs, "Certificate")[0]["spec"]["dnsNames"])

    def test_bsi_rejects_rsa_certificate_issuance(self):
        self.assertIn("ECDSA", render("BSI_TR_02102_2_conformance=true",
                                     "tls.privateKey.algorithm=RSA",
                                     "tls.privateKey.size=3072", success=False))


if __name__ == "__main__":
    unittest.main()
