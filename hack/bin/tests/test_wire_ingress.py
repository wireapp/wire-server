"""Local checks for chart validation and the live runner's failure classification."""

import copy
import importlib.util
import json
import os
from pathlib import Path
import shutil
import subprocess
import tempfile
import unittest
from unittest.mock import Mock, patch


spec = importlib.util.spec_from_file_location(
    "ingress", Path(__file__).resolve().parents[1] / "wire-ingress-test.py"
)
ingress = importlib.util.module_from_spec(spec)
spec.loader.exec_module(ingress)


class ProbeTests(unittest.TestCase):
    def test_scenarios_release_namespaces_before_the_next_case(self):
        for fail in [False, True]:
            with self.subTest(fail=fail):
                suite = Mock()
                suite.namespaces = []
                cleaned = []

                def scenario(*args):
                    self.assertEqual(suite.namespaces, [])
                    suite.namespaces.append("owned")
                    if fail:
                        raise RuntimeError("scenario failed")

                suite.scenario.side_effect = scenario
                suite.cleanup.side_effect = lambda: cleaned.extend(suite.namespaces)
                with patch.object(ingress, "Suite", return_value=suite), patch(
                    "sys.argv", ["wire-ingress-test.py"]
                ):
                    if fail:
                        with self.assertRaisesRegex(RuntimeError, "scenario failed"):
                            ingress.main()
                    else:
                        ingress.main()
                self.assertEqual(len(cleaned), 1 if fail else 12)
                self.assertEqual(suite.namespaces, [])

    def test_rejection_requires_a_server_tls_alert(self):
        for error in ["Connection refused", "no cipher match", "unexpected eof", ""]:
            with self.subTest(error=error), self.assertRaises(AssertionError):
                ingress.assert_probe(
                    subprocess.CompletedProcess([], 1, "", error), False
                )
        ingress.assert_probe(
            subprocess.CompletedProcess([], 1, "", "ssl/tls alert handshake failure"),
            False,
        )

    def test_success_requires_verified_tls_and_the_expected_backend(self):
        output = (
            "CONNECTION ESTABLISHED\nVerification: OK\nProtocol version: TLSv1.3\n"
            "Ciphersuite: TLS_AES_128_GCM_SHA256\nHTTP/1.1 200 OK\n" + ingress.BODY
        )
        ingress.assert_probe(
            subprocess.CompletedProcess([], 0, output, ""),
            True,
            "TLSv1.3",
            "TLS_AES_128_GCM_SHA256",
        )
        for missing in ["Verification: OK", "HTTP/1.1 200", ingress.BODY]:
            with self.subTest(missing=missing), self.assertRaises(AssertionError):
                ingress.assert_probe(
                    subprocess.CompletedProcess([], 0, output.replace(missing, ""), ""),
                    True,
                )


class ChartTests(unittest.TestCase):
    def setUp(self):
        self.config = ingress.values("test-render", False, "fips", "envoy", False)

    def render(self, config, template=None):
        with tempfile.TemporaryDirectory() as directory:
            path = Path(directory) / "values.json"
            path.write_text(json.dumps(config))
            args = [
                "helm",
                "template",
                "regression",
                str(ingress.CHART),
                "-n",
                "test-render",
                "-f",
                str(path),
            ]
            if template:
                args += ["--show-only", "templates/" + template]
            return ingress.run(*args, check=False)

    def test_fips_rejects_unsafe_or_ambiguous_configuration(self):
        for key, value, message in [
            ("FIPS_202205_tls_profile", "true", "must be a boolean"),
            ("gateway.tls.enabled", False, "requires gateway.tls.enabled"),
            ("gateway.patchPolicies.enabled", False, "requires gateway.tls.enabled"),
            ("gateway.patchPolicies.xdsNameSchemeV2", None, "must be set explicitly"),
            (
                "gateway.patchPolicies.targetGatewayClass",
                True,
                "requires a dedicated Gateway",
            ),
            (
                "gateway.envoyProxy.spec.mergeGateways",
                True,
                "requires a dedicated Gateway",
            ),
            ("gateway.allowedListenerNamespaces", "wrong-type", "must be a list"),
        ]:
            with self.subTest(key=key):
                config = copy.deepcopy(self.config)
                target = config
                *parents, leaf = key.split(".")
                for parent in parents:
                    target = target.setdefault(parent, {})
                target[leaf] = value
                result = self.render(config)
                self.assertNotEqual(result.returncode, 0)
                self.assertIn(message, result.stderr)

    def test_both_xds_names_and_all_filter_chains(self):
        for v2, expected in [
            (False, "test-render/regression/test-render/regression-wire-ingress/https"),
            (True, "tcp-443"),
        ]:
            with self.subTest(v2=v2):
                self.config["gateway"]["patchPolicies"]["xdsNameSchemeV2"] = v2
                result = self.render(self.config, "envoypatchpolicy-bsi.yaml")
                self.assertEqual(result.returncode, 0, result.stderr)
                self.assertIn(f'name: "{expected}"', result.stdout)
                self.assertIn(
                    "$.filter_chains[*].transport_socket.typed_config.common_tls_context",
                    result.stdout,
                )
                self.assertIn("compliance_policies: [FIPS_202205]", result.stdout)

    def test_multi_ingress_requires_separate_secret_and_excludes_federation(self):
        config = ingress.values(
            "test-render", False, "default", "envoy", False, multi=True
        )
        result = self.render(config)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertIn("secondary-https", result.stdout)
        self.assertIn("secondary-tls", result.stdout)
        self.assertNotIn("kind: EnvoyExtensionPolicy", result.stdout)
        config["federator"]["enabled"] = True
        result = self.render(config)
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("mutually exclusive", result.stderr)
        config["federator"]["enabled"] = False
        del config["config"]["domains"][1]["tls"]
        result = self.render(config)
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("additional domains need their own TLS secret", result.stderr)

    def test_multi_ingress_csp_is_scoped_to_additional_team_settings(self):
        config = ingress.values(
            "test-render", False, "default", "envoy", False, multi=True
        )
        config["teamSettings"] = {"enabled": True}
        for domain in config["config"]["domains"]:
            domain["dns"]["teamSettings"] = "teams." + domain["base"]
        result = self.render(config, "httproute-team-settings.yaml")
        self.assertEqual(result.returncode, 0, result.stderr)
        routes = [doc for doc in result.stdout.split("---") if "kind: HTTPRoute" in doc]
        self.assertEqual(len(routes), 2)
        self.assertNotIn("Content-Security-Policy", routes[0])
        self.assertIn("Content-Security-Policy", routes[1])
        self.assertIn("*.other.example.test", routes[1])
        result = self.render(config, "httproute-webapp.yaml")
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertNotIn("Content-Security-Policy", result.stdout)
        config["config"]["domains"][1]["renderCSP"] = False
        result = self.render(config, "httproute-team-settings.yaml")
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertNotIn("Content-Security-Policy", result.stdout)

    def test_ha_defaults_and_single_replica_override(self):
        del self.config["gateway"]["envoyProxy"]["replicas"]
        result = self.render(self.config, "envoyproxy.yaml")
        self.assertEqual(result.returncode, 0, result.stderr)
        for expected in [
            "replicas: 3",
            "envoyPDB:",
            "minAvailable: 1",
            "topology.kubernetes.io/zone",
            "kubernetes.io/hostname",
        ]:
            self.assertIn(expected, result.stdout)
        self.config["gateway"]["envoyProxy"]["spec"] = {
            "provider": {"kubernetes": {"envoyDeployment": {"replicas": 1}}}
        }
        result = self.render(self.config, "envoyproxy.yaml")
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertIn("replicas: 1", result.stdout)
        self.assertNotIn("envoyPDB:", result.stdout)


class IntegrationFlowTests(unittest.TestCase):
    def run_flow(self, chart, mode=None):
        # Stub all cluster commands; a failing regression must stop before any
        # application-suite setup or RabbitMQ secret mutation can happen.
        with tempfile.TemporaryDirectory() as directory:
            directory = Path(directory)
            shutil.copy(ingress.ROOT / "hack/bin/integration-test.sh", directory)
            scripts = {
                "helm": "printf '%s\\n' \"$TEST_RELEASES\"",
                "kubectl": "exit 77",
                "run-wire-ingress-tests.sh": "echo REGRESSION-RUNNER; exit 73",
            }
            for name, contents in scripts.items():
                path = directory / name
                path.write_text("#!/usr/bin/env bash\n" + contents + "\n")
                path.chmod(0o755)
            env = os.environ.copy()
            env.pop("WIRE_INGRESS_MODE", None)
            env.update(
                PATH=str(directory) + os.pathsep + env["PATH"],
                TEST_RELEASES=json.dumps([{"chart": chart}]),
            )
            if mode is not None:
                env["WIRE_INGRESS_MODE"] = mode
            return subprocess.run(
                ["bash", str(directory / "integration-test.sh")],
                env=env,
                text=True,
                capture_output=True,
                timeout=10,
            )

    def test_detects_installed_chart_and_propagates_failure(self):
        result = self.run_flow("wire-ingress-1.0.0")
        self.assertEqual(result.returncode, 73, result.stderr)
        self.assertIn("REGRESSION-RUNNER", result.stdout)

    def test_explicit_envoy_mode_runs_regressions(self):
        result = self.run_flow("nginx-ingress-services-1.0.0", "envoy")
        self.assertEqual(result.returncode, 73, result.stderr)

    def test_nginx_does_not_require_envoy(self):
        for chart, mode in [
            ("nginx-ingress-services-1.0.0", None),
            ("wire-ingress-1.0.0", "nginx"),
        ]:
            with self.subTest(mode=mode):
                result = self.run_flow(chart, mode)
                self.assertEqual(result.returncode, 77, result.stderr)
                self.assertNotIn("REGRESSION-RUNNER", result.stdout)


if __name__ == "__main__":
    unittest.main()
