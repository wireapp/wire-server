import copy
import json
import pathlib
import unittest

from render import render


class RenderingTests(unittest.TestCase):
    def setUp(self):
        self.config = json.loads(
            pathlib.Path(__file__).with_name("config.example.json").read_text()
        )
        self.config["nodes"] = dict(services="a", dependencies="b", generator="c")
        self.config["images"] = {
            k: "example.invalid/" + k + ":test" for k in self.config["images"]
        }

    def test_all_stages_and_namespace_scope(self):
        for stage in ["infra", "schema", "services", "smoke", "load"]:
            items = render(self.config, stage)["items"]
            self.assertTrue(items)
            for item in items:
                if item["kind"] != "Namespace":
                    self.assertEqual(item["metadata"]["namespace"], "fan-in-perf-ci")
                if item["kind"] in ["Deployment", "Job"]:
                    pod = item["spec"]["template"]["spec"]
                    self.assertFalse(pod["automountServiceAccountToken"])
                    self.assertIn(pod["nodeSelector"]["kubernetes.io/hostname"], "abc")
                    if stage in ["smoke", "load"]:
                        self.assertEqual(
                            pod["nodeSelector"]["kubernetes.io/hostname"], "c"
                        )
                        self.assertEqual(item["spec"]["backoffLimit"], 0)
                if item["kind"] == "Service":
                    self.assertNotIn("type", item["spec"])

    def test_rejects_shared_namespace_and_bad_parameters(self):
        for namespace in ["wire", "default", "fan-in-perf-../../wire"]:
            cfg = copy.deepcopy(self.config)
            cfg["namespace"] = namespace
            with self.assertRaises(ValueError):
                render(cfg, "infra")
        with self.assertRaises(ValueError):
            render(self.config, "load", run_id="../bad")
        with self.assertRaises(ValueError):
            render(self.config, "load", devices=0)
        self.config["nodes"]["generator"] = "a"
        with self.assertRaises(ValueError):
            render(self.config, "load")

    def test_initialization_requires_empty_database(self):
        jobs = render(self.config, "schema")["items"]
        pg = next(j for j in jobs if j["metadata"]["name"] == "postgres-schema")
        pod = pg["spec"]["template"]["spec"]
        self.assertIn('test "$n" = 0', pod["initContainers"][0]["command"][-1])
        self.assertEqual(
            pod["containers"][0]["env"][0]["valueFrom"]["secretKeyRef"]["key"],
            "postgres-password",
        )

    def test_no_real_aws_endpoint_and_single_cannon(self):
        items = render(self.config, "services")["items"]
        cfg = next(
            x
            for x in items
            if x["kind"] == "ConfigMap" and x["metadata"]["name"] == "gundeck"
        )
        config = json.loads(cfg["data"]["config.yaml"])
        self.assertEqual(config["aws"]["snsEndpoint"], "http://127.0.0.1:9")
        self.assertIs(config["redis"]["insecureSkipVerifyTls"], False)
        cannon = next(
            x
            for x in items
            if x["kind"] == "Deployment" and x["metadata"]["name"] == "cannon"
        )
        self.assertEqual(cannon["spec"]["replicas"], 1)

    def test_network_policy_has_no_external_egress(self):
        policy = next(
            x
            for x in render(self.config, "infra")["items"]
            if x["kind"] == "NetworkPolicy"
        )
        self.assertEqual(policy["spec"]["ingress"], [{"from": [{"podSelector": {}}]}])
        self.assertEqual(policy["spec"]["egress"][0], {"to": [{"podSelector": {}}]})
        self.assertEqual(
            {p["port"] for p in policy["spec"]["egress"][1]["ports"]}, {53}
        )


if __name__ == "__main__":
    unittest.main()
