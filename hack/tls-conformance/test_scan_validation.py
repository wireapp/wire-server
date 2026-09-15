#!/usr/bin/env python3
"""Offline security regression checks for the scan-result gate."""

import copy
import runpy
import unittest
from pathlib import Path

from test_tls import TLS12_REQUIRED, TLS13_ALLOWED

check = runpy.run_path(str(Path(__file__).with_name("check-testssl.py")))["check"]


class ScanValidation(unittest.TestCase):
    def setUp(self):
        findings = {
            "SSLv2": "not offered", "SSLv3": "not offered",
            "TLS1": "not offered", "TLS1_1": "not offered",
            "TLS1_2": "offered", "TLS1_3": "offered with final",
            "supportedciphers_TLS 1_2": " ".join(sorted(TLS12_REQUIRED)),
            "supportedciphers_TLS 1_3": " ".join(sorted(TLS13_ALLOWED)),
            "cert_trust": "Ok via SAN", "cert_chain_of_trust": "passed.",
            "scanTime": "25",
        }
        self.rows = [{"id": key, "finding": value, "ip": "test.invalid/192.0.2.1",
                      "port": "443", "severity": "INFO"} for key, value in findings.items()]

    def test_valid_inventory(self):
        self.assertEqual(check(self.rows), [])

    def test_rejects_extra_cipher_even_if_scanner_calls_it_secure(self):
        for row in self.rows:
            if row["id"] == "supportedciphers_TLS 1_3":
                row["finding"] += " TLS_CHACHA20_POLY1305_SHA256"
                row["severity"] = "OK"
        self.assertTrue(check(self.rows))

    def test_rejects_missing_protocol_scan_or_completion(self):
        for field in ["TLS1_1", "supportedciphers_TLS 1_3", "scanTime"]:
            with self.subTest(field=field):
                self.assertTrue(check([r for r in self.rows if r["id"] != field]))

    def test_rejects_untrusted_certificate(self):
        for row in self.rows:
            if row["id"] == "cert_chain_of_trust":
                row["finding"] = "failed (chain incomplete)"
        self.assertTrue(check(self.rows))

    def test_rejects_extra_cipher_missing_from_summary(self):
        self.rows.append({"id": "cipher-tls1_3_x1303", "finding": "TLS_CHACHA20_POLY1305_SHA256",
                          "ip": "test.invalid/192.0.2.1", "port": "443", "severity": "OK"})
        self.assertTrue(check(self.rows))

    def test_checks_each_address(self):
        other = copy.deepcopy(self.rows)
        for row in other:
            row["ip"] = "test.invalid/192.0.2.2"
            if row["id"] == "TLS1":
                row["finding"] = "offered"
        self.assertTrue(check(self.rows + other))

    def test_rejects_empty_and_inconclusive_scan(self):
        with self.assertRaises(ValueError):
            check([])
        self.rows.append({"id": "scan_error", "finding": "timeout", "ip": "test.invalid/192.0.2.1",
                          "port": "443", "severity": "WARN"})
        self.assertTrue(check(self.rows))


if __name__ == "__main__":
    unittest.main()
