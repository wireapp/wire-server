#!/usr/bin/env python3
"""Fail closed when a testssl.sh JSON scan violates the deployed cipher profile.

Requires --protocols --cipher-per-proto --server-defaults output. Also requires
a trusted, hostname-valid certificate; bootstrap scans intentionally fail this.
"""

import json
import re
import sys
from collections import defaultdict

from test_tls import TLS12_ALLOWED, TLS12_REQUIRED, TLS13_ALLOWED


def check(rows):
    if not isinstance(rows, list) or not rows:
        raise ValueError("empty or unsupported testssl report")
    endpoints = defaultdict(list)
    for row in rows:
        endpoints[(row["ip"], row["port"])].append(row)
    failures = []
    for endpoint, findings in endpoints.items():
        values = defaultdict(list)
        for row in findings:
            values[row["id"]].append(row["finding"].strip())
            # Cross-check individual successful cipher probes too, so a
            # contradictory or incomplete summary cannot hide an extra suite.
            cipher = re.fullmatch(r"cipher-(.+)_x([0-9a-f]+)", row["id"])
            if cipher and (cipher.group(1), cipher.group(2)) not in {
                ("tls1_2", "c02b"), ("tls1_2", "c02c"),
                ("tls1_2", "c02f"), ("tls1_2", "c030"),
                ("tls1_3", "1301"), ("tls1_3", "1302"),
            }:
                failures.append(f"{endpoint}: non-allowlisted cipher probe {row['id']}")
            if row["severity"] in {"FATAL", "WARN"}:
                failures.append(f"{endpoint}: inconclusive scanner finding {row['id']}: {row['finding']}")
        for protocol in ["SSLv2", "SSLv3", "TLS1", "TLS1_1"]:
            if values[protocol] != ["not offered"]:
                failures.append(f"{endpoint}: {protocol} not conclusively disabled")
        for protocol in ["TLS1_2", "TLS1_3"]:
            if not values[protocol] or not all(v.startswith("offered") for v in values[protocol]):
                failures.append(f"{endpoint}: {protocol} missing")
        for protocol, allowed, required in [("1_2", TLS12_ALLOWED, TLS12_REQUIRED), ("1_3", TLS13_ALLOWED, TLS13_ALLOWED)]:
            suites = values[f"supportedciphers_TLS {protocol}"]
            if not suites:
                failures.append(f"{endpoint}: no cipher-per-protocol inventory for TLS {protocol}")
            for suite_list in suites:
                offered = set(suite_list.split())
                if offered - allowed or not required <= offered:
                    failures.append(f"{endpoint}: unexpected TLS {protocol} inventory: {suite_list}")
        # Require complete output and authentication evidence, not just green ciphers.
        if not values["scanTime"]:
            failures.append(f"{endpoint}: scan did not finish")
        for field in ["cert_trust", "cert_chain_of_trust"]:
            valid = all(
                v.lower() in {"ok", "passed", "passed."} if field == "cert_chain_of_trust"
                else v.lower().startswith("ok")
                for v in values[field]
            )
            if not values[field] or not valid:
                failures.append(f"{endpoint}: {field}: {values[field]}")
    return failures


if __name__ == "__main__":
    if len(sys.argv) != 2:
        sys.exit("usage: python3 check-testssl.py REPORT.json")
    try:
        with open(sys.argv[1]) as report:
            failures = check(json.load(report))
    except (OSError, ValueError, KeyError, TypeError) as error:
        sys.exit(f"FAIL invalid/incomplete report: {error}")
    if failures:
        sys.exit("FAIL\n" + "\n".join(failures))
    print("PASS: testssl cipher inventory, protocol versions and public certificate")
