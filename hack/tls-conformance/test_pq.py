#!/usr/bin/env python3
"""Verify hybrid and classical TLS 1.3 handshakes on every target IP.

Requires OpenSSL 3.5+ with X25519MLKEM768. No certificate verification bypass.
Timeouts, client errors and unexpected negotiation fail the test.
"""
import argparse
import ipaddress
import json
import pathlib
import re
import socket
import subprocess


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("host")
    parser.add_argument("--port", type=int, default=443)
    parser.add_argument("--address", action="append")
    parser.add_argument("--json", type=pathlib.Path)
    args = parser.parse_args()
    if not re.fullmatch(r"[A-Za-z0-9](?:[A-Za-z0-9.-]{0,251}[A-Za-z0-9])?", args.host):
        parser.error("host must be a DNS name")
    if not 1 <= args.port <= 65535:
        parser.error("port must be 1..65535")
    try:
        addresses = sorted({str(ipaddress.ip_address(a)) for a in args.address}) if args.address else sorted({
            r[4][0] for r in socket.getaddrinfo(args.host, args.port, type=socket.SOCK_STREAM)
        })
    except (ValueError, socket.gaierror) as error:
        parser.error(str(error))
    evidence = []
    for address in addresses:
        endpoint = f"[{address}]:{args.port}" if ":" in address else f"{address}:{args.port}"
        for group in ["X25519MLKEM768", "X25519"]:
            command = ["openssl", "s_client", "-connect", endpoint, "-servername", args.host,
                       "-verify_hostname", args.host, "-verify_return_error",
                       "-tls1_3", "-groups", group, "-brief"]
            try:
                result = subprocess.run(command, input="", text=True, capture_output=True, timeout=15)
                output = result.stdout + result.stderr
                passed = result.returncode == 0 and all(marker in output for marker in [
                    "CONNECTION ESTABLISHED", "Protocol version: TLSv1.3", "Verification: OK",
                ]) and group in output
            except subprocess.TimeoutExpired:
                output, passed = "TIMEOUT (inconclusive)", False
            evidence.append({"address": address, "group": group, "passed": passed, "output": output})
            print(f"{'PASS' if passed else 'FAIL'} {address}: TLS 1.3 {group}")
    if args.json:
        args.json.write_text(json.dumps({"host": args.host, "port": args.port, "results": evidence}, indent=2) + "\n")
    raise SystemExit(0 if evidence and all(e["passed"] for e in evidence) else 1)


if __name__ == "__main__":
    main()
