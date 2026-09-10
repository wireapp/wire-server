#!/usr/bin/env python3
"""Disruptive PoC-only test: remove our patch, verify safe baseline, restore it.

Only touches joe-test/tr-bsi-ciphers. TLS 1.3 briefly becomes unavailable.
"""

import argparse
import json
import subprocess
import time


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--kubeconfig", default="./kubeconfig")
    parser.add_argument("--address", default="46.225.37.184")
    parser.add_argument("--host", default="tr.hops.wire.link")
    parser.add_argument("--cafile")
    args = parser.parse_args()
    kubectl = ["kubectl", "--kubeconfig", args.kubeconfig, "-n", "joe-test"]
    original = json.loads(subprocess.check_output(kubectl + ["get", "envoypatchpolicy", "tr-bsi-ciphers", "-o", "json"]))
    original["metadata"] = {"name": "tr-bsi-ciphers", "namespace": "joe-test"}
    original.pop("status", None)
    command = ["openssl", "s_client", "-connect", f"{args.address}:443",
               "-servername", args.host, "-verify_hostname", args.host,
               "-verify_return_error", "-brief"]
    if args.cafile:
        command += ["-CAfile", args.cafile]

    def probe(flags):
        return subprocess.run(command + flags, input="", text=True,
                              stdout=subprocess.PIPE, stderr=subprocess.STDOUT, timeout=10)

    def await_state(flags, predicate):
        deadline = time.monotonic() + 45
        while time.monotonic() < deadline:
            result = probe(flags)
            if predicate(result):
                return result
            time.sleep(1)
        raise RuntimeError("Expected TLS behavior did not appear: " + result.stdout)

    tls13 = ["-tls1_3", "-ciphersuites", "TLS_AES_256_GCM_SHA384"]
    try:
        subprocess.run(kubectl + ["delete", "envoypatchpolicy", "tr-bsi-ciphers"], check=True)
        rejected = await_state(tls13, lambda r: "alert protocol version" in r.stdout and "CONNECTION ESTABLISHED" not in r.stdout)
        print("PASS patch absent: TLS 1.3 rejected with protocol_version alert", flush=True)
        for cipher in ["ECDHE-ECDSA-AES128-GCM-SHA256", "ECDHE-ECDSA-AES256-GCM-SHA384"]:
            result = probe(["-tls1_2", "-cipher", cipher, "-groups", "P-256:P-384"])
            if result.returncode or f"Ciphersuite: {cipher}" not in result.stdout or "Verification: OK" not in result.stdout:
                raise RuntimeError(result.stdout)
        result = probe(["-tls1_2", "-cipher", "ECDHE-ECDSA-CHACHA20-POLY1305"])
        if "alert handshake failure" not in result.stdout or "CONNECTION ESTABLISHED" in result.stdout:
            raise RuntimeError(result.stdout)
        print("PASS patch absent: TLS 1.2 AES-GCM works; ChaCha20 rejected", flush=True)
    finally:
        subprocess.run(kubectl + ["apply", "-f", "-"], input=json.dumps(original), text=True, check=True)
    await_state(tls13, lambda r: r.returncode == 0 and "Ciphersuite: TLS_AES_256_GCM_SHA384" in r.stdout and "Verification: OK" in r.stdout)
    print("PASS patch restored: verified TLS 1.3 AES-GCM handshake", flush=True)


if __name__ == "__main__":
    main()
