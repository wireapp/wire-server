#!/usr/bin/env python3
"""Check the exact deployed BSI AES-GCM profile. Requires OpenSSL 3.x.

This is deliberately stricter than the whole BSI list: only the implemented
AES-GCM subset is accepted. Run testssl.sh as well for its broader cipher corpus.
Network errors, local OpenSSL errors and timeouts never count as cipher rejection.
"""

import argparse
import concurrent.futures
import datetime
import ipaddress
import json
import re
import shutil
import socket
import subprocess
import sys
from pathlib import Path


TLS13_ALLOWED = {"TLS_AES_128_GCM_SHA256", "TLS_AES_256_GCM_SHA384"}
TLS13_REJECTED = {
    "TLS_CHACHA20_POLY1305_SHA256",
    "TLS_AES_128_CCM_8_SHA256",
    # BSI permits CCM, but this deployment deliberately implements only GCM.
    "TLS_AES_128_CCM_SHA256",
}
TLS12_ALLOWED = {
    "ECDHE-ECDSA-AES128-GCM-SHA256",
    "ECDHE-ECDSA-AES256-GCM-SHA384",
    "ECDHE-RSA-AES128-GCM-SHA256",
    "ECDHE-RSA-AES256-GCM-SHA384",
}
TLS12_REQUIRED = {c for c in TLS12_ALLOWED if "ECDSA" in c}


def run(command, timeout=15, input_text=""):
    try:
        result = subprocess.run(
            command, input=input_text, text=True, stdout=subprocess.PIPE,
            stderr=subprocess.STDOUT, timeout=timeout, check=False,
        )
        return result.returncode, result.stdout
    except subprocess.TimeoutExpired:
        return 124, "TIMEOUT (inconclusive, not a TLS rejection)"


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("host", nargs="?", default="tr.hops.wire.link")
    parser.add_argument("--port", type=int, default=443)
    parser.add_argument("--address", action="append", help="explicit IP; default: every DNS A/AAAA address")
    parser.add_argument("--openssl", default="openssl")
    parser.add_argument("--cafile", type=Path, help="explicit trust anchor for a diagnostic/private CA")
    parser.add_argument("--json", type=Path, help="write detailed evidence report")
    parser.add_argument("--workers", type=int, default=6)
    args = parser.parse_args()
    if not re.fullmatch(r"[A-Za-z0-9](?:[A-Za-z0-9.-]{0,251}[A-Za-z0-9])?", args.host):
        parser.error("host must be a DNS name, without a scheme or port")
    if not 1 <= args.port <= 65535 or not 1 <= args.workers <= 16:
        parser.error("port must be 1..65535 and workers 1..16")
    executable = shutil.which(args.openssl)
    if executable is None:
        parser.error("OpenSSL executable not found")
    try:
        if args.address:
            addresses = sorted({str(ipaddress.ip_address(a)) for a in args.address})
        else:
            addresses = sorted({r[4][0] for r in socket.getaddrinfo(args.host, args.port, type=socket.SOCK_STREAM)})
    except (ValueError, socket.gaierror) as error:
        parser.error(f"cannot resolve/validate target addresses: {error}")
    version_rc, version = run([executable, "version"])
    if version_rc or not version.startswith("OpenSSL 3."):
        parser.error("use OpenSSL 3.x, including support for both TLS 1.3 CCM suites")

    # -s excludes ciphers the client cannot actually offer (e.g. unavailable PSK
    # authentication). The independent testssl scan covers beyond this corpus.
    rc, inventory = run([executable, "ciphers", "-s", "-tls1_2", "-stdname", "-v", "ALL:COMPLEMENTOFALL:@SECLEVEL=0"])
    if rc:
        parser.error("could not enumerate the client's TLS 1.2 cipher support")
    tls12 = []
    for line in inventory.splitlines():
        match = re.match(r"(TLS_\S+)\s+-\s+(\S+)\s+", line)
        if match:
            tls12.append(match.group(2))
    if not TLS12_REQUIRED.issubset(tls12):
        parser.error("client cannot test required TLS 1.2 cipher suites")

    results = []

    def certificate_chain(address):
        endpoint = f"[{address}]:{args.port}" if ":" in address else f"{address}:{args.port}"
        command = [executable, "s_client", "-connect", endpoint, "-servername", args.host,
                   "-verify_return_error", "-verify_hostname", args.host, "-showcerts"]
        if args.cafile:
            command += ["-CAfile", str(args.cafile)]
        rc, output = run(command)
        certificates = re.findall(r"-----BEGIN CERTIFICATE-----.*?-----END CERTIFICATE-----", output, re.DOTALL)
        passed = rc == 0 and bool(certificates) and "Verify return code: 0 (ok)" in output
        evidence = []
        for index, pem in enumerate(certificates):
            cert_rc, details = run([executable, "x509", "-text", "-noout"], input_text=pem)
            signatures = set(re.findall(r"Signature Algorithm:\s+(\S+)", details))
            curve = re.search(r"ASN1 OID:\s+(\S+)", details)
            # This deployment deliberately uses an all-ECDSA public chain.
            # Fail if preferredChain silently falls back to the RSA X1 cross-sign.
            valid = cert_rc == 0 and bool(signatures) and signatures <= {
                "ecdsa-with-SHA256", "ecdsa-with-SHA384", "ecdsa-with-SHA512",
            } and curve is not None and curve.group(1) in {"prime256v1", "secp384r1", "secp521r1"}
            passed = passed and valid
            evidence.append(f"certificate {index}: signatures={sorted(signatures)}, curve={curve.group(1) if curve else 'unknown'}")
        if not certificates:
            evidence.append(output.strip())
        return {"address": address, "test": "served certificate chain uses BSI-listed ECDSA signatures and curves",
                "expected": "accept", "passed": bool(passed), "exit_code": rc, "output": "\n".join(evidence)}

    def probe(address, label, flags, expected, cipher=None):
        endpoint = f"[{address}]:{args.port}" if ":" in address else f"{address}:{args.port}"
        command = [executable, "s_client", "-connect", endpoint, "-brief"]
        if "-noservername" not in flags:
            command += ["-servername", args.host]
        command += ["-verify_return_error", "-verify_hostname", args.host]
        if args.cafile:
            command += ["-CAfile", str(args.cafile)]
        command += flags
        rc, output = run(command)
        negotiated = re.search(r"Ciphersuite:\s*(\S+)", output)
        success = rc == 0 and negotiated is not None and "Verification: OK" in output
        # Require a peer-generated TLS alert, not merely a nonzero client exit.
        rejected = not negotiated and bool(re.search(
            r"(?:alert handshake failure|alert protocol version|alert insufficient security)", output,
            re.IGNORECASE,
        ))
        if expected == "accept":
            passed = success and (cipher is None or negotiated.group(1) == cipher)
        elif expected == "reject":
            passed = rejected
        elif expected == "sni-reject":
            # Envoy closes TCP when no filter chain matches. Only use this rule
            # for SNI routing checks; it cannot prove a cipher was rejected.
            passed = not negotiated and bool(re.search(
                r"alert|unexpected eof|Connection reset|write:errno=104|read:errno=104", output,
                re.IGNORECASE,
            ))
        else:  # RSA suites are allowed, but absent with an ECDSA-only certificate.
            passed = rejected or (success and negotiated.group(1) in TLS12_ALLOWED)
        return {"address": address, "test": label, "expected": expected,
                "passed": bool(passed), "exit_code": rc, "output": output.strip()}

    tasks = []
    for address in addresses:
        results.append(certificate_chain(address))
        # Positive controls first: a dead endpoint must never produce a green scan.
        for cipher in sorted(TLS13_ALLOWED):
            result = probe(address, f"TLS1.3 {cipher}", ["-tls1_3", "-ciphersuites", cipher, "-groups", "P-256"], "accept", cipher)
            results.append(result)
            if not result["passed"]:
                print(json.dumps(result, indent=2), file=sys.stderr)
                return 1
        for cipher in sorted(TLS13_REJECTED):
            tasks.append((address, f"TLS1.3 rejects {cipher}", ["-tls1_3", "-ciphersuites", cipher, "-cipher", "ALL:@SECLEVEL=0", "-groups", "P-256"], "reject"))
        for cipher in tls12:
            expectation = "accept" if cipher in TLS12_REQUIRED else "optional" if cipher in TLS12_ALLOWED else "reject"
            tasks.append((address, f"TLS1.2 {cipher}", ["-tls1_2", "-cipher", cipher + ":@SECLEVEL=0", "-groups", "P-256:P-384"], expectation, cipher))
        for group in ["P-256", "P-384", "X25519", "P-521", "X25519MLKEM768", "SecP256r1MLKEM768", "SecP384r1MLKEM1024"]:
            expected = "accept" if group in {"P-256", "P-384"} else "reject"
            tasks.append((address, f"TLS1.3 group {group}", ["-tls1_3", "-ciphersuites", "TLS_AES_256_GCM_SHA384", "-groups", group], expected))
        for protocol in ["-tls1", "-tls1_1"]:
            tasks.append((address, f"rejects {protocol}", [protocol, "-cipher", "ALL:@SECLEVEL=0"], "reject"))
        tasks.append((address, "mixed offer prefers forbidden ChaCha but negotiates AES", ["-tls1_3", "-ciphersuites", "TLS_CHACHA20_POLY1305_SHA256:TLS_AES_128_GCM_SHA256", "-groups", "P-256"], "accept", "TLS_AES_128_GCM_SHA256"))
        # Rejects forbidden offers even when older TLS versions are also offered.
        tasks.append((address, "no fallback from forbidden-only TLS1.3/TLS1.2 offer", ["-min_protocol", "TLSv1.2", "-ciphersuites", "TLS_CHACHA20_POLY1305_SHA256", "-cipher", "ECDHE-ECDSA-CHACHA20-POLY1305"], "reject"))
        for protocol in ["-tls1_2", "-tls1_3"]:
            tasks.append((address, f"{protocol} no SNI has no default TLS backend", [protocol, "-noservername"], "sni-reject"))
            tasks.append((address, f"{protocol} unknown SNI has no default TLS backend", [protocol, "-servername", "unconfigured.invalid"], "sni-reject"))

    print(f"Testing {args.host}: {', '.join(addresses)}; {len(tasks) + len(results)} probes", flush=True)
    with concurrent.futures.ThreadPoolExecutor(max_workers=args.workers) as pool:
        for result in pool.map(lambda task: probe(*task), tasks):
            results.append(result)
            if not result["passed"]:
                print(f"FAIL {result['address']} {result['test']}: {result['output']}", flush=True)
    failed = [r for r in results if not r["passed"]]
    report = {
        "timestamp": datetime.datetime.now(datetime.timezone.utc).isoformat(),
        "host": args.host, "addresses": addresses, "openssl": version.strip(),
        "tls12_client_cipher_count": len(tls12), "passed": not failed,
        "scope": "Exact AES-GCM profile, OpenSSL-supported ciphers and served ECDSA certificate chain; complement with testssl.sh. Not a full BSI certification.",
        "results": results,
    }
    if args.json:
        args.json.write_text(json.dumps(report, indent=2) + "\n")
    print(f"{'PASS' if not failed else 'FAIL'}: {len(results) - len(failed)}/{len(results)} probes")
    return int(bool(failed))


if __name__ == "__main__":
    sys.exit(main())
