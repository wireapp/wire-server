#!/usr/bin/env python3
"""Test ONLY the synthetic Hops admin-auth fixture via a local port-forward."""
import argparse
import base64
import http.client
import urllib.parse


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--port", type=int, default=18080)
    args = parser.parse_args()
    cases = [
        ("OAuth missing cookie", "admin.hops.wire.link", "/", {}, 302),
        ("OAuth allowed", "admin.hops.wire.link", "/", {"Cookie": "test=allow"}, 200),
        ("OAuth forbidden group", "admin.hops.wire.link", "/", {"Cookie": "test=deny"}, 403),
        ("OAuth outage fails closed", "admin.hops.wire.link", "/", {"Cookie": "test=error"}, 500),
        ("Spoofed identity fails", "admin.hops.wire.link", "/", {"X-Auth-Request-User": "admin"}, 302),
        ("Internal auth path hidden", "admin.hops.wire.link", "/_wire_auth", {}, 404),
        ("Basic missing password", "brig-test.hops.wire.link", "/", {}, 401),
        ("Basic wrong password", "brig-test.hops.wire.link", "/",
         {"Authorization": "Basic " + base64.b64encode(b"test-user:wrong").decode()}, 401),
        ("Basic existing APR1 hash", "brig-test.hops.wire.link", "/",
         {"Authorization": "Basic " + base64.b64encode(b"test-user:test-password-not-for-production").decode()}, 200),
        ("Unknown host", "unknown.example.com", "/", {}, 404),
        ("Redirect URI encoding", "admin.hops.wire.link", "/test?x=1&rd=https://evil.example/", {}, 302),
    ]
    failures = []
    for label, host, path, headers, status in cases:
        connection = http.client.HTTPConnection("127.0.0.1", args.port, timeout=20)
        connection.request("GET", path, headers={"Host": host, **headers})
        response = connection.getresponse()
        body = response.read()
        passed = response.status == status
        if status == 200:
            passed = passed and b"Wire TLS proof of concept" in body
        if status == 302:
            location = urllib.parse.urlsplit(response.getheader("Location", ""))
            query = urllib.parse.parse_qs(location.query)
            passed = passed and location.scheme == "https" and location.netloc == "tr.hops.wire.link"
            passed = passed and query == {"rd": ["https://admin.hops.wire.link" + path]}
        if status == 401:
            passed = passed and response.getheader("WWW-Authenticate") == 'Basic realm="Authentication Required"'
        connection.close()
        print(f"{'PASS' if passed else 'FAIL'} {label}: HTTP {response.status}")
        if not passed:
            failures.append(label)
    raise SystemExit(1 if failures else 0)


if __name__ == "__main__":
    main()
