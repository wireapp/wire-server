#!/usr/bin/env bash

openssl genpkey -algorithm ed25519 > ed25519.pem
openssl genpkey -algorithm ec -pkeyopt ec_paramgen_curve:P-256 > ecdsa_secp256r1_sha256.pem
openssl genpkey -algorithm ec -pkeyopt ec_paramgen_curve:P-384 > ecdsa_secp384r1_sha384.pem
openssl genpkey -algorithm ec -pkeyopt ec_paramgen_curve:P-521 > ecdsa_secp521r1_sha512.pem
