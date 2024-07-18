#!/usr/bin/env bash
set -eo pipefail

openssl genpkey -algorithm ed25519 > ed25519.pem

for bits in 256 384 521
do
  hash_bits=$bits
  if [[ $bits == 521 ]]; then hash_bits=512; fi
  openssl genpkey -algorithm ec -pkeyopt ec_paramgen_curve:P-$bits \
      > "ecdsa_secp${bits}r1_sha${hash_bits}.pem"
done
