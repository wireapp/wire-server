#!/usr/bin/env bash

set -e
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
TOP_LEVEL="$(cd "$DIR/../.." && pwd)"

cat >>$TOP_LEVEL/stack.yaml <<EOF

- Cabal-3.6.2.0@sha256:e2266e14758c1f799220fad7f0d4b0b4ec567d81b7ba3faea17ff76d4c31de95,12437
- parsec-3.1.15.0@sha256:a162d4cc8884014ba35192545cad293af0529fe11497aad8834bbaaa3dfffc26,4429
- text-1.2.5.0@sha256:791f0f6c97ed96113f17ab520cf0efe1a3a4f883a8c85910a5660567c8241c40,7895

flags:
  cryptonite:
    integer-gmp: false
  hashable:
    integer-gmp: false
  integer-logarithms:
    integer-gmp: false
  scientific:
    integer-simple: true
EOF
