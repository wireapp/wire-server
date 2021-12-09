#!/usr/bin/env bash

set -e
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
TOP_LEVEL="$(cd "$DIR/../.." && pwd)"

cat >>$TOP_LEVEL/stack.yaml <<EOF
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
