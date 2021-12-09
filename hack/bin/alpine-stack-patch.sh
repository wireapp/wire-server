#!/usr/bin/env bash

set -e
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
TOP_LEVEL="$(cd "$DIR/../.." && pwd)"

tmpStackYaml=$(mktemp)
yq '. + {flags: {cryptonite: {"integer-gmp": false}, hashable: {"interger-gmp": false}, "integer-logarithms": {"integer-gmp": false}, scientific: {"integer-simple": true}}}' "$TOP_LEVEL/stack.yaml" > $tmpStackYaml
cp $tmpStackYaml "$TOP_LEVEL/stack.yaml"
