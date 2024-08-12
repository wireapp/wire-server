#!/usr/bin/env bash

LIB_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
TOP_LEVEL="$(cd "$LIB_DIR/../.." && pwd)"
BIN_DIR="${TOP_LEVEL}/hack/bin"
CHARTS_DIR="${TOP_LEVEL}/.local/charts"

export LIB_DIR TOP_LEVEL BIN_DIR CHARTS_DIR
