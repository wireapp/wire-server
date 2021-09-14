#!/usr/bin/env bash

set -euo pipefail
shopt -s nullglob

DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

rm -f "$DIR"/*/*
git add "$DIR"
