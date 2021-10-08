#!/usr/bin/env bash
set -euo pipefail

cabal-plan list-bins "$1"':test:*' | awk '{print $2}' | xargs -n 1 bash -c
