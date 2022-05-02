#!/usr/bin/env bash
set -euo pipefail

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
TOP_LEVEL="$(cd "$DIR/../.." && pwd)"

package=${1:-all}

if [[ "$package" == all ]]; then
  pattern='*.cabal'
else
  pattern="$package.cabal"
fi
for cabal in $(find "$TOP_LEVEL" -name "$pattern" | grep -v dist-newstyle); do
  # This is required because some tests (e.g. golden tests) must be run from
  # the package root.
  cd "$(dirname "$cabal")"
  package="$(basename "${cabal%.*}")"
  for test_suite in $(cabal-plan list-bins "$package:test:*" | awk '{print $2}'); do
    $test_suite "${@:2}"
  done
done
