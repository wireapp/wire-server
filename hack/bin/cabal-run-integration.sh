#!/usr/bin/env bash
set -euo pipefail
#
# this script runs the integration test suite for the given service, after it
# has started all services integration test configuration.
#
# Usage:
#
# ./cabal-run-integration.sh brig
# ./cabal-run-integration.sh brig -p '$2 == "provider" && $3 == "account"'
#
# Any additional arguments are passed as-is to the integration test executable.
# brig uses tasty, so you specify patterns with -p. The above example runs all
# tests in the test tree
#
# Brig API Integration
#   provider
#     account
#
# Federator uses hspec, where you specify patterns like so:
#
# ./cabal-run-integration.sh federator -m rejectRequestsWithoutClientCertIngress
#
# To run all integration tests without arguments run:
#
# ./cabal-run-integration.sh all
#
# If you're not sure what test suite is being used call for help
# ./cabal-run-integration.sh spar --help

set -eo pipefail

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
TOP_LEVEL="$(cd "$DIR/../.." && pwd)"

package=${1:-all}

run_integration_tests() {
  package=${1}

  if [[ "$package" = "gundeck" ]]
  then if [[ -z "${TASTY_PATTERN-}" ]]
       then export TASTY_PATTERN="!/RealAWS/"
       else export TASTY_PATTERN="!/RealAWS/ && $TASTY_PATTERN"
       fi
  fi

  if [[ "$package" = "integration" ]]
  then
    cd "$TOP_LEVEL"
    "$TOP_LEVEL/dist/run-services" \
        -- \
        "$TOP_LEVEL/dist/integration" \
        "${@:2}"
  else
    service_dir="$TOP_LEVEL/services/$package"

    cd "$service_dir"
    "$TOP_LEVEL/dist/run-services" \
      -- \
      "$TOP_LEVEL/dist/$package-integration" \
      -s "$service_dir/$package.integration.yaml" \
      -i "$TOP_LEVEL/services/integration.yaml" \
      "${@:2}"
  fi
}

run_all_integration_tests() {
  for d in "$TOP_LEVEL/services/"*/; do
    package=$(basename "$d")
    service_dir="$TOP_LEVEL/services/$package"
    if [ -d "$service_dir/test/integration" ] || [ -d "$service_dir/test-integration" ]; then
      run_integration_tests "$package"
    fi
  done
  run_integration_tests "stern"
}

if [ "$package" == "all" ]; then
  if [ -n "${2:-}" ]; then
    echo -e "\e[31mCannot pass additional args to all integrations tests.\e[0m" >&2
    exit 1
  fi
  run_all_integration_tests
else
  run_integration_tests "$package" "${@:2}"
fi
