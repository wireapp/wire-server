#!/usr/bin/env bash

# Run all haskell services without immediately starting a test executable.
# Can be useful for manually poking at the API.

set -eo pipefail

SERVICES_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

# call run-services, show a message, then sleep (instead of executing a test executable)
"$SERVICES_DIR/run-services" bash -c 'printf "(This will hang, Control+C to close.)\nNow you can manually curl them or start an integration test executable manually with e.g. \n(first cd to a service dir for correct working directory)\n  cd services/brig && ../../dist/brig-integration -s brig.integration.yaml -i ../integration.yaml\n" && sleep 1000000'
