#!/usr/bin/env bash

# Run all haskell services without immediately starting a test executable.
# Can be useful for manually poking at the API.

# This is NOT intended for a demo with the external API, as nginz is missing here - see the demo.sh script in wire-server/deploy/services-demo for that instead.

set -eo pipefail

SERVICES_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

# call integration.sh, show a message, then sleep (instead of executing a test executable)
"$SERVICES_DIR/integration.sh" bash -c 'printf "(This will hang, Control+C to close.)\nNow you can manually curl them or start an integration test executable manually with e.g. \n(first cd to a service dir for correct working directory)\n  cd services/brig && ../../dist/brig-integration -s brig.integration.yaml -i ../integration.yaml\n" && sleep 1000000'
