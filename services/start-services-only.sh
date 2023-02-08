#!/usr/bin/env bash

# Run all haskell services without immediately starting a test executable.
# Can be useful for manually poking at the API.
set -eo pipefail

SERVICES_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

# call run-services, show a message, then sleep (instead of executing a test executable)
exec "$SERVICES_DIR/run-services"
