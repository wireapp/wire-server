#!/usr/bin/env bash

# This is a wrapper script to call tools/ormolu1.sh It calls that script for
# both, wire-server and the services/wire-server-enterprise submodule. It should
# be compatible to the old behaviour, with the only addition, that the submodule
# is visited as well.
set -e

cd "$(dirname "${BASH_SOURCE[0]}")"

REL_PATH=services/wire-server-enterprise PR_BASE=origin/main ./ormolu1.sh "$@"
PR_BASE=origin/develop ./ormolu1.sh "$@"
