#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" &>/dev/null && pwd)
ROOT_DIR=$(cd -- "$SCRIPT_DIR/../../" &>/dev/null && pwd)
readonly SCRIPT_DIR ROOT_DIR

name=${1:?"Please provide a migration name"}
nameWithoutSpaces=$(tr '[:blank:]' '-' <<< "$name")
timestamp=$(date --utc '+%Y%m%d%H%M%S')
fileName="$timestamp-$nameWithoutSpaces.sql"
fullPath="$ROOT_DIR/libs/wire-subsystems/postgres-migrations/$fileName"

touch "$fullPath"
echo "Migration created: $fullPath"
