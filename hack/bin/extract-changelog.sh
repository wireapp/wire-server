#!/usr/bin/env bash

TOP_LEVEL="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
CHANGELOG="$TOP_LEVEL/CHANGELOG.md"

USAGE="Example usage: $0 2022-01-28"
RELEASE_DATE="${1:?$USAGE}"

# For a given release date (e.g. 2022-01-28),
# extract the changelog from the previous release to this one.
#
# For details on this awk syntax, see
# https://stackoverflow.com/a/40450360
awk -v ver="[$RELEASE_DATE]" '
 /^# / { if (p) { exit }; if ($2 == ver) { p=1; next } } p && NF
' "$CHANGELOG"
