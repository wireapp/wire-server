#!/usr/bin/env bash

# This script checks the tags on quay.io for the 'brig' docker image and returns the highest version.
# Note that this may be brittle: while CI uses the same version for brig as well as galley and other docker images it uploads,
# they are not uploaded simulaneously, so this script is subject to race conditions and CI failures.
# Use at your own risk!

curl -sSL 'https://quay.io/api/v1/repository/wire/brig/tag/?limit=50&page=1&onlyActiveTags=true' \
    | jq -r '.tags[].name' \
    | sort | uniq | grep -v latest | grep -v 'pr\.' | tail -1
