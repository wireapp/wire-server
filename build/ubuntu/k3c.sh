#!/usr/bin/env bash

# Not maintained, adding the k3s layer makes it harder to debug.

set -ex
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

k3c build --progress plain -f "${DIR}/Dockerfile.base" .