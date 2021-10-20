#!/usr/bin/env bash

# Helm (v3) writes into XDG folders only these days. They don't honor HELM_ vars
# anymore.
# Derive a helm-specific folder inside the wire-server/.local to avoid polluting
# ~.

DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
TOP_LEVEL="$DIR/../.."
LOCAL_HELM_FOLDER="$TOP_LEVEL/.local/helm"

[[ -e $LOCAL_HELM_FOLDER ]] || mkdir -p "$LOCAL_HELM_FOLDER"
export XDG_CACHE_HOME=${LOCAL_HELM_FOLDER}/cache
export XDG_CONFIG_HOME=${LOCAL_HELM_FOLDER}/config
export XDG_DATA_HOME=${LOCAL_HELM_FOLDER}/data
