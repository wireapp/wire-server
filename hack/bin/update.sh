#!/usr/bin/env bash

set -e

USAGE="download and bundle dependent helm charts: $0 <chart-directory>"
dir=${1:?$USAGE}

# remove any global helm repositories to avoid re-querying them for all charts again and again
helm repo list | grep -v NAME | awk '{print $1}' | xargs -n 1 helm repo remove > /dev/null 2>&1 || true

# hacky workaround for helm's lack of recursive dependency update
# See https://github.com/helm/helm/issues/2247
helmDepUp () {
    local path
    path=$1
    cd "$path"
    # remove previous bundled versions of helm charts, if any
    find . -name "*\.tgz" -delete
    if [ -f requirements.yaml ]; then
      echo "Updating dependencies in $path ..."
      # very hacky bash, I'm sorry
      for subpath in $(grep "file://" requirements.yaml | awk '{ print $2 }' | xargs -n 1 | cut -c 8-)
      do
        ( helmDepUp "$subpath" )
      done
      helm dep up
      echo "... updating in $path done."
    fi
}

helmDepUp "$dir"
