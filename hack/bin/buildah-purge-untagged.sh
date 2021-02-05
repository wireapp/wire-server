#!/usr/bin/env bash

# Remove untagged images (if there are any) in the buildah store
if buildah images | grep "<none>"; then
    buildah images | grep "<none>" | awk '{print $3}' | xargs -n 1 buildah rmi
fi
