#!/bin/sh
# GIT_ASKPASS helper: git gets the token of wire-builds from the environment, so it is
# never written to a remote URL or to a file.
printf '%s' "${WIRE_BUILDS_TOKEN:?}"
