#!/usr/bin/env bash

set -euo pipefail

overridesDir="$1"
drv=$(eval "$2")
name=$(echo "$drv" | sed -n 's|.*pname = "\(.*\)";|\1|p')
echo "$drv" > "$overridesDir/$name.nix"
