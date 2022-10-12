#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
ROOT_DIR=$(cd -- "$SCRIPT_DIR/../../" &> /dev/null && pwd)
cabalFiles=$(find "$ROOT_DIR" -name '*.cabal' \
                 | grep -v dist-newstyle | sort)

warningFile=$(mktemp)
cat >"$warningFile" <<EOF
# WARNING: GENERATED FILE, DO NOT EDIT.
# This file is generated by running hack/bin/generate-local-nix-packages.sh and
# must be regenerated whenever local packages are added or removed, or
# dependencies are added or removed.
EOF

# shellcheck disable=SC2016
echo "$cabalFiles" \
    | xargs -I {} bash -c 'cd $(dirname {}); cat $0 > default.nix' "$warningFile"

# shellcheck disable=SC2016
echo "$cabalFiles" \
    | xargs -I {} bash -c 'cd $(dirname {}); cabal2nix . --no-hpack --extra-arguments gitignoreSource | sed "s/.\/./gitignoreSource .\/./g" >> default.nix'

overridesFile="$ROOT_DIR/nix/local-haskell-packages.nix"

echo "{ gitignoreSource }: hsuper: hself: {" > "$overridesFile"
# shellcheck disable=SC2016
echo "$cabalFiles" \
    | xargs -I {} bash -c 'name=$(basename {} | sed "s|.cabal||"); echo "  $name = hself.callPackage $(realpath --relative-to='"$ROOT_DIR/nix"' "$(dirname {})")/default.nix { inherit gitignoreSource; };"' >> "$overridesFile"
echo "}" >> "$overridesFile"
