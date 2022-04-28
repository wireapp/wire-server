#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

overridesDir=$(cd -- "$SCRIPT_DIR/../../nix/haskell-overrides" &> /dev/null && pwd)
overridesFile="$overridesDir/../haskell-overrides.nix"
pinsFile="$SCRIPT_DIR/../../pins.yaml"

rm -rf "$overridesDir"
rm -f "$overridesFile"
mkdir -p "$overridesDir"

mkDrv() {
    local drv name
    drv=$(eval "$1")
    name=$(echo "$drv" | sed -n 's|.*pname = "\(.*\)";|\1|p')
    echo "$drv" > "$overridesDir/$name.nix"
    echo "  $name = hself.callPackage ./$name.nix {};" >> "$overridesFile"
}

gitCabal2Nix=$(cat <<-'EOF'
.gitPins[] |
  "cabal2nix \(.location) --no-check --no-haddock --revision \(.commit)" as $basecmd |
    if .subdirs == null
    then $basecmd
    else (
      .subdirs[] | "\($basecmd) --subpath \(.)"
    )
    end
EOF
            )

hackageCabal2Nix='.hackagePins[] | "cabal2nix cabal://\(.package)-\(.version) --no-check --no-haddock"'

(yq -r "$gitCabal2Nix" "$pinsFile" && \
     yq -r "$hackageCabal2Nix" "$pinsFile") |  \
    xargs -P 10 -n 1 -I {} "$SCRIPT_DIR/mk-drv.sh" "$overridesDir" {}

echo "hsuper: hself: {" > "$overridesFile"

for f in "$overridesDir"/*.nix; do
    name=$(basename "$f" | sed 's|.nix||')
    echo "  $name = hself.callPackage ./haskell-overrides/$name.nix {};" >> "$overridesFile"
done

echo "}" >> "$overridesFile"
