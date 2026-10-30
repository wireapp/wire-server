#!/usr/bin/env bash
set -euo pipefail

DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$DIR/../.."

case "${WIRE_INGRESS_TEST_IMPLEMENTATION:-python}" in
    python)
        python3 -m unittest discover -s hack/bin/tests -v
        exec python3 hack/bin/wire-ingress-test.py
        ;;
    haskell)
        # This component is independent of the application integration library.
        # Run local checks before deploying any cluster resources.
        cabal build integration:exe:ingress-regression integration:test:ingress-unit 2>&1 | grep -vE 'Compiling|Linking|Preprocessing|Configuring|Building'
        cabal test integration:ingress-unit --test-show-details=direct 2>&1 | grep -vE 'Compiling|Linking|Preprocessing|Configuring|Building'
        binary=$(cabal list-bin integration:exe:ingress-regression)
        exec "$binary"
        ;;
    *)
        echo "WIRE_INGRESS_TEST_IMPLEMENTATION must be python or haskell" >&2
        exit 2
        ;;
esac
