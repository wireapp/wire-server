#!/bin/bash

# This script generates a module containing golden tests for JSON instances.

# Note: for this to work, it has to run on a patched checkout of
# wire-server. The patch is contained in `libs/wire-api/test/golden/`
# and can be applied it with `git am`.
#
# Remember to undo the patch commit afterwards!

set -e

rm -f test/unit/Test/Wire/API/Golden/Generated.hs

tmp=$(mktemp)

( set -e
  cat <<EOF
module Test.Wire.API.Golden.Generated where

import Imports
import qualified Data.UUID as UUID
import Wire.API.User
import Wire.API.Conversation
import Wire.API.Conversation.Role
import Data.Qualified
import Data.Domain
import Wire.API.Provider.Service
import Data.Handle
import Data.Id
import Test.Tasty
import Data.Json.Util
import Data.ISO3166_CountryCodes
import qualified Data.LanguageCodes

import Test.Wire.API.Golden.Runner

EOF
  stack build --fast --test --bench --no-run-benchmarks wire-api
  ) > "$tmp"

# replace UUID with correct haskell code
sed -r 's/ ([0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12})/ (Id (fromJust (UUID.fromString "\1")))/g' "$tmp" > \
    test/unit/Test/Wire/API/Golden/Generated.hs
