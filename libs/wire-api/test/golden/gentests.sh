#!/bin/bash

# This script generates a module containing golden tests for JSON instances.

# Note: for this to work, it has to run on a patched checkout of
# wire-server. The patch is contained in `libs/wire-api/test/golden/`
# and can be applied with `git am`.
#
# Remember to undo the patch commit afterwards!

set -e
set -o pipefail

GOLDEN_TMPDIR=$(mktemp -d)
export GOLDEN_TMPDIR
export GOLDEN_TESTDIR="test/unit/Test/Wire/API/Golden/Generated"

# trap cleanup EXIT
function cleanup() {
    [ -z "$GOLDEN_TMPDIR" ] || rm -rf "$GOLDEN_TMPDIR"
}

gen_imports() {
  cat <<EOF
import Codec.MIME.Type (Type(..))
import qualified Codec.MIME.Type as MIME
import Control.Lens ((.~))
import Data.Code
import Data.Coerce
import Data.Currency
import Data.Domain
import Data.Handle
import Data.Id
import Data.ISO3166_CountryCodes
import Data.Json.Util
import Data.List1
import qualified Data.List.NonEmpty as NonEmpty
import Data.List.NonEmpty (NonEmpty (..))
import Data.Misc
import Data.PEM
import Data.Qualified
import Data.Range (unsafeRange)
import qualified Data.Set as Set
import Data.Text.Ascii
import Data.Time (secondsToNominalDiffTime)
import Imports hiding (LT, GT)
import qualified Data.LanguageCodes
import qualified Data.UUID as UUID
import Test.Tasty (testGroup, TestTree)
import URI.ByteString
import qualified Wire.API.Call.Config as CallConfig
import qualified Wire.API.User.Profile as User.Profile
import qualified Wire.API.Team.Conversation as Team.Conversation
import qualified Wire.API.Provider as Provider
import qualified Wire.API.Provider.Bot as Provider
import qualified Wire.API.Provider.External as Provider
import qualified Wire.API.Provider.Service as Provider
import qualified Wire.API.Provider.Service.Tag as Provider
import Data.Aeson
import GHC.Exts
EOF
}

section_team() {
    cat <<EOF
import Data.LegalHold
import Wire.API.Conversation.Role
import Wire.API.Event.Team
import Wire.API.Provider.Service
import Wire.API.Team
import Wire.API.Team.Conversation
import Wire.API.Team.Feature
import Wire.API.Team.Invitation
import Wire.API.Team.LegalHold
import Wire.API.Team.LegalHold.External
import Wire.API.Team.Member
import Wire.API.Team.Permission
import Wire.API.Team.Role
import Wire.API.Team.SearchVisibility
import Wire.API.User.Client.Prekey
import Wire.API.User.Identity
import Wire.API.User.Profile
EOF
}

section_provider() {
    cat <<EOF
import Wire.API.Conversation.Member
import Wire.API.Conversation.Role
import Wire.API.Provider
import Wire.API.Provider.Bot
import Wire.API.Provider.External
import Wire.API.Provider.Service
import Wire.API.Provider.Service.Tag
import Wire.API.User.Client.Prekey
import Wire.API.User.Identity
import Wire.API.User.Profile
EOF
}

section_asset() {
    cat <<EOF
import Wire.API.Asset
EOF
}

section_user() {
    cat <<EOF
import Wire.API.Asset
import Wire.API.Asset.V3.Resumable
import Wire.API.Call.Config
import Wire.API.Connection
import Wire.API.Conversation
import Wire.API.Conversation.Bot
import Wire.API.Conversation.Code
import Wire.API.Conversation.Member
import Wire.API.Conversation.Role
import Wire.API.Conversation.Typing
import Wire.API.CustomBackend
import Wire.API.Event.Conversation
import Wire.API.Message
import Wire.API.Notification (QueuedNotification, queuedNotification, QueuedNotificationList, queuedNotificationList)
import Wire.API.Properties
import Wire.API.Provider.Bot
import Wire.API.Provider.External
import Wire.API.Provider.Service
import Wire.API.Push.Token hiding (Transport)
import qualified Wire.API.Push.Token as Push.Token
import Wire.API.Team
import Wire.API.Team.Role
import Wire.API.User
import Wire.API.User.Activation
import Wire.API.User.Auth
import Wire.API.User.Client
import Wire.API.User.Client.Prekey
import Wire.API.User.Handle
import Wire.API.User.Identity
import Wire.API.User.Password
import Wire.API.User.Profile
import Wire.API.User.RichInfo
import Wire.API.User.Search
import Wire.API.Wrapped
EOF
}

rm -fr "$GOLDEN_TESTDIR"
rm -fr "$GOLDEN_TESTDIR.hs"

mkdir -p "$GOLDEN_TESTDIR"
mkdir -p "$GOLDEN_TMPDIR/dump"

stack build --fast --test --bench --no-run-benchmarks wire-api |
    while read -r module section; do
        echo -ne "\033[KProcessing module $module...\r"
        {
            echo "{-# OPTIONS_GHC -Wno-unused-imports #-}"
            echo "{-# OPTIONS_GHC -ddump-minimal-imports #-}"
            echo "{-# OPTIONS_GHC -dumpdir $GOLDEN_TMPDIR/dump #-}"
            echo "{-# LANGUAGE OverloadedLists #-}"
            echo ""
            echo "module Test.Wire.API.Golden.Generated.$module where"
            echo ""
            gen_imports
            fun="section_$section"
            "$fun"
            sed -r -e 's/([0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12})/(Id (fromJust (UUID.fromString "\1")))/g' \
                -e 's/:: Scheme/:: CallConfig.Scheme/' \
                -e 's/Id \* C/ConvId/g' \
                -e 's/mimeType = ([^ ]*)/mimeType = MIME.\1/g' \
                -e 's/newConvAccess = fromList/newConvAccess = Set.fromList/g' \
                -e 's/Number ([-0-9.e]+)/Number (\1)/g' \
                -e 's/URI \{/coerce URI \{/g' \
                -e 's/BotId = \(Id/BotId = ((BotId . Id)/g' \
                -e 's/UserUUID = Just \(Id/UserUUID = (Just . toUUID) (Id/g' \
                < "$GOLDEN_TMPDIR/$module.hs"
                    } > "$GOLDEN_TESTDIR/$module.hs"
    done

echo

# build again
stack build --fast --test --bench --no-run-benchmarks --no-run-tests wire-api

readarray -t EXTS < <(sed -rn '/^default-extensions:/,$ { s/^- (.*)/\1/ }' ../../package-defaults.yaml)

# fix imports
for module in "$GOLDEN_TESTDIR"/*; do
    name="Test.Wire.API.Golden.Generated.$(basename "$module")"
    dump="$GOLDEN_TMPDIR/dump/${name%.hs}.imports"
    sed -r -i '/\(\)$/d' "$dump" # remove empty imports
    sed -r -i \
      -e '/dump-minimal-imports/d' \
      -e '/dumpdir/d' \
      -e '/no-unused-imports/d' \
      -e '/^import/d' \
      -e "/^module/ r $dump" \
      "$module"
    ormolu -m inplace -c "${EXTS[@]/#/'-o '}" "$module"
done

ormolu -m inplace -c "${EXTS[@]/#/'-o '}" "$GOLDEN_TESTDIR.hs"
( cd ../.. && headroom run -a -s libs/wire-api/test/unit/Test/Wire/API/Golden/ )

# build one final time
stack build --fast --test --bench --no-run-benchmarks --no-run-tests wire-api
