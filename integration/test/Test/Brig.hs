module Test.Brig where

import qualified API.Brig as Public
import qualified API.BrigInternal as Internal
import qualified API.Common as API
import qualified API.GalleyInternal as Internal
import GHC.Stack
import SetupHelpers
import Testlib.Prelude

testSearchContactForExternalUsers :: HasCallStack => App ()
testSearchContactForExternalUsers = do
  owner <- randomUser def {Internal.team = True}
  partner <- randomUser def {Internal.team = True}

  bindResponse (Internal.putTeamMember partner (partner %. "team") (API.teamRole "partner")) $ \resp ->
    resp.status `shouldMatchInt` 200

  bindResponse (Public.searchContacts partner (owner %. "name")) $ \resp ->
    resp.status `shouldMatchInt` 403
