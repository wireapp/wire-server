-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2025 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any
-- later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

module Test.FeatureFlags.AssetAuditLog where

import qualified API.Galley as Public
import qualified API.GalleyInternal as Internal
import SetupHelpers
import Test.FeatureFlags.Util
import Testlib.Prelude

testAssetAuditLog :: (HasCallStack) => TaggedBool "isSet" -> App ()
testAssetAuditLog (TaggedBool isSet) = do
  let setting = object ["status" .= "enabled"]
      defSetting = object ["status" .= "disabled"]

  let galleyConf =
        if isSet
          then def {galleyCfg = setField "settings.featureFlags.assetAuditLog" $ setting}
          else def {galleyCfg = removeField "settings.featureFlags.assetAuditLog"}

  withModifiedBackend galleyConf $ \domain -> do
    (admin, tid, _) <- createTeam domain 0
    expected <-
      if isSet
        then setting & setField "lockStatus" "locked" & setField "ttl" "unlimited"
        else defSetting & setField "lockStatus" "locked" & setField "ttl" "unlimited"

    checkFeature "assetAuditLog" admin tid expected

    -- feature is immutable (no PUT/PATCH available)
    Public.setTeamFeatureConfig admin tid "assetAuditLog" (object ["status" .= "enabled"]) `bindResponse` \resp ->
      resp.status `shouldMatchInt` 404

    req <- baseRequest admin Galley Unversioned $ joinHttpPath ["i", "teams", tid, "features", "assetAuditLog", "unlocked"]
    bindResponse (submit "PUT" $ req) $ \resp ->
      resp.status `shouldMatchInt` 404

    Internal.setTeamFeatureStatus admin tid "assetAuditLog" "enabled" `bindResponse` \resp ->
      resp.status `shouldMatchInt` 404

    -- check with personal user
    personalUser <- randomUser domain def
    bindResponse (Public.getFeatureConfigs personalUser) $ \resp -> do
      resp.status `shouldMatchInt` 200
      resp.json %. "assetAuditLog" `shouldMatch` expected
