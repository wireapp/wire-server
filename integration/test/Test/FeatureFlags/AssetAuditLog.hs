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
