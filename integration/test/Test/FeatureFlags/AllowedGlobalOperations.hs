module Test.FeatureFlags.AllowedGlobalOperations where

import qualified API.Galley as Public
import qualified API.GalleyInternal as Internal
import SetupHelpers
import Test.FeatureFlags.Util
import Testlib.Prelude

testAllowedGlobalOperations :: (HasCallStack) => TaggedBool "isSet" -> App ()
testAllowedGlobalOperations (TaggedBool isSet) = do
  let setting =
        object
          [ "status" .= "disabled",
            "config"
              .= object
                ["mlsConversationReset" .= True]
          ]
      defSetting =
        object
          [ "status" .= "enabled",
            "config"
              .= object
                [ "mlsConversationReset" .= False
                ]
          ]
  let galleyConf =
        if isSet
          then def {galleyCfg = setField "settings.featureFlags.allowedGlobalOperations" $ setting}
          else def {galleyCfg = removeField "settings.featureFlags.allowedGlobalOperations"}
  withModifiedBackend galleyConf
    $ \domain -> do
      (admin, tid, _) <- createTeam domain 0
      expected <-
        if isSet
          then setting & setField "lockStatus" "locked" & setField "ttl" "unlimited"
          else defSetting & setField "lockStatus" "locked" & setField "ttl" "unlimited"
      checkFeature "allowedGlobalOperations" admin tid expected

      -- feature is immutable
      Public.setTeamFeatureConfig admin tid "allowedGlobalOperations" (object ["status" .= "enabled"])
        `bindResponse` \resp -> do
          resp.status `shouldMatchInt` 404

      req <- baseRequest admin Galley Unversioned $ joinHttpPath ["i", "teams", tid, "features", "allowedGlobalOperations", "unlocked"]
      bindResponse (submit "PUT" $ req) $ \resp ->
        resp.status `shouldMatchInt` 404

      Internal.setTeamFeatureStatus admin tid "allowedGlobalOperations" "enabled"
        `bindResponse` \resp -> do
          resp.status `shouldMatchInt` 404

      -- check with personal user
      personalUser <- randomUser domain def
      bindResponse (Public.getFeatureConfigs personalUser) $ \resp -> do
        resp.status `shouldMatchInt` 200
        resp.json %. "allowedGlobalOperations" `shouldMatch` expected
