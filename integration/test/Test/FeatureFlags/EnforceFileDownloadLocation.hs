module Test.FeatureFlags.EnforceFileDownloadLocation where

import qualified API.GalleyInternal as Internal
import SetupHelpers
import Test.FeatureFlags.Util
import Testlib.Prelude

testPatchEnforceFileDownloadLocation :: (HasCallStack) => App ()
testPatchEnforceFileDownloadLocation = do
  let defCfg =
        object
          [ "lockStatus" .= "unlocked",
            "status" .= "disabled",
            "ttl" .= "unlimited",
            "config"
              .= object
                [ "enforcedDownloadLocation" .= "downloads"
                ]
          ]
  checkPatch OwnDomain "enforceFileDownloadLocation" True defCfg
    $ object ["lockStatus" .= "unlocked"]
  checkPatch OwnDomain "enforceFileDownloadLocation" True defCfg
    $ object ["status" .= "enabled"]
  checkPatch OwnDomain "enforceFileDownloadLocation" True defCfg
    $ object ["lockStatus" .= "unlocked", "status" .= "enabled"]
  checkPatch OwnDomain "enforceFileDownloadLocation" True defCfg
    $ object ["lockStatus" .= "locked", "config" .= object []]
  checkPatch OwnDomain "enforceFileDownloadLocation" True defCfg
    $ object ["config" .= object ["enforcedDownloadLocation" .= "/tmp"]]

  do
    (user, tid, _) <- createTeam OwnDomain 0
    bindResponse
      ( Internal.patchTeamFeature
          user
          tid
          "enforceFileDownloadLocation"
          (object ["config" .= object ["enforcedDownloadLocation" .= ""]])
      )
      $ \resp -> do
        resp.status `shouldMatchInt` 400
        resp.json %. "label" `shouldMatch` "empty-download-location"

testEnforceDownloadLocation :: (HasCallStack) => APIAccess -> App ()
testEnforceDownloadLocation access = do
  mkFeatureTests
    "enforceFileDownloadLocation"
    & addUpdate
      ( object
          [ "status" .= "enabled",
            "config" .= object ["enforcedDownloadLocation" .= "/tmp"]
          ]
      )
    & addUpdate
      (object ["status" .= "disabled", "config" .= object []])
    & addInvalidUpdate
      ( object
          [ "status" .= "enabled",
            "config"
              .= object
                [ "enforcedDownloadLocation" .= object []
                ]
          ]
      )
    & runFeatureTests OwnDomain access
